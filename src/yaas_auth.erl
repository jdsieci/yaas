%% -*- coding: utf-8 -*-
%%% -------------------------------------------------------------------
%%% Author  : tofik
%%% Description :
%%%
%%% Created : 01-03-2014
%%% -------------------------------------------------------------------

-module(yaas_auth).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%poolboy_worker callbacks
-export([start_link/1]).

-include("yaas_realm.hrl").
-include("yaas_user.hrl").
-include("yaas_group.hrl").
-include("yaas_auth.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([check/2, add/2, update/2, delete/1]).

-spec check(User::yaas:user(), Password::string()) -> Result when
          Result :: ok | error.
%% @doc Checks credentials (login process)
check({UserName, Realm}, Password) ->
    poolboy:transaction(?MODULE, fun(Worker) ->
                                      gen_server:call(Worker, #check{user = #user{username = UserName, realm = Realm}, password = Password})
                        end).

-spec add(User::yaas:user(), list()) -> Result when
          Result :: ok | error.
%% @doc Adds user
add({UserName, Realm}, []) ->
    poolboy:transaction(?MODULE, fun(Worker) ->
                                      gen_server:call(Worker, #add{user = #user{username = UserName, realm = Realm}})
                        end);
add({UserName, Realm}, Props) when is_list(Props) ->
    poolboy:transaction(?MODULE, fun(Worker) ->
                                      gen_server:call(Worker, #add{user = #user{username = UserName, realm = Realm}, props = Props})
                        end).

-spec update(User::yaas:user(), list()) -> Result when
          Result :: ok | error.
%% @doc Update users informations
update({UserName, Realm}, Props) when is_list(Props) ->
    poolboy:transaction(?MODULE, fun(Worker) ->
                                      gen_server:call(Worker, #update{user = #user{username = UserName, realm = Realm}, props = Props})
                        end).

-spec delete(User::yaas:user()) -> Result when
          Result :: ok | error.
%% @doc Removes user
delete({UserName, Realm}) ->
    poolboy:transaction(?MODULE, fun(Worker) ->
                                         gen_server:call(Worker, #delete{username = UserName, realm = Realm})
                        end).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

%% start_link/1
-spec start_link(Args::term()) -> Result when
          Result :: {ok, pid()}.

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(#check{user = #user{username = UserName, realm = Realm}, password = Password}, _From, State) ->
    #yaas_realm{id = RealmId} = boss_db:find_first(yaas_realm, [{realm,'equals', Realm}]),
    #yaas_user{password = UserPassword} = boss_db:find_first(yaas_user, [{user_name, 'equals', UserName},
                                                                         {realm_id, 'equals', RealmId}]),
    case {ok, UserPassword} =:= bcrypt:hashpw(Password, UserPassword) of
        true ->
            lager:info("User ~p@~p authenticated", [UserName, Realm]),
            {reply, ok, State};
        false ->
            lager:info("Authentication failed ~p@~p", [UserName, Realm]),
            {reply, error, State}
    end;
handle_call(#add{user = #user{username = UserName, realm = Realm}, props = Props}, _From, State) ->
    #yaas_realm{id = RealmId} = boss_db:find_first(yaas_realm, [{realm,'equals', Realm}]),
    case boss_db:find_first(yaas_group, [{group, 'equals', proplists:get_value(group, Props)},
                                         {realm_id, 'equals', RealmId}
                                        ]) of
        undefined ->
            lager:error("Group does not exists"),
            {reply, {error, "Group does not exists"}, State};
        Group ->
            case boss_db:transaction(fun() ->
                                             User = yaas_user:new(id, UserName, proplists:get_value(password, Props), RealmId, Group:id()),
                                             {ok, SavedUser} = User:save(),
                                             NewGroup = Group:set(users, [SavedUser:id() | Group:users()]),
                                             {ok, _} = NewGroup:save(),
                                             proplists:delete(password, Props),
                                             update_props(SavedUser, Props),
                                             {ok, added}
                                     end) of
                {atomic, _Result} ->
                    lager:info("User ~p@~p added", [UserName, Realm]),
                    {reply, ok, State};
                {aborted, Reason}  ->
                    lager:warning("Failed adding user ~p@~p errmgs: ~p", [UserName, Realm, Reason]),
                    {reply, error, State}
            end
    end;
handle_call(#update{user = #user{username = UserName, realm = RealmName}, props = Props}, _From, State) ->
    Realm = boss_db:find_first(yaas_realm, [{realm,'equals', RealmName}]),
    case boss_db:find(yaas_user, [{user_name, 'equals', UserName},
                                  {realm_id, 'equals', Realm:id()}
                                 ]) of
        [User] ->
            lager:debug("User= ~p", [User]),
            case boss_db:transaction(fun() -> update_props(User, Props),
                                              {ok, updated}
                                     end) of
                {atomic, Result} ->
                    lager:info("User ~p@~p updated: ~p", [UserName, RealmName, Result]),
                    {reply, ok, State};
                {aborted, Reason}  ->
                    lager:warning("Failed updating user ~p@~p errmgs: ~p", [UserName, RealmName, Reason]),
                    {reply, error, State}
            end;
        [] ->
            lager:error("User does not exist"),
            {reply, {error, "User does not exist"}, State}
    end;

handle_call(#delete{username = UserName, realm = RealmName}, _From, State) ->
    lager:debug("yaas_auth:handle_call/3 action=delete"),
    Realm = boss_db:find_first(yaas_realm, [{realm,'equals', RealmName}]),
    User = boss_db:find_first(yaas_user, [{user_name, 'equals', UserName},
                                          {realm_id, 'equals', Realm:id()}]),
    lager:debug("User=~p; User:id() = ~p",[User, User:id()]),
    Groups = boss_db:find(yaas_group, [{users, 'contains', User:id()},
                                       {realm_id, 'equals', Realm:id()}
                                      ]),
    lager:debug("Groups=~p",[Groups]),
    case boss_db:transaction(fun() ->
                                     ok = boss_db:delete(User:id()),
                                     group_delete(User, Groups),
                                     {ok, deleted}
                             end) of
        {atomic, Result} ->
            lager:info("User ~p@~p deleted", [UserName, RealmName]),
            {reply, Result, State};
        {aborted, Reason} ->
            lager:warning("Failed deleting user ~p@~p reason: ~p", [UserName, RealmName, Reason]),
            {reply, error, State}
    end.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(#delete{username = UserName, realm = Realm}, State) ->
    #yaas_realm{id = RealmId} = boss_db:find(yaas_realm, [{realm,'equals', Realm}]),
    #yaas_user{id = UserId} = boss_db:find(yaas_user, [{user_name, 'equals', UserName},
                                                       {realm, 'equals', RealmId}
                                                      ]),
    boss_db:delete(UserId),
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(shutdown, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

-spec update_props(User::yaas:boss_record(), Props::yaas:properties()) -> Result when
          Result :: {ok, list()}.

update_props(User, Props) ->
    update_props(User, Props, {ok, []}).


-spec update_props(User::yaas:boss_record(), Props::yaas:properties(), State) -> Result when
          Result :: {ok, list()},
          State :: {ok, list()}.

update_props(User, [], State) ->
    lager:debug("In update_props/3 User=~p; end", [User]),
    {ok, _} = User:save(),
    State;
update_props(User, [{password, NewPassword} | Props], {ok, State}) ->
    NewUser = User:set(password, NewPassword),
    update_props(NewUser, Props, {ok, [password | State]});
update_props(User, [{group, NewGroupName} | Props], {ok, State}) ->
    lager:debug("In update_props/3 prop=~p", [{group, NewGroupName}]),
    case boss_db:find(yaas_group, [{group, 'equals', NewGroupName},
                                   {realm_id, 'equals', User:realm_id()},
                                   {id, 'not_equals', User:main_group_id()}
                                  ]) of
        [Group] ->
            lager:debug("Group= ~p", [Group]),
            NewUser = User:set(main_group_id, Group:id()),
            lager:debug("NewUser= ~p", [NewUser]),
            lager:debug("User= ~p", [User]),
            {ok, _} = group_add(User, Group),
            lager:debug("OldGroup= ~p", [boss_db:find(User:main_group_id())]),
            {ok, _} = group_delete(User, boss_db:find(User:main_group_id())),
            update_props(NewUser, Props, {ok, [group | State]});
        [] ->
            update_props(User, Props, {ok, State})
    end;
update_props(User, [{groups, Groups} | Props], {ok, State}) ->
    lager:debug("In update_props/3 prop=~p", [{groups, Groups}]),
    case update_groups(User, Groups) of
        {ok, _} ->
            update_props(User, Props, {ok, [groups | State]});
        Error ->
            throw(Error)
    end;
update_props(User, [{add_groups, Groups} | Props], {ok, State}) ->
    lager:debug("In update_props/3 prop=~p", [{add_groups, Groups}]),
    case add_groups(User, Groups) of
        {ok, _} ->
            update_props(User, Props, {ok, [ add_groups | State]});
        Error ->
            throw(Error)
    end;
update_props(User, [{delete_groups, Groups} | Props], {ok, State}) ->
    lager:debug("In update_props/3 prop=~p", [{delete_groups, Groups}]),
    case delete_groups(User, Groups) of
        {ok, _} ->
            update_props(User, Props, {ok, [delete_groups | State]});
        Error ->
            throw(Error)
    end;
update_props(_User, [_Unknown | _Props], _) ->
    throw("Unknown property").


-spec add_groups(User::yaas:boss_record(), GroupNames::list()) -> Result when
          Result :: {ok, string()}.

add_groups(User, GroupNames) ->
    group_add(User, boss_db:find(yaas_group, [{group, 'in', GroupNames},
                                              {users, 'not_contains', User:id()},
                                              {realm_id, 'equals', User:realm_id()}
                                             ])).


-spec delete_groups(User::yaas:boss_record(), GroupNames::[string()]) -> Result when
          Result :: {ok, string()}.

delete_groups(User, GroupNames) ->
    group_delete(User, boss_db:find(yaas_group, [{group, 'in', GroupNames},
                                                 {users, 'contains', User:id()},
                                                 {realm_id, 'equals', User:realm_id()}
                                                ])).


-spec update_groups(User::yaas:boss_record(), GroupNames::[string()]) -> Result when
          Result :: {ok, string()}.

update_groups(User, GroupNames) ->
    lager:debug("In update_groups/2 params: User= ~p, GroupNames=~p", [User, GroupNames]),
    group_delete(User, boss_db:find(yaas_group, [{group, 'not_in', GroupNames},
                                                 {users, 'contains', User:id()},
                                                 {realm_id, 'equals', User:realm_id()},
                                                 {id, 'not_equals', User:main_group_id()}
                                                ])),
    group_add(User, boss_db:find(yaas_group, [{group, 'in', GroupNames},
                                              {users, 'not_contains', User:id()},
                                              {realm_id, 'equals', User:realm_id()},
                                              {id, 'not_equals', User:main_group_id()}
                                             ])).


-spec group_add(User::yaas:boss_record(), Groups) -> Result when
          Result :: {ok, string()} | {ok, yaas:boss_record()} | {error, Reason::term()},
          Groups :: yaas:boss_record() | [yaas:boss_record()].

group_add(User, []) ->
    lager:debug("In group_add/2 params: User= ~p, end", [User]),
    {ok, "Groups added"};
group_add(User, [Group | Groups]) ->
    lager:debug("In group_add/2 params: User= ~p, Groups=~p", [User, [Group | Groups]]),
    case group_add(User, Group) of
        {ok, _} ->
            group_add(User, Groups);
        Error ->
            Error
    end;
group_add(User, Group) ->
    lager:debug("In group_add/2 single group process params: User= ~p, Group=~p", [User, Group]),
    case lists:member(User:id(), Group:users()) of
        false ->
            NewGroup = Group:set(users, [User:id() | Group:users()]),
            NewGroup:save();
        true ->
            {ok, Group}
    end.


-spec group_delete(User::yaas:boss_record(), Groups) -> Result when
          Result :: {ok, string()} | {ok, yaas:boss_record()} | {error, Reason::term()},
          Groups :: yaas:boss_record() | [yaas:boss_record()].

group_delete(User, []) ->
    lager:debug("In group_delete/2 params: User= ~p, end", [User]),
    {ok, "Groups deleted"};
group_delete(User, [Group | Groups]) ->
    lager:debug("In group_delete/2 params: User= ~p, Groups=~p", [User, [Group | Groups]]),
    case group_delete(User, Group) of
        {ok, _} ->
            group_delete(User, Groups);
        Error ->
            Error
    end;
group_delete(User, Group) ->
    lager:debug("In group_delete/2 single group process params: User= ~p, Group=~p", [User, Group]),
    NewGroup = Group:set(users, lists:delete(User:id(), Group:users())),
    NewGroup:save().