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

-type user()::{string(), string()}.

%% ====================================================================
%% API functions
%% ====================================================================
-export([check/2, add/2, update/2, delete/1]).


-spec check(User::user(), Password::string()) -> Result when
          Result :: ok | error.
check({UserName, Realm}, Password) ->
    poolboy:transaction(?MODULE, fun(Worker) ->
                                      gen_server:call(Worker, #check{user = #user{username = UserName, realm = Realm}, password = Password})
                        end).

-spec add(User::user(), list()) -> Result when
          Result :: ok | error.
add({UserName, Realm}, []) ->
    poolboy:transaction(?MODULE, fun(Worker) ->
                                      gen_server:call(Worker, #add{user = #user{username = UserName, realm = Realm}})
                        end);
add({UserName, Realm}, Props) when is_list(Props) ->
    poolboy:transaction(?MODULE, fun(Worker) ->
                                      gen_server:call(Worker, #add{user = #user{username = UserName, realm = Realm}, props = Props})
                        end).

-spec update(User::user(), list()) -> Result when
          Result :: ok | error.
update({UserName, Realm}, Props) when is_list(Props) ->
    poolboy:transaction(?MODULE, fun(Worker) ->
                                      gen_server:call(Worker, #update{user = #user{username = UserName, realm = Realm}, props = Props})
                        end).

-spec delete(User::user()) -> Result when
          Result :: ok | error.
delete({UserName, Realm}) ->
    poolboy:transaction(?MODULE, fun(Worker) ->
                                         gen_server:cast(Worker, #delete{username = UserName, realm = Realm})
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
        #yaas_group{id = GroupId} ->
            case boss_db:transaction(fun() ->
                                             User = yaas_user:new(id, UserName, proplists:get_value(password, Props), RealmId, GroupId),
                                             {ok, SavedUser} = User:save(),
                                             proplists:delete(password, Props),
                                             update_props(SavedUser, Props)
                                     end) of
                {atomic, _Result} ->
                    lager:info("User ~p@~p added", [UserName, Realm]),
                    {reply, ok, State};
                {aborted, Reason}  ->
                    lager:warning("Failed adding user ~p@~p errmgs: ~p", [UserName, Realm, Reason]),
                    {reply, error, State}
            end;
%%             User = yaas_user:new(id, UserName, proplists:get_value(password, Props), RealmId, GroupId),
%%             proplists:delete(password, Props),
%%             case User:save() of
%%                 {ok, SavedUser} ->
%%                     case update_props(SavedUser, Props) of
%%                         ok ->
%%                             lager:info("User ~p@~p added", [UserName, Realm]),
%%                             {reply, ok, State};
%%                         _ ->
%%                             {reply, error, State}
%%                     end;
%%                 {error, ErrMsg} ->
%%                     lager:warning("Failed adding user ~p@~p errmgs: ~p", [UserName, Realm, ErrMsg]),
%%                     {reply, error, State}
%%             end;
        undefined ->
            lager:error("Group does not exists"),
            {reply, {error, "Group does not exists"}, State}
    end;
handle_call(#update{user = #user{username = UserName, realm = RealmName}, props = Props}, _From, State) ->
    Realm = boss_db:find_first(yaas_realm, [{realm,'equals', RealmName}]),
    case boss_db:find(yaas_user, [{user_name, 'equals', UserName},
                                  {realm_id, 'equals', Realm:id()}
                                 ]) of
        [User] ->
            case boss_db:transaction(fun() -> update_props(User, Props) end) of
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

handle_call(#delete{username = UserName, realm = Realm}, _From, State) ->
    #yaas_realm{id = RealmId} = boss_db:find_first(yaas_realm, [{realm,'equals', Realm}]),
    #yaas_user{id = UserId} = boss_db:find_first(yaas_user, [{user_name, 'equals', UserName},
                                                             {realm, 'equals', RealmId}]),
    {reply, boss_db:delete(UserId), State}.


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
    {ok, #yaas_realm{id = RealmId}} = boss_db:find(yaas_realm, [{realm,'equals', Realm}]),
    {ok, #yaas_user{id = UserId}} = boss_db:find(yaas_user, [{user_name, 'equals', UserName},
                                                                         {realm, 'equals', RealmId}]),
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

update_props(User, Props) ->
    update_props(User, Props, {ok, []}).


update_props(User, [], State) ->
    User:save(),
    State;
update_props(User, [{password, NewPassword} | Props], {ok, State}) ->
    NewUser = User:set(password, NewPassword),
    update_props(NewUser, Props, {ok, [password | State]});
update_props(User, [{group, NewGroupName} | Props], {ok, State}) ->
    case boss_db:find(yaas_group, [{group, 'equals', NewGroupName},
                                   {users, 'not_contains', User:user_name()},
                                   {realm_id, 'equals', User:realm_id()},
                                   {id, 'not_equals', User:main_group_id()}
                                  ]) of
        [Group] ->
            NewUser = User:set(main_group_id, Group:id()),
            update_props(NewUser, Props, {ok, [group | State]});
        [] ->
            update_props(User, Props, {ok, State})
    end;
update_props(User, [{groups, Groups} | Props], {ok, State}) ->
    case update_groups(User:id(), User:realm_id(), Groups) of
        ok ->
            update_props(User, Props, {ok, [groups | State]});
        error ->
            error
    end;
update_props(User, [{add_groups, Groups} | Props], {ok, State}) ->
    case add_groups(User:id(), User:realm_id(), Groups) of
        ok ->
            update_props(User, Props, {ok, [ add_groups | State]});
        error ->
            error
    end;
update_props(User, [{delete_groups, Groups} | Props], {ok, State}) ->
    case delete_groups(User:id(), User:realm_id(), Groups) of
        ok ->
            update_props(User, Props, {ok, [delete_groups | State]});
        error ->
            error
    end;
update_props(_User, [_Unknown | _Props], _) ->
    throw("Unknown property").


add_groups(UserId, RealmId, GroupNames) ->
    Groups = boss_db:find(yaas_group, [{group, 'in', GroupNames},
                                       {users, 'not_contains', UserId},
                                       {realm_id, 'equals', RealmId}
                                      ]),
    add_groups(UserId, Groups).

delete_groups(UserId, RealmId, GroupNames) ->
    Groups = boss_db:find(yaas_group, [{group, 'in', GroupNames},
                                       {users, 'contains', UserId},
                                       {realm_id, 'equals', RealmId}
                                      ]),
    delete_groups(UserId, Groups).

update_groups(UserId, RealmId, GroupNames) ->
    delete_groups(UserId, boss_db:find(yaas_group, [{group, 'not_in', GroupNames},
                                                    {users, 'contains', UserId},
                                                    {realm_id, 'equals', RealmId}
                                                   ])),
    add_groups(UserId, boss_db:find(yaas_group, [{group, 'in', GroupNames},
                                                 {users, 'not_contains', UserId},
                                                 {realm_id, 'equals', RealmId}
                                                ])).


add_groups(_, []) ->
    ok;
add_groups(UserId, [ Group | T ]) ->
    NewGroup = Group:set(users, [UserId | Group:users()]),
    case NewGroup:save() of
        {ok, _} ->
            add_groups(UserId, T);
        Error ->
            Error
    end.

delete_groups(_, []) ->
    ok;
delete_groups(UserId, [Group | T]) ->
    NewGroup = Group:set(users, lists:delete(UserId, Group:users())),
    case NewGroup:save() of
        {ok, _} ->
            delete_groups(UserId, T);
        Error ->
            Error
    end.
