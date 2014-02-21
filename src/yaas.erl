%% @author tofik
%% @doc @todo Add description to yaas.


-module(yaas).

-include("yaas_realm.hrl").
-include("yaas_user.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).

%Checks
-export([check_auth/2, check_authz/2]).

%Adds
-export([add_auth/2, add_authz/2]).

%Dels
-export([delete_auth/1, delete_authz/2]).

%Updates
-export([update_auth/2, update_authz/2]).

start() ->
    application:start(yaas).

stop() ->
    application:stop(stop).

check_auth({UserName, Realm}, Password) ->
    {ok, #yaas_realm{id = RealmId}} = boss_db:find(yaas_realm, [{realm,'equals', Realm}]),
    {ok, #yaas_user{password = UserPassword}} = boss_db:find(yaas_user, [{user_name, 'equals', UserName},
                                                                         {realm, 'equals', RealmId}]),
    case {ok, UserPassword} =:= bcrypt:hashpw(Password, UserPassword) of
        true ->
            {ok, "Take a Cookie"};
        false ->
            {error, "Failed"}
    end.


add_auth({UserName, Realm}, []) ->
    ok.

update_auth({UserName, Realm}, []) ->
    ok.

delete_auth({UserName, Realm}) ->
    ok.


check_authz({UserName, Realm}, []) ->
    ok.

add_authz({UserName, Realm}, []) ->
    ok.

update_authz({UserName, Realm}, []) ->
    ok.

delete_authz({UserName, Realm}, []) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


