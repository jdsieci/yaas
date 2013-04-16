%% @author tofik
%% @doc @todo Add description to yaas.


-module(yaas).

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

check_auth(UserName, Password) ->
    ok.


add_auth(UserName,[]) ->
    ok.

update_auth(UserName, []) ->
    ok.

delete_auth(UserName) ->
    ok.


check_authz(UserName, []) ->
    ok.

add_authz(UserName, []) ->
    ok.

update_authz(UserName, []) ->
    ok.

delete_authz(UserName, []) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


