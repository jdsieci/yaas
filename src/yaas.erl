%% @author tofik
%% @doc @todo Add description to yaas.


-module(yaas).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, stop/0]).

%Checks
-export([check_auth/2]).

%Adds
-export([add_auth/2]).

start() ->
    application:start(yaas).

stop() ->
    application:stop(stop).

check_auth(UserName, Password) ->
    ok.


add_auth(UserName,[]) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


