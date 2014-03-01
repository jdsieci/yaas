%% @author tofik
%% @doc @todo Add description to yaas.


-module(yaas).

-behaviour(application).

%-include("yaas_realm.hrl").
-include("yaas_user.hrl").
-include("yaas_auth.hrl").

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([start/2, start/0, stop/1]).

%% --------------------------------------------------------------------
%% API exports
%% --------------------------------------------------------------------

%Checks
-export([check_auth/2, check_authz/2]).

%Adds
-export([add_auth/2, add_authz/2]).

%Dels
-export([delete_auth/1, delete_authz/2]).

%Updates
-export([update_auth/2, update_authz/2]).

%% ====================================================================!
%% External functions
%% ====================================================================!

%% --------------------------------------------------------------------
%% Behavioural functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case yass_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            {error, Error}
    end.

start() ->
    start([],[]).
%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------

check_auth({UserName, Realm}, Password) ->
    poolboy:transaction(auth, fun(Worker) ->
                                      gen_server:call(Worker, #check{user = #user{username = UserName, realm = Realm}, password = Password})
                        end).

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


