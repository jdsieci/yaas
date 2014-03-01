%% -*- coding: utf-8 -*-
%%% -------------------------------------------------------------------
%%% Author  : tofik
%%% Description :
%%%
%%% Created : 07-03-2013
%%% -------------------------------------------------------------------
-module(yaas_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start/0, start_link/0, stop/1]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------
-record(pool, {name, size, worker_args}).
%% ====================================================================
%% External functions
%% ====================================================================
start() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop(_State) ->
    ok.

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
%%     {ok, AuthzPool} = application:get_env(yaas, authz),
    {ok, AuthPool} = get_pool_config(yaas_auth),
    AuthSpec = poolboy:child_spec(AuthPool#pool.name,
                                  [{name, {local, AuthPool#pool.name}},
                                   {worker_module, yaas_auth}] ++ AuthPool#pool.size,
                                  AuthPool#pool.worker_args
                                   ),
    Children = [AuthSpec],
    RestartStrategy = {one_for_one, 10 , 10},
    {ok, {RestartStrategy, Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec get_pool_config(Pool::atom()) -> Result when
          Result :: {ok, Config}
              | {error, Reason},
          Config :: term(),
          Reason :: term.

get_pool_config(Pool) ->
    case application:get_env(Pool) of
        {ok, {SizeArgs, WorkerArgs}} ->
            {ok, #pool{name = Pool, size = SizeArgs, worker_args = WorkerArgs }};
        undefined ->
            {error, "Undefined pool"}
    end.
