%% @author tofik
%% @doc @todo Add description to yaas.


-module(yaas).

-behaviour(application).

-include("yaas_user.hrl").

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([start/2, stop/1]).

%% --------------------------------------------------------------------
%% API exports
%% --------------------------------------------------------------------
-export([start/0, stop/0]).
-export([login/2, logout/1]).

%Types
-type uuid()        :: binary().
-type cookie()      :: uuid().

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
    start_boss_db(),
    case yaas_main_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            {error, Error}
    end.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    boss_news:stop(),
    boss_cache:stop(),
    boss_db:stop(),
    ok.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
start() ->
    ensure_started(crypto),
    ensure_started(bcrypt),
    application:start(?MODULE).

stop() ->
    Res = application:stop(?MODULE),
    application:stop(bcrypt),
    application:stop(crypto),
    Res.

-spec login(User::record(), Password ::string() ) -> Result when
          Result :: {ok, cookie()}
            | {error, Reason},
          Reason :: term().

login(#user{username = UserName, realm = Realm}, Password) ->
    {ok, uuid:uuid4()}.


-spec logout(Cookie::cookie()) -> Result when
          Result :: ok
            | {error, Reason},
          Reason :: term().

logout(Cookie) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
ensure_started(App, Opts) ->
    case App:start(Opts) of
        ok ->
            ok;
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid}
    end.

start_boss_db() ->
    DBOpts = application:get_env(?MODULE, db_opts, []),
    ensure_started(boss_db, DBOpts),
    case proplists:get_value(cache_enable, DBOpts, false) of
        true ->
            start_db_cache(proplists:get_value(cache_adapter, DBOpts), DBOpts);
        false ->
            ok
    end,
    ensure_started(boss_news, []),
    ok.

start_db_cache(memcached_bin, Opts) ->
    CacheOpts = [{adapter, memcached_bin},
                 {cache_servers, proplists:get_value(cache_servers, Opts)}],
    ensure_started(boss_cache, CacheOpts);
start_db_cache(_, _) ->
    ok.