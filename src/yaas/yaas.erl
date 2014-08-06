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
-export_type([user/0, boss_record/0, property/0, properties/0]).

%Types
-type uuid()        :: binary().
-type cookie()      :: uuid().

-type user() :: {string(), string()}.
-type boss_record() :: module().
-type property() :: {atom(), term()}.
-type properties() :: [property()].



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
    case main_sup:start_link() of
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
    ok.

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
start() ->
    application:ensure_all_started(?MODULE).

stop() ->
    Res = application:stop(?MODULE),
    application:stop(lager),
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

