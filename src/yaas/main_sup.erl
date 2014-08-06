%% @author tofik
%% @doc @todo Add description to yaas_main_sup.


-module(yaas.main_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, start_link/0, stop/1]).

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    YaasSup = {'yaas_sup' ,{'yaas_sup', start_link,[]},
			   permanent, infinity, supervisor, ['yaas_sup']},
    YaasController = {yaas_controller ,{yaas_controller, start_link,[]},
               permanent, infinity, worker, dynamic},
	Children = [YaasController, YaasSup],
    {ok,{{one_for_one, 0, 1}, Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

