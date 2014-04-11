%% @author tofik
%% @doc @todo Add description to yaas_main_sup.


-module(yaas_main_sup).
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
	BossDb = boss_db_spec(),
	Children = [YaasSup] ++ BossDb,
    {ok,{{one_for_one, 0, 1}, Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

boss_db_spec() ->
	Options = application:get_env(db_opts, []),
    AdapterName = proplists:get_value(adapter, Options, mock),
    Adapter     = list_to_atom(lists:concat(["boss_db_adapter_", AdapterName])),
    lager:info("Start Database Adapter ~p options ~p", [Adapter, Options]),
    Adapter:start(Options),
    lists:foldr(fun(ShardOptions, Acc) ->
                case proplists:get_value(db_shard_models, ShardOptions, []) of
                    [] -> Acc;
                    _ ->
                        ShardAdapter = case proplists:get_value(db_adapter, ShardOptions) of
                            undefined -> Adapter;
                            ShortName -> list_to_atom(lists:concat(["boss_db_adapter_", ShortName]))
                        end,
                        ShardAdapter:start(ShardOptions ++ Options),
                        Acc
                end
        end, [], proplists:get_value(shards, Options, [])),
	case boss_db_cache(Options) of
		{} ->	boss_db_spec(Options);
		CacheSpec ->	[ CacheSpec | boss_db_spec(Options)]
	end.

boss_db_spec(Options) ->
	[{boss_db, {boss_db_sup, start_link, Options}, permanent, infinity, supervisor, [boss_db_sup]},
	 {boss_news, {boss_news_sup, start_link, []}, permanent, infinity, supervisor, [boss_news_sup]}].

boss_db_cache(Options) ->
	case proplists:get_value(cache_enable, Options, false) of
        true ->
            prepare_db_cache(Options);
        false ->
            {}
    end.

prepare_db_cache(Options) ->
    CacheOpts = [{adapter, proplist:get_value(cache_adapter, Options, memcached_bin)},
                 {cache_servers, proplists:get_value(cache_servers, Options)}],
    AdapterName = proplists:get_value(adapter, CacheOpts, memcached_bin),
    Adapter	= list_to_atom(lists:concat(["boss_cache_adapter_", AdapterName])),
    Adapter:start(CacheOpts),
	{boss_cache, {boss_cache_sup, start_link, CacheOpts}, permanent, infinity, supervisor, [boss_cache_sup]}.