%% -*- coding: utf-8 -*-
%%% -------------------------------------------------------------------
%%% Author  : tofik
%%% Description :
%%%
%%% Created : 28-02-2014
%%% -------------------------------------------------------------------

-module(yaas_auth).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-define(SERVER, ?MODULE).

-include("yaas_realm.hrl").
-include("yaas_user.hrl").
-include("yaas_auth.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

init(_Args) ->
    ok.

handle_call(#check{user = #user{username = UserName, realm = Realm}, password = Password}, _From, State) ->
    {ok, #yaas_realm{id = RealmId}} = boss_db:find(yaas_realm, [{realm,'equals', Realm}]),
    {ok, #yaas_user{password = UserPassword}} = boss_db:find(yaas_user, [{user_name, 'equals', UserName},
                                                                         {realm, 'equals', RealmId}]),
    case {ok, UserPassword} =:= bcrypt:hashpw(Password, UserPassword) of
        true ->
            {reply, ok, State};
        false ->
            {reply, error, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
    

terminate(shutdown, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================


