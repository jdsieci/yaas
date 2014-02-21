%% -*- coding: utf-8 -*-
%% @author tofik
%% @doc.

-module(yaas_groups,[Id,
                     Group,
                     RealmID,
                     Users]).

-belongs_to_yaas_realms(realmid).

%%
%% Include files
%%
-include("yaas_group.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([validation_tests/1]).

validation_tests(on_create) ->
    [{fun() -> [] == boss_db:find(yaas_group, [{group, 'equals', Group},
                                               {realmid, 'equals', RealmID}])
      end, "Group exists"}].



%% ====================================================================
%% Internal functions
%% ====================================================================


