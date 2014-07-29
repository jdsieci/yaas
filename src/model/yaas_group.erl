%% -*- coding: utf-8 -*-
%% @author tofik
%% @doc.

-module(yaas_group,[Id,
                     Group::string(),
                     RealmId,
                     Users]).

-belongs_to_yaas_realm(realm).
-has({yaas_user, many}).
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
                                               {realmid, 'equals', RealmId}])
      end, "Group exists"}].



%% ====================================================================
%% Internal functions
%% ====================================================================


