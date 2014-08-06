%% -*- coding: utf-8 -*-
%% @private
%% @author tofik
%% @doc.

-module(group,[Id,
                     Group::string(),
                     RealmId,
                     Users]).
-package(yaas.model).
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


