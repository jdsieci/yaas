%% -*- coding: utf-8 -*-
%% @private
%% @author tofik
%% @doc.

-module(yaas_realm, [Id,
                      Realm::string(),
                      ParentRealmId,
                      Children]).

-belongs_to_yaas_realm(parent_realm).
-has({yaas_user, many}).
-has({yaas_group, many}).
%%
%% Include files
%%
-include("yaas_realm.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([validation_tests/1]).

validation_tests(on_create) ->
    [{fun() -> [] == boss_db:find(yaas_realm, [{realm, 'equals', Realm}])
     end, "Realm exists"}].

%% ====================================================================
%% Internal functions
%% ====================================================================


