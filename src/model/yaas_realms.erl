%% -*- coding: utf-8 -*-
%% @author tofik
%% @doc.

-module(yaas_realms, [Id,
                      Realm::string(),
                      ParentRealmId,
                      Children]).

-belongs_to_yaas_realms(parent_realm).

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


