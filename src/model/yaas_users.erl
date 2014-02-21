%% -*- coding: utf-8 -*-
%% @author tofik
%% @doc @todo Add description to yaas_auth.


-module(yaas_users,[Id,
                   UserName::string(),
                   Password,
                   RealmID,
                   MainGroup]).

-belongs_to_yaas_realms(realmid).
-belongs_to_yaas_groups(maingroup).
%%
%% Include files
%%
-include("yaas_user.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([validation_tests/1,
         before_create/0,
         before_update/0]).


validation_tests(on_create) ->
    [{fun() -> [] == boss_db:find(yaas_user, [{username, 'equals', Username},
                                              {realmid, 'equals', RealmID}])
      end, "User exists"}].

before_create() ->
    ModifiedRecord = set(password, hash_password()),
    {ok, ModifiedRecord}.

before_update() ->
    #yaas_user{password = OldPassword} = boss_db:find(Id),
    case OldPassword == Password of
        false -> ModifiedRecord = set(password, hash_password()),
                 {ok, ModifiedRecord};
        true -> ok
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================


