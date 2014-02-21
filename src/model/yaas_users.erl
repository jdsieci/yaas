%% -*- coding: utf-8 -*-
%% @author tofik
%% @doc @todo Add description to yaas_auth.


-module(yaas_users,[Id,
                   UserName::string(),
                   Password,
                   RealmId,
                   MainGroupId]).

-belongs_to_yaas_realms(realm).
-belongs_to_yaas_groups(main_group).
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
                                              {realmid, 'equals', RealmId}])
      end, "User exists"}].

before_create() ->
    ModifiedRecord = set(password, bcrypt:hashpw(Password, brypt:gen_salt())),
    {ok, ModifiedRecord}.

before_update() ->
    #yaas_user{password = OldPassword} = boss_db:find(Id),
    case OldPassword == Password of
        false -> ModifiedRecord = set(password, bcrypt:hashpw(Password, brypt:gen_salt())),
                 {ok, ModifiedRecord};
        true -> ok
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================


