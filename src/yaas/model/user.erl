%% -*- coding: utf-8 -*-
%% @private
%% @author tofik


-module(user, [Id,
                   UserName::string(),
                   Password,
                   RealmId,
                   MainGroupId]).
-package(yaas.model).
-belongs_to_yaas_realm(realm).
-belongs_to_yaas_group(main_group).
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
    [{fun() -> [] == boss_db:find(yaas_user, [{user_name, 'equals', UserName},
                                              {realm_id, 'equals', RealmId}])
      end, "User exists"}].

before_create() ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, HashedPassword} = bcrypt:hashpw(Password, Salt),
    ModifiedRecord = set(password, HashedPassword),
    {ok, ModifiedRecord}.

before_update() ->
    #yaas_user{password = OldPassword} = boss_db:find(Id),
    case OldPassword =:= Password of
        false ->
            {ok, Salt} = bcrypt:gen_salt(),
            {ok, HashedPassword} = bcrypt:hashpw(Password, Salt),
            ModifiedRecord = set(password, HashedPassword),
            {ok, ModifiedRecord};
        true -> ok
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================
