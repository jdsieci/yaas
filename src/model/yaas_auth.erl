%% -*- coding: utf-8 -*-
%% @author tofik
%% @doc @todo Add description to yaas_auth.


-module(yaas_auth,[Id,
                   UserName::string(),
                   Password]).
%%
%% Include files
%%
-include("yaas_auth.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([validation_tests/1,
         before_create/0,
         before_update/0]).


validation_tests(on_create) ->
    [{fun() -> [] == boss_db:find(yaas_auth, [{username, 'equals', Username}])
      end, "User exists"}].

before_create() ->
    ModifiedRecord = set(password, hash_password()),
    {ok, ModifiedRecord}.

before_update() ->
    #yaas_auth{password = OldPassword} = boss_db:find(Id),
    case OldPassword == Password of
        false -> ModifiedRecord = set(password, hash_password()),
                 {ok, ModifiedRecord};
        true -> ok
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

hash_password() ->
    lists:flatten([io_lib:fwrite("~2.16.0b", [Byte]) || Byte <- binary_to_list(erlang:md5(Password))]).
