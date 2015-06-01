%% @author tofik
%% @doc @todo Add description to yaas_auth_test.


-module(yaas_auth_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


start() ->
    yaas:start(),
    Realm = yaas_realm:new(id, "realm", root, []),
    {ok, SavedRealm} = Realm:save(),
    Group = yaas_group:new(id, "group1", SavedRealm:id(), []),
    {ok, _SavedGroup} = Group:save(),
    Group2 = yaas_group:new(id, "group2", SavedRealm:id(), []),
    {ok, _SavedGroup2} = Group2:save(),
    lager:stop(),
    ok.

stop(_) ->
    yaas:stop().

user_test_() ->
    {"Add user",{setup, fun start/0, fun stop/1, fun(_) ->
                                                         [?assertEqual({error, "User without group"}, yaas_auth:add({"username", "realm"}, [])),
                                                         ?assertEqual({error, "Group does not exist"}, yaas_auth:add({"username", "realm"}, [{group, "notexistinggroup"}])),
                                                         ?assertEqual(ok, yaas_auth:add({"username", "realm"}, [{group, "group1"}]))]
                                                 end}
    }.


-endif.