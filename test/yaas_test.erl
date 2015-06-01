%% @author tofik
%% @doc @todo Add description to yaas_test.


-module(yaas_test).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

yaas_test_() ->
    {inorder, [?assertMatch({ok, _}, yaas:start()),
               ?assertEqual(ok, yaas:stop())
    ]}.

-endif.