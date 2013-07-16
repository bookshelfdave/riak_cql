-module(riak_cql_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() -> {ok, T} = riak_cql_parser:string("{counter foo++, counter bar--}"),
                 Expected = [{update,
                              [{update,{"foo",riak_dt_pncounter},{increment,1}},
                               {update,{"bar",riak_dt_pncounter},{decrement,1}}]}],
                 ?assertEqual(Expected, T).
