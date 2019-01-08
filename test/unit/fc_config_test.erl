-module(fc_config_test).


-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").



test_loading_postgres_config() ->
    {ok, Value} = scio_config:db(),
    ?assert(erlang:is_list(Value)).


test_env() ->
    ?assert_equal("test", scio_config:env()).
