-module(scio_config_test).


-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").

test_env() ->
    ?assert_equal("test", scio_config:env()).
