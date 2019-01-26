-module(scio_session_store_test).


-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").

before_suite() ->
    application:ensure_all_started(scio).

after_suite() ->
    application:stop(scio).

test_creating_a_session() ->
    UserId = <<"1">>,
    {ok, Session} = scio_session_store:create(UserId),
    ?assert_equal("", Session).


test_deleting_a_session() ->
    ok.


test_validating_a_session() ->
    ok.


test_expiring_a_session_while_validating() ->
    ok.




