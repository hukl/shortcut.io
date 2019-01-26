-module(scio_session_store_test).


-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").

-include("scio.hrl").

before_suite() ->
    application:ensure_all_started(scio).

after_suite() ->
    application:stop(scio).


test_storing_a_session() ->
    User = #user{
        id       = <<"1">>,
        uuid     = <<"some-uuid">>,
        username = <<"Bente">>,
        password = <<"foo">>,
        email    = <<"foo@bar.com">>
    },

    {ok, Session} = scio_session:new(User),
    {ok, Session} = scio_session_store:save(Session).



test_validating_a_bogus_session_returns_error() ->
    Result = scio_session_store:validate(<<"somebogusr.sessionid">>),
    ?assert_equal({error, invalid}, Result).


test_deleting_a_session() ->
    ok.


test_validating_a_session() ->
    ok.


test_expiring_a_session_while_validating() ->
    ok.




