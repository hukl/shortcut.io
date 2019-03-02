-module(scio_session_test).

-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").

-include("scio.hrl").

before_suite() ->
    scio_config:load_secrets().


test_new_session() ->
    User = #user{
        id       = 1,
        uuid     = <<"some-uuid">>,
        username = <<"Bente">>,
        password = <<"foo">>,
        email    = <<"foo@bar.com">>
    },

    {ok, Session} = scio_session:new(User),

    ?assert_equal(1, Session#session.user_id).


test_generating_a_session_id() ->
    SessionID = scio_session:generate_session_id(),
    ?assert_equal(32, erlang:size(SessionID)).


test_signing_a_session_id() ->
    SessionID     = scio_session:generate_session_id(),
    Secret        = <<"mysecret23">>,
    SignedSession = scio_session:sign_session_id(SessionID, Secret),

    ?assert(0 < erlang:size(SignedSession)),

    % For sha256
    ?assert(32 =:= size(SignedSession)),
    ?assert_not_equal(SessionID, SignedSession).


test_generating_a_signed_session_id() ->
    Secret = <<"mysecret42">>,

    SessionId = scio_session:generate_signed_session_id(Secret),

    ?assert(scio_session:validate_session_id(Secret, SessionId)).
