-module(scio_session).

-include("scio.hrl").

-export([
    new/1,
    generate_session_id/0,
    sign_session_id/2,
    generate_signed_session_id/1,
    validate_session_id/1,
    validate_session_id/2
]).


-spec new(#user{}) -> {'ok', #session{}} | {'error', atom()}.
new(#user{ id = UserId }) ->
    TimeNow   = scio_utils:timestamp(),

    try
        SessionId = generate_signed_session_id(cookie_secret()),

        Session = #session{
            session_id = SessionId,
            user_id    = UserId,
            created_at = TimeNow
        },

        {ok, Session}
    catch
        Error:Reason ->
            logger:error("Create Session Failed ~p~n~p~n", [Error, Reason]),
            {error, failed_to_create_session}
    end.


-spec generate_session_id() -> binary().
generate_session_id() ->
    crypto:strong_rand_bytes(32).


-spec sign_session_id(binary(), bitstring()) -> binary().
sign_session_id(Secret, Message) ->
    crypto:hmac(sha256, Secret, Message).


-spec generate_signed_session_id(bitstring()) -> bitstring().
generate_signed_session_id(Secret) ->
    SessionId       = generate_session_id(),
    Base64SessionId = base64:encode(SessionId),
    Signature       = sign_session_id(Secret, SessionId),
    Base64Signature = base64:encode(Signature),

   << Base64SessionId/binary, ".", Base64Signature/binary >>.


-spec validate_session_id(bitstring()) -> boolean().
validate_session_id(SessionIdWithSignature) ->
    try
        validate_session_id(cookie_secret(), SessionIdWithSignature)
    catch
        Error:Reason ->
            logger:error("Invalid Signature ~p~n~p~n", [Error, Reason]),
            false
    end.


-spec validate_session_id(bitstring(), bitstring()) -> boolean().
validate_session_id(Secret, SessionIdWithSignature) ->
    try
        [SessionId, Signature] = binary:split(SessionIdWithSignature, <<".">>),

        DecordedSessionId = base64:decode(SessionId),
        DecodedSignature  = base64:decode(Signature),

        ExpectedSignature = crypto:hmac(
            sha256,
            Secret,
            DecordedSessionId
        ),

        ExpectedSignature =:= DecodedSignature
    catch
        _:_ -> false
    end.


cookie_secret() ->
    {ok, [Secrets]} = application:get_env(scio, secrets),
    proplists:get_value(cookie_secret, Secrets).
