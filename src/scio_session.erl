-module(scio_session).


-export([
    generate_session_id/0,
    sign_session_id/2,
    generate_signed_session_id/1,
    validate_session_id/2
]).

-spec generate_session_id() -> binary().
generate_session_id() ->
    crypto:strong_rand_bytes(32).


-spec sign_session_id(binary(), bitstring()) -> bitstring().
sign_session_id(Secret, Message) ->
    crypto:hmac(sha256, Secret, Message).


-spec generate_signed_session_id(bitstring()) -> bitstring().
generate_signed_session_id(Secret) ->
    SessionId       = generate_session_id(),
    Base64SessionId = base64:encode(SessionId),
    Signature       = sign_session_id(Secret, SessionId),
    Base64Signature = base64:encode(Signature),

   << Base64SessionId/binary, ".", Base64Signature/binary >>.


-spec validate_session_id(bitstring(), bitstring()) -> boolean().
validate_session_id(Secret, SessionIdWithSignature) ->
    [SessionId, Signature] = binary:split(SessionIdWithSignature, <<".">>),

    DecordedSessionId = base64:decode(SessionId),
    DecodedSignature  = base64:decode(Signature),

       ExpectedSignature = crypto:hmac(
        sha256,
        Secret,
        DecordedSessionId
    ),

    ExpectedSignature =:= DecodedSignature.



