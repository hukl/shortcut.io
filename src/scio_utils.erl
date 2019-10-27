-module(scio_utils).

-export([
    timestamp/0,
    timestamp/1,
    crypt_password/1,
    check_password/2
]).

% Wrapper function to make mocking / freezing time in tests easy
-spec timestamp() -> integer().
timestamp() ->
    erlang:system_time(seconds).

-spec timestamp('second' | 'millisecond' | 'microsecond') -> integer().
timestamp(Unit) ->
    erlang:system_time(Unit).


-spec crypt_password(bitstring()) -> {ok, bitstring()}.
crypt_password(Password) ->
    {ok, Salt}            = bcrypt:gen_salt(),
    {ok, CryptedPassword} = bcrypt:hashpw(Password, Salt),
    BinaryPassword        = erlang:list_to_bitstring(CryptedPassword),

    {ok, BinaryPassword}.


-spec check_password(bitstring(), bitstring()) -> boolean().
check_password(Password, CryptedPassword) ->
    StringPassword = erlang:bitstring_to_list(CryptedPassword),
    {ok, StringPassword} =:= bcrypt:hashpw(Password, StringPassword).
