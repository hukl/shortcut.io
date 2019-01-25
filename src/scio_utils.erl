-module(scio_utils).

-export([crypt_password/1, check_password/2]).

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
