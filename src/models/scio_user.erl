-module(scio_user).

-export([
    first/0,
    count/0,
    create/1,
    find_by_email/1,
    verify_password/2
]).

-include("scio.hrl").


% CRUD

-spec create(#{ bitstring() => bitstring() }) -> {'ok', #user{}} | {'error', tuple()}.
create(#{
    <<"username">> := Username,
    <<"email">>    := Email,
    <<"password">> := Password} = _Params) ->

    {ok, CryptedPassword} = scio_utils:crypt_password(Password),

    Query = "INSERT INTO users "
             "    (username, email, password) "
             "VALUES  "
             "    ($1, $2, $3) "
             "RETURNING id, uuid, username, email, password;",

    case scio_sql:equery(pg, Query, [Username, Email, CryptedPassword]) of
        {ok, _Count, _Colums, [{Id, Uuid, Username, Email, CryptedPassword}]}->

            User = #user{
                id       = Id,
                uuid     = Uuid,
                username = Username,
                email    = Email,
                password = CryptedPassword
            },

            {ok, User};
        {error, Error}
            -> {error, Error}
    end.


-spec find_by_email(bitstring()) -> {'ok', #user{}} | {'error', term()}.
find_by_email(Email) ->
    Query = "SELECT id, uuid, username, email, password "
            "FROM users "
            "WHERE email = $1;",

    case scio_sql:equery(pg, Query, [Email]) of
        {ok, _Colums, [{Id, Uuid, Username, Email, CryptedPassword}]}->

            User = #user{
                id       = Id,
                uuid     = Uuid,
                username = Username,
                email    = Email,
                password = CryptedPassword
            },

            {ok, User};
        {ok, _Colums, []} ->
            {error, not_found};
        {error, Error} ->
            {error, Error}
    end.

% Helpers

first() ->
    geil.

-spec count() -> {'ok', integer()} | {'error', tuple()}.
count() ->
    Query = "SELECT count(id) FROM users;",

    case scio_sql:equery(pg, Query, []) of
        {ok, _Colums, Rows} ->
            [Count] = [Row || {Row} <- Rows],
            {ok, Count};
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% Helpers
%% ===================================================================

-spec verify_password(bitstring(), #user{}) -> boolean().
verify_password(Password, User) ->
    scio_utils:check_password(Password, User#user.password).
