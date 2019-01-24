-module(scio_user).

-export([
    first/0,
    count/0
]).


first() ->
    geil.

count() ->
    Query = "SELECT count(id) FROM users;",

    case scio_sql:equery(pg, Query, []) of
        {ok, _Colums, Rows} ->
            [Count] = [Row || {Row} <- Rows],
            {ok, Count};
        {error, Error} ->
            {error, Error}
    end.


