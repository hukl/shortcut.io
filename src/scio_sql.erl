-module(scio_sql).

-export([squery/2, equery/3, with_transaction/3, flush_db/0]).

squery(PoolName, Sql) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {squery, Sql})
    end).

equery(PoolName, Stmt, Params) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {equery, Stmt, Params})
    end).


with_transaction(PoolName, Fun, Opts) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {with_transaction, Fun, Opts})
    end).



%%====================================================================
%% Helpers
%%====================================================================

flush_db() ->
    {ok, TableNames} = table_names(),

    Query = <<"TRUNCATE ", TableNames/binary>>,

    case scio_sql:squery(pg, Query) of
        {ok, _Colums, []} ->
            {ok, []};
        {error, Error} ->
            {error, Error}
    end.


-spec table_names() -> {'ok', bitstring()} | {'error', tuple()}.
table_names() ->
    Query = "SELECT "
            "  array_to_string(array( "
            "    SELECT table_name  "
            "    FROM information_schema.tables "
            "    WHERE table_schema='public' "
            "  ), ', ') AS table_names; ",

    case scio_sql:squery(pg, Query) of
        {ok, _Colums, Rows} ->
            [TableNames] = [Row || {Row} <- Rows],
            {ok, TableNames};
        {error, Error} ->
            {error, Error}
    end.
