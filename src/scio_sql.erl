-module(scio_sql).

-export([squery/2, equery/3, with_transaction/3]).

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
