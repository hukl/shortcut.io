-module(scio_session_handler).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    Req1 = cowboy_req:set_resp_cookie(
        <<"session">>, scio_session:generate_signed_session_id(<<"mysecret">>),
        Req, #{path => <<"/">>}
    ),
    io:format("REQ: ~p~n", [Req1]),
    {ok, Req1, Env}.
