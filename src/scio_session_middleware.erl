-module(scio_session_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

-include("include/scio.hrl").

execute(Req, Env) ->

    SessionId = extract_session_id(Req),

    case scio_session_store:validate(SessionId) of
        {ok, Session} ->
            Bindings       = maps:get(bindings, Req),
            NewBindings    = maps:put(session, Session, Bindings),
            ReqWithSession = maps:put(bindings, NewBindings, Req),

            {ok, ReqWithSession, Env};
        {error, _} ->
            {ok, Req, Env}
    end.


extract_session_id(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),

    case lists:keyfind(<<"session">>, 1, Cookies) of
        {_, SessioId} -> SessioId;
        false         -> <<"">>
    end.

