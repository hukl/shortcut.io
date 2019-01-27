-module(scio_session_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

-include("scio.hrl").

execute(Req, Env) ->
    % check if there is a session cookie
    % if so then validate it and retrieve the session
    % if that succeeds, put the session into the request binding
    % {_Binding, Req2} = cowboy_req:binding(session, Req1, Session)
    % if the checks fail, undefined will be returned when trying to access it


    Req1 = cowboy_req:set_resp_cookie(
        <<"session">>, scio_session:generate_signed_session_id(<<"mysecret">>),
        Req, #{path => <<"/">>}
    ),

    {ok, Req1, Env}.
