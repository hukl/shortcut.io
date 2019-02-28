-module(scio_shortcut_handler).

-export([handle_request/4]).

-include("scio.hrl").


handle_request(_, _, Request, undefined) ->
    {ok, 403, #{}, <<"Forbidden">>, Request};

handle_request(<<"POST">>, [], Request, Session) ->
    {ok, Json, _RequestWithBody} = cowboy_req:read_body(Request),

    Params           = jiffy:decode(Json, [return_maps]),
    ParamsWithUserId = maps:put(<<"user_id">>, Session#session.user_id, Params),

    case scio_shortcut:create(ParamsWithUserId) of
        {ok, _} ->
            {ok, 201, #{<<"location">> => <<"/">>}, <<"ok">>, Request};
        {error, _Reason} ->
            {ok, 400, #{}, <<"Bad Request">>, Request}
    end.
