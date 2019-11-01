-module(scio_shortcut_handler).

-export([handle_request/4]).

-include("scio.hrl").


% No Session
handle_request(_, _, Request, undefined) ->
    scio_default_handler:error_response(403, <<"Forbidden">>, Request);


handle_request(<<"POST">>, [], Request, Session) ->
    {ok, Json, _RequestWithBody} = cowboy_req:read_body(Request),

    Params           = jiffy:decode(Json, [return_maps]),
    ParamsWithUserId = maps:put(<<"user_id">>, Session#session.user_id, Params),

    case scio_shortcut:create(ParamsWithUserId) of
        {ok, Shortcut} ->
            spawn(scio_screenshot, fetch_url, [Shortcut]),
            {ok, 201, #{<<"location">> => <<"/">>}, <<"ok">>, Request};
        {error, _Reason} ->
            scio_default_handler:error_response(400, <<"Bad Request">>, Request)
    end;


handle_request(<<"GET">>, [], Request, #session{ user_id = UserId }) ->
    {ok, Shortcuts} = scio_shortcut:find_all_by_user_id(UserId),

    MapFun = fun(Shortcut) ->
        scio_shortcut:to_map(Shortcut)
    end,

    ShortcutList = lists:map(MapFun, Shortcuts),
    JsonResponse = jiffy:encode(ShortcutList),

    {ok, 200, #{}, JsonResponse, Request};


handle_request(<<"GET">>, [ShortcutId], Request, #session{ user_id = UserId }) ->
    {ok, Shortcut} = scio_shortcut:find(
        UserId, erlang:binary_to_integer(ShortcutId)
    ),

    Response     = scio_shortcut:to_map(Shortcut),
    JsonResponse = jiffy:encode(Response),

    {ok, 200, #{}, JsonResponse, Request};


handle_request(<<"PUT">>, [ShortcutId], Request, #session{ user_id = UserId }) ->
    {ok, Json, _RequestWithBody} = cowboy_req:read_body(Request),

    Params = jiffy:decode(Json, [return_maps]),

    Sid = erlang:binary_to_integer(ShortcutId),
    case scio_shortcut:update(Sid, UserId, Params) of
        {ok, Shortcut} ->
            Response     = scio_shortcut:to_map(Shortcut),
            JsonResponse = jiffy:encode(Response),

            {ok, 200, #{}, JsonResponse, Request};
        {error, no_record_found} ->
            scio_default_handler:error_response(404, <<"Not Found">>, Request);
        {error, _} ->
            scio_default_handler:error_response(400, <<"Bad Request">>, Request)
    end.
