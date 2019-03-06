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
    end;

handle_request(<<"GET">>, [], Request, #session{ user_id = UserId }) ->
    {ok, Shortcuts} = scio_shortcut:find_all_by_user_id(UserId),

    MapFun = fun(Shortcut) ->
        #{
            <<"id">>                => Shortcut#shortcut.id,
            <<"url">>               => Shortcut#shortcut.url,
            <<"title">>             => Shortcut#shortcut.title,
            <<"description">>       => Shortcut#shortcut.description,
            <<"screenshot_id">>     => Shortcut#shortcut.screenshot_id,
            <<"created_at">>        => Shortcut#shortcut.created_at,
            <<"updated_at">>        => Shortcut#shortcut.updated_at
        }
    end,

    ShortcutList = lists:map(MapFun, Shortcuts),

    JsonResponse = jiffy:encode(ShortcutList),

    {ok, 200, #{<<"content-type">> => <<"application/json">>}, JsonResponse, Request};


handle_request(<<"GET">>, [ShortcutId], Request, #session{ user_id = UserId }) ->
    {ok, Shortcut} = scio_shortcut:find(
        UserId, erlang:binary_to_integer(ShortcutId)
    ),

    Response = #{
        <<"id">>                => Shortcut#shortcut.id,
        <<"url">>               => Shortcut#shortcut.url,
        <<"title">>             => Shortcut#shortcut.title,
        <<"description">>       => Shortcut#shortcut.description,
        <<"screenshot_id">>     => Shortcut#shortcut.screenshot_id,
        <<"created_at">>        => Shortcut#shortcut.created_at,
        <<"updated_at">>        => Shortcut#shortcut.updated_at
    },

    JsonResponse = jiffy:encode(Response),

    {ok, 200, #{<<"content-type">> => <<"application/json">>}, JsonResponse, Request};


handle_request(<<"PUT">>, [ShortcutId], Request, #session{ user_id = _UserId }) ->
    {ok, Json, _RequestWithBody} = cowboy_req:read_body(Request),

    Params = jiffy:decode(Json, [return_maps]),

    {ok, _} = scio_shortcut:update(erlang:binary_to_integer(ShortcutId), Params),
    {ok, 200, #{<<"content-type">> => <<"application/json">>}, <<"">>, Request}.
