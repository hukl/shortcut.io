-module(scio_shortcut_handler).

-export([handle_request/3]).

-include("scio.hrl").


handle_request(<<"POST">>, [], Request) ->
    
    case nested:get([bindings, session], Request, undefined) of
        undefined ->
            {ok, 403, #{}, <<"Forbidden">>, Request};
        Session ->
            {ok, Json, _RequestWithBody} = cowboy_req:read_body(Request),
            Params = jiffy:decode(Json, [return_maps]),
            ParamsWithUserId = maps:put(<<"user_id">>, Session#session.user_id, Params),

            case scio_shortcut:create(ParamsWithUserId) of
                {ok, _} ->
                    {ok, 201, #{<<"location">> => <<"/">>}, <<"ok">>, Request};
                {error, _Reason} ->
                    {ok, 403, #{}, <<"Forbidden">>, Request}
            end
    end.
    
