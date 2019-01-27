-module(scio_users_handler).

-export([handle_request/3]).


handle_request(<<"GET">>, [], Request) ->
    {ok, 200, #{}, <<"Formidable">>, Request};


handle_request(<<"POST">>, [], Request) ->
    {ok, Json, _RequestWithBody} = cowboy_req:read_body(Request),

    Params = jiffy:decode(Json, [return_maps]),

    {ok, _User} = scio_user:create(Params),

    {ok, 303, #{<<"location">> => <<"/">>}, <<"CREATE">>, Request};


handle_request(<<"GET">>, [<<"new">>], Request) ->
    Body = user_new_view:render(#{}),
    {ok, 200, #{}, Body, Request};


handle_request(_, _, Request) ->
    {ok, 404, #{}, <<"NOT FOUND">>, Request}.
