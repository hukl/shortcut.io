-module(scio_users_handler).

-export([handle_request/3]).


handle_request(<<"GET">>, [], _Request) ->
    {ok, 200, #{}, <<"Formidable">>};


handle_request(<<"POST">>, [], Request) ->
    {ok, Json, _RequestWithBody} = cowboy_req:read_body(Request),

    Params = jiffy:decode(Json, [return_maps]),

    {ok, _User} = scio_user:create(Params),

    {ok, 303, #{<<"location">> => <<"/">>}, <<"CREATE">>};


handle_request(<<"GET">>, [<<"new">>], _Request) ->
    Body = user_new_view:render(#{}),
    {ok, 200, #{}, Body};


handle_request(_, _, _Request) ->
    {ok, 404, #{}, <<"NOT FOUND">>}.
