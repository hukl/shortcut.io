-module(scio_users_handler).

-export([handle_request/4]).


handle_request(<<"POST">>, [], Request, _) ->
    {ok, Json, _RequestWithBody} = cowboy_req:read_body(Request),

    Params = jiffy:decode(Json, [return_maps]),

    case scio_user:create(Params) of
        {ok, _User} ->
            {ok, 201, #{<<"location">> => <<"/">>}, <<"CREATE">>, Request};
        {error, email_already_exists} ->
            scio_default_handler:error_response(400, <<"Email Address already exists">>, Request);
        {error, Reason} ->
            logger:error("Unexpected Error: ~p~n", [Reason]),
            scio_default_handler:error_response(400, <<"Bad Request">>, Request)
    end;


handle_request(_, _, Request, _) ->
    scio_default_handler:error_response(404, <<"Not Found">>, Request).
