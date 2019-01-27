-module(scio_users_handler).

-export([handle_request/3]).


handle_request(<<"POST">>, [], Request) ->
    {ok, Json, _RequestWithBody} = cowboy_req:read_body(Request),

    Params = jiffy:decode(Json, [return_maps]),

    case scio_user:create(Params) of
        {ok, _User} ->
            {ok, 303, #{<<"location">> => <<"/">>}, <<"CREATE">>, Request};
        {error, email_already_exists} ->
            Message = #{
              <<"ok">>    => <<"false">>,
              <<"error">> => <<"Email Address already exists">>
            },
            {ok, 400, #{}, jiffy:encode(Message), Request};
        {error, Reason} ->
            logger:error("Unexpected Error: ~p~n", [Reason]),
            Message = #{
              <<"ok">>    => <<"false">>,
              <<"error">> => <<"An unexpected error has occurred">>
            },
            {ok, 400, #{}, jiffy:encode(Message), Request}
    end;


handle_request(<<"GET">>, [<<"new">>], Request) ->
    Body = user_new_view:render(#{}),
    {ok, 200, #{}, Body, Request};


handle_request(_, _, Request) ->
    {ok, 404, #{}, <<"NOT FOUND">>, Request}.
