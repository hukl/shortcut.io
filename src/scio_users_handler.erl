-module(scio_users_handler).

-export([handle_request/2]).


handle_request([], _Request) ->
    {ok, 200, {}, <<"Formidable">>};

handle_request([<<"new">>], _Request) ->
    Body = user_new_view:render(#{}),
    {ok, 200, {}, Body}.
