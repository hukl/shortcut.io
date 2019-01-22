-module(scio_users_handler).

-compile(export_all).


handle_request([], _Request) ->
    {ok, 200, {}, <<"Formidable">>};

handle_request([<<"new">>], _Request) ->
    Body = user_new_view:render(#{}),
    {ok, 200, {}, Body}.
