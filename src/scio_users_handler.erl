-module(scio_users_handler).

-compile(export_all).

handle_request(<<"new">>, _Request) ->
    Body = user_new_view:render(#{}),
    {ok, 200, {}, Body}.
