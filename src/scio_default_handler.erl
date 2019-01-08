-module(scio_default_handler).

-export([init/2]).


init(Request, State) ->
    handle_request(Request, State).


% ##############################################################################
% # Request Handlers                                                           #
% ##############################################################################

handle_request(Request, State) ->
    logger:info("HELLLLO"),
    Response = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello World!">>, Request),

    {ok, Response, State}.

% ##############################################################################
% # Internal API                                                               #
% ##############################################################################


