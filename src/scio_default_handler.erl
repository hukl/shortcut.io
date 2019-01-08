-module(scio_default_handler).

-export([init/2]).


init(Request, State) ->
    parse_request(Request, State).


% ##############################################################################
% # Request Handlers                                                           #
% ##############################################################################

parse_request(#{ path := Path } = Request, State) ->
    Response = handle_request(Path, Request),

    {ok, Response, State}.

% ##############################################################################
% # Internal API                                                               #
% ##############################################################################

handle_request(<<"/">>, Request) ->
    Body = <<"Hello World">>,
    cowboy_req:reply(
      200,
      #{<<"content-type">> => <<"text/plain">>},
      Body,
      Request
    );

handle_request(<<"/health">>, Request) ->
    Body = <<"OK">>,
    cowboy_req:reply(
      200,
      #{<<"content-type">> => <<"text/plain">>},
      Body,
      Request
    );

handle_request(_, Request) ->
    Body = <<"NOT FOUND">>,

    cowboy_req:reply(
      404,
      #{<<"content-type">> => <<"text/plain">>},
      Body,
      Request
    ).
