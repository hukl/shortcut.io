-module(scio_default_handler).

-export([init/2]).

-define(PATH_SEPARATOR, binary:compile_pattern(<<"/">>)).
-define(PATH_OPTIONS,   [trim_all, global]).

init(#{ path := Path } = Request, State) ->
    PathSegments = binary:split(Path, ?PATH_SEPARATOR, ?PATH_OPTIONS),
    logger:error("Path: ~p~n", [PathSegments]),

    {ok, Status, Headers, Body} = handle_request(PathSegments, Request),

    Response = render_html(Status, Headers, Body, Request),

    {ok, Response, State}.


% ##############################################################################
% # Request Handlers                                                           #
% ##############################################################################

handle_request([], _Request) ->
    Body = landing_page_view:render(
        #{
             <<"greeting">> => <<"hello world">>,
             <<"names">>    => [
                #{<<"name">> => <<"alice">>},
                #{<<"name">> => <<"bob">>}
            ]
        }
    ),

    {ok, 200, {}, Body};


handle_request([<<"users">>|Path], Request) ->
    scio_users_handler:handle_request(Path, Request);


handle_request([<<"health">>], _Request) ->
    {ok, 200, {}, <<"OK">>};

handle_request(_, Request) ->
    Body = <<"NOT FOUND">>,

    cowboy_req:reply(
        404,
        #{<<"content-type">> => <<"text/plain">>},
        Body,
        Request
    ).


% ##############################################################################
% # Internal API                                                               #
% ##############################################################################

render_html(Status, {} = _Headers, Body, Request) ->
    Html = layout_view:render(
        #{<<"body">> => Body}
    ),

    cowboy_req:reply(
        Status,
        #{<<"content-type">> => <<"text/html">>},
        Html,
        Request
    ).
