-module(scio_default_handler).

-export([init/2]).

-define(PATH_SEPARATOR, binary:compile_pattern(<<"/">>)).
-define(PATH_OPTIONS,   [trim_all, global]).

init(#{ method := Method, path := Path} = Request, State) ->
    PathSegments = binary:split(Path, ?PATH_SEPARATOR, ?PATH_OPTIONS),
    logger:error("Method: ~p Path: ~p~n", [Method, PathSegments]),

    {ok, Status, Headers, Body} = handle_request(Method, PathSegments, Request),

    Response = render_html(Status, Headers, Body, Request),

    {ok, Response, State}.


% ##############################################################################
% # Request Handlers                                                           #
% ##############################################################################

handle_request(<<"GET">>, [] ,_Request) ->
    Body = landing_page_view:render(
        #{
             <<"greeting">> => <<"hello world">>,
             <<"names">>    => [
                #{<<"name">> => <<"alice">>},
                #{<<"name">> => <<"bob">>}
            ]
        }
    ),

    {ok, 200, #{}, Body};


handle_request(Method, [<<"users">>|Path], Request) ->
    scio_users_handler:handle_request(Method, Path, Request);


handle_request(<<"GET">>, [<<"health">>], _Request) ->
    {ok, 200, #{}, <<"OK">>};

handle_request(_, _, Request) ->
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

render_html(Status, Headers, Body, Request) ->
    Html = layout_view:render(
        #{<<"body">> => Body}
    ),

    DefaultHeader = #{<<"content-type">> => <<"text/html">>},
    NewHeaders    = maps:merge(DefaultHeader, Headers),

    cowboy_req:reply(
        Status,
        NewHeaders,
        Html,
        Request
    ).
