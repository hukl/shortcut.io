-module(scio_default_handler).

-export([init/2]).

-define(PATH_SEPARATOR, binary:compile_pattern(<<"/">>)).
-define(PATH_OPTIONS,   [trim_all, global]).

init(#{ method := Method, path := Path} = Request, State) ->
    PathSegments = binary:split(Path, ?PATH_SEPARATOR, ?PATH_OPTIONS),
    logger:error("Method: ~p Path: ~p~n", [Method, PathSegments]),

    {ok, Status, Headers, Body, NewRequest} = handle_request(
        Method, PathSegments, Request
    ),

    Response = render_html(Status, Headers, Body, NewRequest),

    {ok, Response, State}.


% ##############################################################################
% # Request Handlers                                                           #
% ##############################################################################

-spec handle_request(
    bitstring(),
    list(),
    cowboy:req()) -> {'ok', integer(), map(), bitstring(), cowboy:req()}.

handle_request(<<"GET">>, [] , Request) ->
    Body = landing_page_view:render(
        #{
             <<"greeting">> => <<"hello world">>,
             <<"names">>    => [
                #{<<"name">> => <<"alice">>},
                #{<<"name">> => <<"bob">>}
            ]
        }
    ),

    {ok, 200, #{}, Body, Request};


handle_request(Method, [<<"users">>|Path], Request) ->
    scio_users_handler:handle_request(Method, Path, Request);

handle_request(Method, [<<"sessions">>|Path], Request) ->
    scio_session_handler:handle_request(Method, Path, Request);

handle_request(Method, [<<"shortcuts">>|Path], Request) ->
    scio_shortcut_handler:handle_request(Method, Path, Request);


handle_request(<<"GET">>, [<<"health">>], Request) ->
    {ok, 200, #{}, <<"OK">>, Request};

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


render_html(Status, #{<<"content-type">> := <<"application/json">>} = Headers, Body, Request) ->

    cowboy_req:reply(
        Status,
        Headers,
        Body,
        Request
    );


render_html(Status, Headers, Body, Request) ->
    Session = case cowboy_req:binding(session, Request) of
        undefined -> <<"Not logged in">>;
        _         -> <<"You're logged in!">>
    end,

    Html = layout_view:render(
        #{
             <<"body">>    => Body,
             <<"session">> => Session
         }
    ),

    DefaultHeader = #{<<"content-type">> => <<"text/html">>},
    NewHeaders    = maps:merge(DefaultHeader, Headers),

    cowboy_req:reply(
        Status,
        NewHeaders,
        Html,
        Request
    ).
