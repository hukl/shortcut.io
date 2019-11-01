-module(scio_default_handler).

-export([init/2, error_response/3]).

-include("scio.hrl").

-define(PATH_SEPARATOR, binary:compile_pattern(<<"/">>)).
-define(PATH_OPTIONS,   [trim_all, global]).

init(#{ method := Method, path := Path} = Request, State) ->
    PathSegments = binary:split(Path, ?PATH_SEPARATOR, ?PATH_OPTIONS),
    logger:error("Method: ~p Path: ~p~n", [Method, PathSegments]),

    Session = nested:get([bindings, session], Request, undefined),

    {ok, Status, Headers, Body, NewRequest} = handle_request(
        Method, PathSegments, Request, Session
    ),

    Response = render_response(Status, Headers, Body, NewRequest),

    {ok, Response, State}.


-spec error_response(integer(), bitstring(), cowboy_req:req()) -> cowboy_req:req().
error_response(Status, Message, Request) ->
    ResponseBody = #{
      <<"ok">>    => <<"false">>,
      <<"error">> => Message
    },

    Res = cowboy_req:reply(
        Status,
        #{},
        jiffy:encode(ResponseBody),
        Request
    ),

    Res.


% ##############################################################################
% # Request Handlers                                                           #
% ##############################################################################

-spec handle_request(
    bitstring(),
    list(),
    cowboy:req(),
    #session{} | 'undefined') -> {'ok', integer(), map(), bitstring(), cowboy:req()}.


handle_request(Method, [<<"users">>|Path], Request, Session) ->
    scio_users_handler:handle_request(Method, Path, Request, Session);

handle_request(Method, [<<"sessions">>|Path], Request, Session) ->
    scio_session_handler:handle_request(Method, Path, Request, Session);

handle_request(Method, [<<"shortcuts">>|Path], Request, Session) ->
    scio_shortcut_handler:handle_request(Method, Path, Request, Session);


handle_request(<<"GET">>, [<<"health">>], Request, _) ->
    {ok, 200, #{<<"content-type">> => <<"text/plain">>}, <<"OK">>, Request};

handle_request(_, _, Request, _) ->
    error_response(400, <<"Not Found">>, Request).

% ##############################################################################
% # Internal API                                                               #
% ##############################################################################

render_response(Status, Headers, Body, Request) ->
    DefaultHeader = #{<<"content-type">> => <<"application/json">>},
    NewHeaders    = maps:merge(DefaultHeader, Headers),

    cowboy_req:reply(
        Status,
        NewHeaders,
        Body,
        Request
    ).
