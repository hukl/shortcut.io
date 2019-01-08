%%%-------------------------------------------------------------------
%% @doc scio public API
%% @end
%%%-------------------------------------------------------------------

-module(scio_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    application:start(cowlib),
    application:start(ranch),
    application:ensure_all_started(cowboy),

    initialize_cowboy(),

    scio_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    cowboy:stop_listener(http),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

initialize_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {'_', scio_default_handler, []}
        ]}
    ]),

    {ok, Port} = application:get_env(http_port),

    {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }).