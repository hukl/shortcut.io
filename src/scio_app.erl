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


    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", scio_default_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),

    scio_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    cowboy:stop_listener(http),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
