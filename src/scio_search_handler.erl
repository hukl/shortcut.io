-module(scio_search_handler).


-behaviour(gen_server).

% Managment API
-export([start/1, start_link/1, stop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([ingest/2, search/2]).

-include("include/scio.hrl").

% Example state record
% -record(state, { data = {} }).

%% ===================================================================
%% Management API
%% ===================================================================

start(Args) ->
    gen_server:start( ?MODULE, [Args], []).

start_link(Args) ->
    gen_server:start_link( ?MODULE, [Args], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Args]) ->
    io:format("ARGS ~p~n", [Args]),
    {ok, {}}.


handle_call(_Message, _From, State) ->
    {reply, ok, State}.

handle_cast({connect, #{host := Host, port := Port, password := Password}}, State) ->
    {ok, Connection} = gen_tcp:connect(Host, Port, [{active, true}, binary], 1000),
    io:format("Connection ~p~n", [Connection]),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    whatever.

%% ===================================================================
%% Public API
%% ===================================================================

ingest(Bucket, Document) ->
    ok.

search(Bucket, Document) ->
    ok.


%% ===================================================================
%% Private API
%% ===================================================================

