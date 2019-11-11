-module(scio_search_handler).


-behaviour(gen_server).
-behaviour(poolboy_worker).

% Managment API
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([ingest/2, query/2]).

-include("include/scio.hrl").

% Example state record
-record(state, { connection, args }).

%% ===================================================================
%% Management API
%% ===================================================================

start_link(Args) ->
    gen_server:start_link( ?MODULE, Args, []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init(Args) ->
    process_flag(trap_exit, true),
    Host     = proplists:get_value(hostname, Args),
    Port     = proplists:get_value(port, Args),

    {ok, Connection} = gen_tcp:connect(Host, Port, [{active, true}, binary], 1000),

    {ok, #state{ connection = Connection, args = Args}}.


handle_call({query, Term}, _From, State) ->
    Query = <<"QUERY shortcuts default \"", Term/binary, "\" LIMIT(40)\n" >>,
    io:format("QUERY: ~p~n", [Query]),
    ok = gen_tcp:send(
        State#state.connection,
        Query
    ),
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {stop, normal, State}.


handle_info({tcp, _Port, <<"CONNECTED ", _/binary>>}, State) ->
    Password    = proplists:get_value(password, State#state.args),
    PasswordBin = erlang:list_to_bitstring(Password),
    gen_tcp:send(
        State#state.connection,
        <<"START search ", PasswordBin/binary, "\n">>
    ),
    {noreply, State};



handle_info(Info, State) ->
    io:format("INFO: ~p~n", [Info]),
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    whatever.

%% ===================================================================
%% Public API
%% ===================================================================

ingest(Bucket, Document) ->
    ok.

query(Pid, Term) ->
    gen_server:call(Pid, {query, Term}).


%% ===================================================================
%% Private API
%% ===================================================================

