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

    gen_server:cast(self(), connect),
    {ok, #state{ args = Args}}.


handle_call({query, Term}, _From, State) ->
    Query = <<"QUERY shortcuts default \"", Term/binary, "\" LIMIT(40)\n" >>,
    io:format("QUERY: ~p~n", [Query]),
    ok = gen_tcp:send(
        State#state.connection,
        Query
    ),

    {ok, <<"PENDING", QuertId/binary>>} = gen_tcp:recv(State#state.connection, 0),
    io:format("QUERY ID: ~p~n", [QuertId]),

    {ok, <<"EVENT", Result/binary>>} = gen_tcp:recv(State#state.connection, 0),
    io:format("RESULT: ~p~n", [Result]),
    {reply, ok, State}.


handle_cast(connect, State) ->
    Args = State#state.args,
    Host = proplists:get_value(hostname, Args),
    Port = proplists:get_value(port, Args),

    {ok, Connection} = connect(Host, Port),

    case gen_tcp:recv(Connection, 0) of
        {ok, <<"CONNECTED ", _/binary>>} ->
            io:format("CONNECTED ~n"),
            gen_server:cast(self(), establish_channel),
            {noreply, State#state{ connection = Connection }};
        WTF ->
            io:format("WTF ~p~n", [WTF]),
            {stop, WTF, State}
    end;


handle_cast(establish_channel, State) ->
    Args        = State#state.args,
    Password    = proplists:get_value(password, Args),
    PasswordBin = erlang:list_to_bitstring(Password),

    ok = gen_tcp:send(
        State#state.connection,
        <<"START search ", PasswordBin/binary, "\n">>
    ),

    case gen_tcp:recv(State#state.connection, 0) of
        {ok, <<"STARTED search", _/binary>>} ->
            io:format("ESTABLISHED ~n"),
            {noreply, State};
        WTF ->
            io:format("WTF ~p~n", [WTF]),
            {stop, WTF, State}
    end;



    %{ok, Response} = gen_tcp:recv(Connection, 0, 1000),


handle_cast(_Msg, State) ->
    {stop, normal, State}.


handle_info({tcp, _Port, <<"PENDING ", _/binary>>}, State) ->
    {noreply, State};

handle_info({tcp, _Port, <<"CONNECTED ", _/binary>>}, State) ->

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


connect(Host, Port) ->
    case gen_tcp:connect(Host, Port, [{active, false}, {packet, line}, binary], 1000) of
        {ok, Connection} ->
            {ok, Connection};
        {error, Reason} ->
            io:format("Connection Failed ~p~n", [Reason]),
            {error, Reason}
    end.

