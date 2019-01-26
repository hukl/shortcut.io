-module(scio_session_store).


-behaviour(gen_server).

% Managment API
-export([start/0, start_link/0, stop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([create/1, login/4, remove/1, list/0, find_online_users/1]).

-include("scio.hrl").

% Example state record
% -record(state, { data = {} }).

%% ===================================================================
%% Management API
%% ===================================================================

start() ->
    gen_server:start( {local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link( {local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ets:new(users, [set, named_table, public]),
    {ok, {}}.


handle_call({}, _From, State) ->
    {reply, ok, State}.

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

create(_Userid) ->
    {ok, {}}.

list() ->
    Result = ets:match(users, {'$1', '$2', '_'}),
    [{Uuid, Name} || [Uuid, Name] <- Result].

login(UserUUID, Username, Token, Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {login, UserUUID, Username, Token, Pid}).

remove(Username) ->
    ok = gen_server:call(?MODULE, {remove, Username}).

find_online_users(Users) ->
    Result = ets:select(users, [{{Key,'_', '_'},[],['$_']} || Key <- Users]),
    [{User, Pid} || {User, _, Pid} <- Result].


%% ===================================================================
%% Private API
%% ===================================================================

% is_user_logged_in(UserUUID) ->
%     case ets:lookup(users, UserUUID) of
%         []                 -> false;
%         [{UserUUID, _, _}] -> true
%     end.


