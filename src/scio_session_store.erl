-module(scio_session_store).


-behaviour(gen_server).

% Managment API
-export([start/0, start_link/0, stop/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([save/1, find/1, validate/1, login/4, remove/1, list/0, find_online_users/1]).

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
    ets:new(sessions, [set, named_table, public]),
    {ok, {}}.


handle_call({save, Session}, _From, State) ->
    #session{
       session_id = SessionId,
       user_id    = UserId,
       created_at = CreatedAt
    } = Session,

    true = ets:insert_new(sessions, {SessionId, UserId, CreatedAt}),

    {reply, {ok, Session}, State}.

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

-spec save(#session{}) -> {'ok', #session{}}.
save(Session) ->
    gen_server:call(?MODULE, {save, Session}).



validate_session(Validations, SessionId) ->
    ValidationFun = fun
        % Keep checking while status is ok
        (Check, {ok, _} = SessionTuple) -> Check(SessionTuple);

        % Early exit if one of the validations failed
        (_, {error, Reason})   -> {error, Reason}
    end,

    lists:foldl(
        ValidationFun,
        {ok, #session{ session_id = SessionId }},
        Validations
    ).


-spec validate(bitstring()) ->  { 'ok', #session{} } | { 'error', 'atom' }.
validate(SessionId) ->
    Validations = [
        fun validate_signature/1,
        fun validate_existence/1,
        fun validate_timestamp/1
    ],

    validate_session(Validations, SessionId).

-spec find(bitstring()) -> { 'ok', #session{} } | { 'error', atom() }.
find(SessionId) ->
     case ets:lookup(sessions, SessionId) of
        [] ->
            {error, not_found};
        [{SessionId, UserId, CreatedAt}] ->
            Session = #session{
                session_id = SessionId,
                user_id    = UserId,
                created_at = CreatedAt
            },

            {ok, Session}
    end.

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

validate_signature({ok, #session{ session_id = SessionId }}) ->
    case scio_session:validate_session_id(SessionId) of
        true  -> {ok, SessionId};
        false -> {error, invalid}
    end.


validate_existence({ok, #session{ session_id = SessionId }}) ->
    find(SessionId).


validate_timestamp({ok, #session{ created_at = CreatedAt } = Session }) ->
    Now           = scio_utils:timestamp(),
    MaxSessionAge = CreatedAt + (30 * 24 * 60 * 60),

    case MaxSessionAge < Now of
        true  -> {ok, Session};
        false -> {error, expired}
    end.
