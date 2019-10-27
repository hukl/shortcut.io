%%%-------------------------------------------------------------------
%% @doc scio top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(scio_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Children = [
        ?CHILD(scio_session_store, worker),
        pg_poolboy_spec()
    ],

    {ok, {{one_for_all, 0, 1}, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================


pg_poolboy_spec() ->
    {ok, Pools} = application:get_env(scio, pg_pools),
    [DefaultSpec|_] = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, scio_sql_worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),

    DefaultSpec.
