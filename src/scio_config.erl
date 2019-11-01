-module(scio_config).

-export([config_dir/0, env/0]).


config_dir() ->
    code:lib_dir(scio, config).


env() ->
    {ok, Env} = application:get_env(scio, env),
    Env.
