-module(scio_config).

-export([config_dir/0, db/0, env/0]).


config_dir() ->
    code:lib_dir(scio, config).


env() ->
    {ok,[[Path]]} = init:get_argument(config),
    {ok, RegExp}  = re:compile("config\/(\\w+).config$"),
    {match,[Env]} = re:run(Path, RegExp, [{capture, all_but_first, binary}]),
    erlang:binary_to_list(Env).


db() ->
    EnvAtom = erlang:list_to_atom(env()),
    {ok, [Config]} = file:consult(code:priv_dir(scio) ++ "/database.config"),

    EnvConfig = proplists:get_value(EnvAtom, Config),

    {ok, EnvConfig}.
