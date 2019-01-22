-module(scio_view).

-compile(export_all).
-compile(nowarn_export_all).


load_templates() ->
    Files        = view_paths(),
    {ok, RegExp} = re:compile("\\b(\\w+).mustache$"),

    LoadFun = fun(Path) ->
        Match = re:run(Path, RegExp, [{capture, all_but_first, binary}]),

        case Match of
            {match, [Name]} ->
                ViewName = << Name/binary, "_view">>,
                template_to_module(ViewName, Path);
            _ ->
                noop
        end
    end,

    lists:foreach(LoadFun, Files).


-spec template_to_module(binary(), binary()) -> {module, atom()}.
template_to_module(TemplateName, Path) ->
    % Parse the template file
    Template = bbmustache:parse_file(Path),

    % Define module
    ModName = erlang:binary_to_atom(TemplateName, utf8),
    Module  = erl_syntax:attribute(erl_syntax:atom(module),[erl_syntax:atom(ModName)]),
    ModForm = erl_syntax:revert(Module),

    % Define Module Exports
    Export = erl_syntax:attribute(
        erl_syntax:atom(export), [
            erl_syntax:list(
                [
                    erl_syntax:arity_qualifier(
                        erl_syntax:atom(render),
                        erl_syntax:integer(1)
                     )
                ]
            )
        ]),

    ExportForm = erl_syntax:revert(Export),

    % Variable for render function
    Var = erl_syntax:variable("Content"),

    % Convert bbmustache template compile option, to use binary keys,
    % to abstract form
    CompileOption = erl_syntax:abstract([{key_type, binary}]),

    % Define render function body and pass in the template tuple converted to
    % abstract representation
    Body = erl_syntax:application(
        erl_syntax:module_qualifier(erl_syntax:atom(bbmustache),
        erl_syntax:atom(compile)),
        [erl_syntax:abstract(Template), Var, CompileOption]
    ),

    % Define the only clause of the function
    Clause =  erl_syntax:clause([Var], [], [Body]),

    % Define the function body
    Function     = erl_syntax:function(erl_syntax:atom(render), [Clause]),
    FunctionForm = erl_syntax:revert(Function),

    % Compile and load it into runtime
    {ok, Mod, Bin} = compile:forms([ModForm, ExportForm, FunctionForm]),
    code:load_binary(Mod, [], Bin).


-spec view_directory() -> string().
view_directory() ->
    filename:join([
        code:lib_dir(scio),
        "src",
        "views"
    ]).


-spec view_paths() -> [string()].
view_paths() ->
    Wildcard = filename:join([view_directory(), "*.mustache"]),
    filelib:wildcard(Wildcard).

