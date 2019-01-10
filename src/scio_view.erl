-module(scio_view).

-compile(export_all).

-define(hello_world, bbmustache:parse_file(view_path("hello_world"))).



template_to_module(TemplateName) ->
    % Define module
    ModName = erlang:list_to_atom(TemplateName),
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

    % Parse the actual template
    Template = bbmustache:parse_file(view_path("hello_world")),

    % Define render function body and pass in the template tuple converted to
    % abstract representation
    Body = erl_syntax:application(
        erl_syntax:module_qualifier(erl_syntax:atom(bbmustache),
        erl_syntax:atom(compile)),
        [erl_syntax:abstract(Template), Var]
    ),

    % Define the only clause of the function
    Clause =  erl_syntax:clause([Var], [], [Body]),

    % Define the function body
    Function     = erl_syntax:function(erl_syntax:atom(render), [Clause]),
    FunctionForm = erl_syntax:revert(Function),

    % Compile and load it into runtime
    {ok, Mod, Bin} = compile:forms([ModForm, ExportForm, FunctionForm]),
    code:load_binary(Mod, [], Bin).

view_path(Name) ->
    code:lib_dir(scio) ++ "/src/views/" ++ Name ++ ".mustache".


render(View, Content) ->
    bbmustache:compile(View, Content).
