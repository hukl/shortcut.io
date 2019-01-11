-module(default_handler_test).


-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").
% etest_http macros
-include_lib ("etest_http/include/etest_http.hrl").

before_suite() ->
    application:ensure_all_started(scio).

after_suite() ->
    application:stop(scio).


test_application_is_starting() ->
    Apps     = application:which_applications(),
    AppNames = [Name || {Name, _, _} <- Apps],

    TestFun = fun(AppName) ->
        lists:member(AppName, AppNames)
    end,

    ExpectedApps = [
        scio
    ],

    ?assert(lists:all(TestFun, ExpectedApps)).


test_health_check() ->
    Url = "http://localhost:9090/health",
    Res = ?perform_get(Url),

    ?assert_status(200, Res).


test_index_page() ->
    Url = "http://localhost:9090/",
    Res = ?perform_get(Url),

    ?assert_status(200, Res).
