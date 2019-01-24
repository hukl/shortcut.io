-module(user_signup_integration_test).


-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").
% etest_http macros
-include_lib ("etest_http/include/etest_http.hrl").

before_suite() ->
    application:ensure_all_started(scio).

after_suite() ->
    application:stop(scio).


test_signup_page() ->
    Url = "http://localhost:9090/users/new",
    Res = ?perform_get(Url),

    ?assert_status(200, Res).


test_submitting_valid_form_should_create_user() ->
    Url     = "http://localhost:9090/users/",
    Body    = <<>>,
    Headers = [],
    Queries = #{
        <<"username">> => <<"Perter">>,
        <<"email">>    => <<"foo@bar.com">>,
        <<"password">> => <<"dreimalraten">>
    },

    Res = ?perform_post(Url, Headers, Body, maps:to_list(Queries)),
    ?assert_header("location", Res),
    ?assert_header_value("location", "http://localhost:9090/", Res),
    ?assert_status(302, Res).
