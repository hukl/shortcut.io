-module(user_integration_test).


-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").
% etest_http macros
-include_lib ("etest_http/include/etest_http.hrl").

-define(BASE_URL, "http://localhost:9090").

before_suite() ->
    application:ensure_all_started(scio).

before_test() ->
    scio_session_store:flush(),
    scio_sql:flush_db().

after_suite() ->
    application:stop(scio).


test_signup_page() ->
    Url = ?BASE_URL ++ "/users/new",
    Res = ?perform_get(Url),

    ?assert_status(200, Res).


test_submitting_valid_form_should_create_user() ->
    Url     = ?BASE_URL ++ "/users/",
    Headers = [{"content-type", "application/json"}],
    Params  = #{
        <<"username">> => <<"Peter">>,
        <<"email">>    => <<"foo@bar.com">>,
        <<"password">> => <<"dreimalraten">>
    },
    Json = jiffy:encode(Params),

    Res = ?perform_post(Url, Headers, Json, []),

    ?assert_header("location", Res),
    ?assert_header_value("location", "/", Res),
    ?assert_status(201, Res),

    ?assert_equal({ok, 1}, scio_user:count()).


test_signing_up_with_existing_email_address() ->
    % create the default user
    test_helper:create_user(),

    % then sign up again which should create an error
    Response = test_helper:create_user(),

    ?assert_status(400, Response),
    ?assert_json_value(<<"ok">>, <<"false">>, Response).


test_signing_up_with_existing_user_name() ->
    % create the default user
    test_helper:create_user(),

    Url     = ?BASE_URL ++ "/users/",
    Headers = [{"content-type", "application/json"}],
    Params  = #{
        <<"username">> => <<"Peter">>,
        <<"email">>    => <<"different@address.com">>,
        <<"password">> => <<"dreimalraten">>
    },
    Json = jiffy:encode(Params),

    Response = ?perform_post(Url, Headers, Json, []),

    ?assert_status(400, Response).


test_log_in_page() ->
    Url = ?BASE_URL ++ "/sessions/new",
    Res = ?perform_get(Url),

    ?assert_status(200, Res).


test_successful_log_in() ->
    test_helper:create_user(),

    Url     = ?BASE_URL ++ "/sessions",
    Headers = [{"content-type", "application/json"}],
    Params  = #{
        <<"email">>    => <<"foo@bar.com">>,
        <<"password">> => <<"dreimalraten">>
    },

    Json = jiffy:encode(Params),

    Res  = ?perform_post(Url, Headers, Json, []),

    ?assert_header("set-cookie", Res),

    ?assert_header("location", Res),
    ?assert_header_value("location", "/", Res),
    ?assert_status(201, Res),
    ?assert_equal({ok, 1}, scio_session_store:count()).


test_unsuccessful_log_in_with_wrong_password() ->
    test_helper:create_user(),

    Url     = ?BASE_URL ++ "/sessions",
    Headers = [{"content-type", "application/json"}],
    Params  = #{
        <<"email">>    => <<"foo@bar.com">>,
        <<"password">> => <<"wrongpassword">>
    },

    Json = jiffy:encode(Params),

    Res  = ?perform_post(Url, Headers, Json, []),

    ?assert_status(403, Res),
    ?assert_equal({ok, 0}, scio_session_store:count()).


test_unsuccessful_log_in_with_wrong_email() ->
    test_helper:create_user(),

    Url     = ?BASE_URL ++ "/sessions",
    Headers = [{"content-type", "application/json"}],
    Params  = #{
        <<"email">>    => <<"foo@wrong.com">>,
        <<"password">> => <<"dreimalraten">>
    },

    Json = jiffy:encode(Params),

    Res  = ?perform_post(Url, Headers, Json, []),

    ?assert_status(403, Res),
    ?assert_equal({ok, 0}, scio_session_store:count()).


test_login_and_stay_logged_in() ->
    test_helper:create_user(),

    Url     = ?BASE_URL ++ "/sessions",
    Headers = [{"content-type", "application/json"}],
    Params  = #{
        <<"email">>    => <<"foo@bar.com">>,
        <<"password">> => <<"dreimalraten">>
    },

    Json = jiffy:encode(Params),

    Res  = ?perform_post(Url, Headers, Json, []),
    ResHeaders = Res#etest_http_res.headers,
    Cookie     = proplists:get_value("set-cookie", ResHeaders),

    UrlNew = ?BASE_URL ++ "/",
    ResNew = ?perform_get(UrlNew, [{"cookie", Cookie}]),
    ?assert_body_contains("You're logged in", ResNew).


test_not_found() ->
    Url = ?BASE_URL ++ "/users/bogus_path",
    Res = ?perform_get(Url),

    ?assert_status(404, Res).
