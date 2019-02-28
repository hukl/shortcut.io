-module(shortcut_integration_test).


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


test_creating_a_bookmark() ->
    #etest_http_res{ headers = LoginHeaders} = test_helper:log_in_user(),

    Cookie  = proplists:get_value("set-cookie", LoginHeaders),
    Url     = ?BASE_URL ++ "/shortcuts",
    Headers = [
        {"content-type", "application/json"},
        {"cookie",       Cookie}
    ],
    Params  = #{
        <<"url">>         => <<"https://lsf.htw-berlin.de">>,
        <<"title">>       => <<"Vorlesungsverzeichnis">>,
        <<"description">> => <<"Vorlesungsverzeichnis der HTW">>
    },

    Json = jiffy:encode(Params),

    ?assert_equal({ok, 0}, scio_shortcut:count()),

    Res  = ?perform_post(Url, Headers, Json, []),

    ?assert_status(201, Res),
    ?assert_equal({ok, 1}, scio_shortcut:count()).


test_creating_a_bookmark_should_fail_for_logged_out_users() ->
    Url     = ?BASE_URL ++ "/shortcuts",
    Headers = [
        {"content-type", "application/json"}
    ],
    Params  = #{
        <<"url">>         => <<"https://lsf.htw-berlin.de">>,
        <<"title">>       => <<"Vorlesungsverzeichnis">>,
        <<"description">> => <<"Vorlesungsverzeichnis der HTW">>
    },

    Json = jiffy:encode(Params),

    ?assert_equal({ok, 0}, scio_shortcut:count()),

    Res  = ?perform_post(Url, Headers, Json, []),

    ?assert_equal({ok, 0}, scio_shortcut:count()),

    ?assert_status(403, Res).


test_creating_a_bookmark_with_empty_params_should_fail() ->
    #etest_http_res{ headers = LoginHeaders} = test_helper:log_in_user(),

    Cookie  = proplists:get_value("set-cookie", LoginHeaders),
    Url     = ?BASE_URL ++ "/shortcuts",
    Headers = [
        {"content-type", "application/json"},
        {"cookie",       Cookie}
    ],
    Params  = #{
        <<"url">>         => <<"">>,
        <<"title">>       => <<"">>,
        <<"description">> => <<"">>
    },

    Json = jiffy:encode(Params),

    ?assert_equal({ok, 0}, scio_shortcut:count()),

    Res1  = ?perform_post(Url, Headers, Json, []),

    ?assert_status(201, Res1),
    ?assert_equal({ok, 1}, scio_shortcut:count()),

    Res2  = ?perform_post(Url, Headers, Json, []),

    ?assert_status(400, Res2),
    ?assert_equal({ok, 1}, scio_shortcut:count()).
