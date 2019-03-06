-module(shortcut_integration_test).


-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").
% etest_http macros
-include_lib ("etest_http/include/etest_http.hrl").

-include("scio.hrl").

-define(BASE_URL, "http://localhost:9090").

before_suite() ->
    application:ensure_all_started(scio).

before_test() ->
    scio_session_store:flush(),
    scio_sql:flush_db().

after_suite() ->
    application:stop(scio).


test_creating_a_shortcut() ->
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


test_creating_a_shortcut_should_fail_for_logged_out_users() ->
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


test_creating_a_shortcut_with_empty_params_should_fail() ->
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


test_displaying_shortcuts() ->
    #etest_http_res{ headers = LoginHeaders} = test_helper:log_in_user(),

    [test_helper:create_shortcut_fixtures(1) || _ <- lists:seq(1,5)],

    Cookie  = proplists:get_value("set-cookie", LoginHeaders),
    Url     = ?BASE_URL ++ "/shortcuts",
    Headers = [
        {"content-type", "application/json"},
        {"cookie",       Cookie}
    ],

    Res = ?perform_get(Url, Headers),
    ?assert_status(200, Res),

    ResJson = jiffy:decode(Res#etest_http_res.body, [return_maps]),
    ?assert_equal(5, length(ResJson)),
    RespKeys = lists:sort(maps:keys(hd(ResJson))),
    ExpectedKeys = lists:sort([
        <<"id">>,
        <<"url">>,
        <<"title">>,
        <<"description">>,
        <<"screenshot_id">>,
        <<"created_at">>,
        <<"updated_at">>
    ]),
    ?assert_equal(ExpectedKeys, RespKeys).


test_displaying_single_shortcut() ->
    #etest_http_res{ headers = LoginHeaders} = test_helper:log_in_user(),

    test_helper:create_shortcut_fixtures(1),

    Cookie  = proplists:get_value("set-cookie", LoginHeaders),
    Url     = ?BASE_URL ++ "/shortcuts/1",
    Headers = [
        {"content-type", "application/json"},
        {"cookie",       Cookie}
    ],

    Res = ?perform_get(Url, Headers),
    ?assert_status(200, Res),

    ResJson = jiffy:decode(Res#etest_http_res.body, [return_maps]),
    ?assert_equal(7, maps:size(ResJson)),
    RespKeys = maps:keys(ResJson),
    ExpectedKeys = [
        <<"id">>,
        <<"url">>,
        <<"title">>,
        <<"description">>,
        <<"screenshot_id">>,
        <<"created_at">>,
        <<"updated_at">>
    ],
    ?assert_equal(lists:sort(ExpectedKeys), lists:sort(RespKeys)).


test_updating_a_shortcut() ->
    #etest_http_res{ headers = LoginHeaders} = test_helper:log_in_user(),

    test_helper:create_shortcut_fixtures(1),

    Cookie  = proplists:get_value("set-cookie", LoginHeaders),
    Url     = ?BASE_URL ++ "/shortcuts/1",
    Headers = [
        {"content-type", "application/json"},
        {"cookie",       Cookie}
    ],

    Res1 = ?perform_get(Url, Headers),
    ?assert_status(200, Res1),

    Params  = #{
        <<"url">>         => <<"http://bar.com">>,
        <<"title">>       => <<"Neuer Titel">>,
        <<"description">> => <<"Neue Beschreibung">>
    },
    ReqJson = jiffy:encode(Params),
    Res2 = ?perform_put(Url, Headers, ReqJson, []),
    ?assert_status(200, Res2),

    Res3 = ?perform_get(Url, Headers),
    ResJson = jiffy:decode(Res3#etest_http_res.body, [return_maps]),
    ?assert_equal(<<"http://bar.com">>,    maps:get(<<"url">>, ResJson)),
    ?assert_equal(<<"Neuer Titel">>,       maps:get(<<"title">>, ResJson)),
    ?assert_equal(<<"Neue Beschreibung">>, maps:get(<<"description">>, ResJson)).


test_updating_a_shortcut_of_a_different_user_should_fail() ->
    #etest_http_res{ headers = LoginHeaders} = test_helper:log_in_user(),

    {ok, OldShortcut} = test_helper:create_shortcut_fixtures(42),

    Cookie  = proplists:get_value("set-cookie", LoginHeaders),
    Url     = ?BASE_URL ++ "/shortcuts/1",
    Headers = [
        {"content-type", "application/json"},
        {"cookie",       Cookie}
    ],

    Params  = #{
        <<"url">>         => <<"http://bar.com">>,
        <<"title">>       => <<"Neuer Titel">>,
        <<"description">> => <<"Neue Beschreibung">>
    },
    ReqJson = jiffy:encode(Params),
    Res2 = ?perform_put(Url, Headers, ReqJson, []),
    ?assert_status(403, Res2),

    {ok, Shortcut} = scio_shortcut:find(1, 42),
    ?assert_equal(OldShortcut#shortcut.url, Shortcut#shortcut.url),
    ?assert_equal(OldShortcut#shortcut.title, Shortcut#shortcut.title),
    ?assert_equal(OldShortcut#shortcut.description, Shortcut#shortcut.description).
