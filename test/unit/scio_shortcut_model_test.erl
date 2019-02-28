-module(scio_shortcut_model_test).


-compile(export_all).

-include("scio.hrl").

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").

before_suite() ->
    application:ensure_all_started(scio).

before_test() ->
   scio_sql:flush_db().

after_suite() ->
    application:stop(scio).


test_creating_a_shortcut() ->
    Params = #{
        <<"url">>           => <<"https://lsf.htw-berlin.de">>,
        <<"title">>         => <<"Vorlesungsverzeichnis">>,
        <<"description">>   => <<"Vorlesungsverzeichnis der HTW">>,
        <<"user_id">>       => 1
    },

    {ok, Shortcut} = scio_shortcut:create(Params),
    ?assert_equal({ok, 1},                             scio_shortcut:count()),
    ?assert_equal(<<"https://lsf.htw-berlin.de">>,     Shortcut#shortcut.url),
    ?assert_equal(<<"Vorlesungsverzeichnis">>,         Shortcut#shortcut.title),
    ?assert_equal(<<"Vorlesungsverzeichnis der HTW">>, Shortcut#shortcut.description),
    ?assert_equal(1,                                   Shortcut#shortcut.user_id).