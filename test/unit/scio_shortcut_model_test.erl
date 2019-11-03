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
        <<"user_id">>       => 1,
        <<"tags">>          => [<<"Uni">>, <<"htw">>]
    },

    {ok, Shortcut} = scio_shortcut:create(Params),
    ?assert_equal({ok, 1},                             scio_shortcut:count()),
    ?assert_equal(<<"https://lsf.htw-berlin.de">>,     Shortcut#shortcut.url),
    ?assert_equal(<<"Vorlesungsverzeichnis">>,         Shortcut#shortcut.title),
    ?assert_equal(<<"Vorlesungsverzeichnis der HTW">>, Shortcut#shortcut.description),
    ?assert_equal(1,                                   Shortcut#shortcut.user_id),
    ?assert_equal([<<"Uni">>, <<"htw">>],              Shortcut#shortcut.tags).


test_find_by_user_id_with_no_data_in_db() ->
    ?assert_equal({ok, []}, scio_shortcut:find_all_by_user_id(1)).

test_find_by_user_id_with_data_in_db() ->
    test_helper:create_shortcut_fixtures(1),
    {ok, [#shortcut{} = _Shortcut]} = scio_shortcut:find_all_by_user_id(1).

test_find() ->
    test_helper:create_shortcut_fixtures(1),
    {ok, #shortcut{}} = scio_shortcut:find(1, 1).

test_update_shortcut() ->
    {ok, Shortcut} = test_helper:create_shortcut_fixtures(1),
    Params  = #{
        <<"url">>         => <<"http://bar.com">>,
        <<"title">>       => <<"Neuer Titel">>,
        <<"description">> => <<"Neue Beschreibung">>,
        <<"tags">>        => [<<"foo">>, <<"bar">>, <<"baz">>]
    },
    {ok, UpdatedShortcut} = scio_shortcut:update(1, 1, Params),

    ?assert_not_equal(Shortcut, UpdatedShortcut),
    ?assert_equal(<<"http://bar.com">>,              UpdatedShortcut#shortcut.url),
    ?assert_equal(<<"Neuer Titel">>,                 UpdatedShortcut#shortcut.title),
    ?assert_equal(<<"Neue Beschreibung">>,           UpdatedShortcut#shortcut.description),
    ?assert_equal(1,                                 UpdatedShortcut#shortcut.user_id),
    ?assert_equal([<<"foo">>, <<"bar">>, <<"baz">>], UpdatedShortcut#shortcut.tags).


test_delete_shortcut() ->
    {ok, Shortcut}  = test_helper:create_shortcut_fixtures(75),
    ShortcutId      = Shortcut#shortcut.id,

    ?assert_equal({ok, 1}, scio_shortcut:count()),

    {ok, ShortcutId} = scio_shortcut:delete(ShortcutId, 75),

    ?assert_equal({ok, 0}, scio_shortcut:count()).
