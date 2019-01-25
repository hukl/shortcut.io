-module(scio_user_model_test).


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



test_creating_a_user() ->
    Password = <<"SicherSicher23">>,

    Params = #{
        username => <<"Peter">>,
        email    => <<"foo@bar.com">>,
        password => Password
    },

    {ok, User} = scio_user:create(Params),

    ?assert_equal({ok, 1},              scio_user:count()),
    ?assert_equal(<<"Peter">>,          User#user.username),
    ?assert_equal(<<"foo@bar.com">>,    User#user.email),

    ?assert_not_equal(Password, User#user.password),

    ?assert(scio_utils:check_password(Password, User#user.password)).
