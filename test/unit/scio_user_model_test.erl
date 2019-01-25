-module(scio_user_model_test).


-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").

before_suite() ->
    application:ensure_all_started(scio).

before_test() ->
   scio_sql:flush_db().

after_suite() ->
    application:stop(scio).



test_creating_a_user() ->
    Params = #{
        username => <<"Peter">>,
        email    => <<"foo@bar.com">>,
        password => <<"SicherSicher23">>
    },

    {ok, User} = scio_user:create(Params),

    ?assert_equal({ok, 1}, scio_user:count()).
