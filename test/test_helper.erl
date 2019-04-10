-module(test_helper).


-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").
% etest_http macros
-include_lib ("etest_http/include/etest_http.hrl").

-define(BASE_URL, "http://localhost:9090").

% Helper for creating new users
create_user() ->
    Url     = ?BASE_URL ++ "/users/",
    Headers = [{"content-type", "application/json"}],
    Params  = #{
        <<"username">> => <<"Peter">>,
        <<"email">>    => <<"foo@bar.com">>,
        <<"password">> => <<"dreimalraten">>
    },
    Json = jiffy:encode(Params),

    ?perform_post(Url, Headers, Json, []).


log_in_user() ->
    create_user(),

    Url     = ?BASE_URL ++ "/sessions",
    Headers = [{"content-type", "application/json"}],
    Params  = #{
        <<"email">>    => <<"foo@bar.com">>,
        <<"password">> => <<"dreimalraten">>
    },

    Json = jiffy:encode(Params),

    ?perform_post(Url, Headers, Json, []).


create_shortcut_fixtures(UserId) ->
    create_shortcut_fixtures(UserId, #{}).

create_shortcut_fixtures(UserId, OptionalParams) ->
    Url = "http://foo.com/" ++ erlang:integer_to_list(rand:uniform(1000000)),

    Params = #{
        <<"url">>         => erlang:list_to_binary(Url),
        <<"title">>       => <<"foo">>,
        <<"description">> => <<"bar">>,
        <<"user_id">>     => UserId,
        <<"tags">>        => []
    },

    NewParams = maps:merge(Params, OptionalParams),

    scio_shortcut:create(NewParams).