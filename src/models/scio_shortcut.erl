-module(scio_shortcut).

-export([
    count/0,
    create/1,
    find_all_by_user_id/1,
    find/2,
    update/3
]).

-include("scio.hrl").

-spec create(#{ bitstring() => bitstring() | integer() }) -> {'ok', #shortcut{}} | {'error', term()}.
create(#{
    <<"url">>         := Url,
    <<"title">>       := Title,
    <<"description">> := Description,
    <<"user_id">>     := UserId} = _Params) ->

    Query = "INSERT INTO shortcuts "
             "    (url, title, description, user_id) "
             "VALUES  "
             "    ($1, $2, $3, $4) "
             "RETURNING id, url, title, description, user_id, screenshot_id, created_at, updated_at;",

    case scio_sql:equery(pg, Query, [Url, Title, Description, UserId]) of
        {ok, _Count, _Colums, [{Id, Url, Title, Description, UserId, ScreenshotId, CreatedAt, UpdatedAt}]}->

            Shortcut = #shortcut{
                id              = Id,
                url             = Url,
                title           = Title,
                description     = Description,
                user_id         = UserId,
                screenshot_id   = ScreenshotId,
                created_at      = CreatedAt,
                updated_at      = UpdatedAt
            },

            {ok, Shortcut};
        {error, Error} ->
            {error, Error}
    end.

-spec find_all_by_user_id(integer()) -> {'ok', []} | {'ok', [#shortcut{}]}.
find_all_by_user_id(UserId) ->
    Query = "SELECT "
                "id, "
                "url, "
                "title, "
                "description, "
                "user_id, "
                "screenshot_id, "
                "EXTRACT(EPOCH FROM created_at) * 1000000 as created_at, "
                "EXTRACT(EPOCH FROM created_at) * 1000000 as updated_at  "
            "FROM shortcuts "
            "WHERE user_id = $1 "
            "ORDER BY created_at DESC "
            "LIMIT 25;",

    {ok, _Colums, Rows} = scio_sql:equery(pg, Query, [UserId]),

    MapFun = fun({Id, Url, Title, Description, UId, ScreenshotId, CreatedAt, UpdatedAt}) ->
        #shortcut{
            id              = Id,
            url             = Url,
            title           = Title,
            description     = Description,
            user_id         = UId,
            screenshot_id   = ScreenshotId,
            created_at      = CreatedAt,
            updated_at      = UpdatedAt
        }
    end,

    {ok, lists:map(MapFun, Rows)}.


find(ShortcutId, UserId) ->
    Query = "SELECT "
                "id, "
                "url, "
                "title, "
                "description, "
                "user_id, "
                "screenshot_id, "
                "EXTRACT(EPOCH FROM created_at) * 1000000 as created_at, "
                "EXTRACT(EPOCH FROM created_at) * 1000000 as updated_at  "
            "FROM shortcuts "
            "WHERE user_id = $1 "
            "AND id = $2;",

    {ok, _Colums, [{Id, Url, Title, Description, UId, ScreenshotId, CreatedAt, UpdatedAt}]} = scio_sql:equery(pg, Query, [UserId, ShortcutId]),

    Shortcut = #shortcut{
        id              = Id,
        url             = Url,
        title           = Title,
        description     = Description,
        user_id         = UId,
        screenshot_id   = ScreenshotId,
        created_at      = CreatedAt,
        updated_at      = UpdatedAt
    },

    {ok, Shortcut}.


-spec update(integer(), integer(), map()) -> {ok, integer()}.
update(ShortcutId, UserId, #{<<"url">> := Url, <<"title">> := Title, <<"description">> := Description}) ->
    Query = "UPDATE shortcuts "
            "SET (url, title, description) = ($1, $2, $3) "
            "WHERE id = $4 "
            "AND user_id = $5 "
            "RETURNING id, url, title, description, user_id, screenshot_id, EXTRACT(EPOCH FROM created_at) * 1000000 as created_at, EXTRACT(EPOCH FROM created_at) * 1000000 as updated_at;",

    case scio_sql:equery(pg, Query, [Url, Title, Description, ShortcutId, UserId]) of
        {ok, _Count, _Colums, [{Id, Url, Title, Description, UId, ScreenshotId, CreatedAt, UpdatedAt}]} ->
            Shortcut = #shortcut{
                id              = Id,
                url             = Url,
                title           = Title,
                description     = Description,
                user_id         = UId,
                screenshot_id   = ScreenshotId,
                created_at      = CreatedAt,
                updated_at      = UpdatedAt
            },
            {ok, Shortcut};
        {ok, 0 , _Columns, _Rows} ->
            {error, no_record_found};
        {error, Error} ->
            {error, Error}
    end.


-spec count() -> {'ok', integer()} | {'error', tuple()}.
count() ->
    Query = "SELECT count(id) FROM shortcuts;",

    case scio_sql:equery(pg, Query, []) of
        {ok, _Colums, Rows} ->
            [Count] = [Row || {Row} <- Rows],
            {ok, Count};
        {error, Error} ->
            {error, Error}
    end.
