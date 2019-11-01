-module(scio_shortcut).

-export([
    count/0,
    create/1,
    find_all_by_user_id/1,
    find/2,
    update/3,
    to_map/1
]).

-include("scio.hrl").

-define(
    SHORTCUT_COLUMNS,
    "id, "
    "url, "
    "title, "
    "description, "
    "screenshot_id, "
    "user_id, "
    "tags, "
    "EXTRACT(EPOCH FROM created_at) as created_at, "
    "EXTRACT(EPOCH FROM created_at) as updated_at "
).


%% ===================================================================
%% Public API
%% ===================================================================


-spec create(#{ bitstring() => bitstring() | integer() }) -> {'ok', #shortcut{}} | {'error', term()}.
create(#{
    <<"url">>           := Url,
    <<"title">>         := Title,
    <<"description">>   := Description,
    <<"user_id">>       := UserId,
    <<"tags">>          := Tags} = _Params) ->

    Query = "INSERT INTO shortcuts "
             "    (url, title, description, screenshot_id, user_id, tags) "
             "VALUES  "
             "    ($1, $2, $3, $4, $5, $6) "
             "RETURNING " ++ ?SHORTCUT_COLUMNS,

    JsonTags     = jiffy:encode(Tags),
    ScreenShotId = uuid:to_string(uuid:uuid4()),

    case scio_sql:equery(pg, Query, [Url, Title, Description, ScreenShotId, UserId, JsonTags]) of
        {ok, _Count, _Colums, [Row]}->
            {ok, row_to_record(Row)};
        {error, Error} ->
            {error, Error}
    end.


-spec find_all_by_user_id(integer()) -> {'ok', []} | {'ok', [#shortcut{}]}.
find_all_by_user_id(UserId) ->
    Query = "SELECT " ++ ?SHORTCUT_COLUMNS
            "FROM shortcuts "
            "WHERE user_id = $1 "
            "ORDER BY created_at DESC "
            "LIMIT 25;",

    {ok, _Colums, Rows} = scio_sql:equery(pg, Query, [UserId]),

    MapFun = fun(Row) ->
        row_to_record(Row)
    end,

    {ok, lists:map(MapFun, Rows)}.


find(ShortcutId, UserId) ->
    Query = "SELECT " ++ ?SHORTCUT_COLUMNS
            "FROM shortcuts "
            "WHERE user_id = $1 "
            "AND id = $2;",

    {ok, _Colums, [Row]} = scio_sql:equery(pg, Query, [UserId, ShortcutId]),

    {ok, row_to_record(Row)}.


-spec update(integer(), integer(), map()) -> {ok, integer()}.
update(ShortcutId, UserId, #{<<"url">> := Url, <<"title">> := Title, <<"description">> := Description, <<"tags">> := Tags}) ->
    Query = "UPDATE shortcuts "
            "SET (url, title, description, tags) = ($1, $2, $3, $4) "
            "WHERE id = $5 "
            "AND user_id = $6 "
            "RETURNING " ++ ?SHORTCUT_COLUMNS,

    JsonTags = jiffy:encode(Tags),

    case scio_sql:equery(pg, Query, [Url, Title, Description, JsonTags, ShortcutId, UserId]) of
        {ok, _Count, _Colums, [Row]} ->
            {ok, row_to_record(Row)};
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


-spec to_map(#shortcut{}) -> map().
to_map(Shortcut) ->
    #{
        <<"id">>                => Shortcut#shortcut.id,
        <<"url">>               => Shortcut#shortcut.url,
        <<"title">>             => Shortcut#shortcut.title,
        <<"description">>       => Shortcut#shortcut.description,
        <<"screenshot_id">>     => Shortcut#shortcut.screenshot_id,
        <<"tags">>              => Shortcut#shortcut.tags,
        <<"created_at">>        => Shortcut#shortcut.created_at,
        <<"updated_at">>        => Shortcut#shortcut.updated_at
    }.

%% ===================================================================
%% Private API
%% ===================================================================


row_to_record({Id, Url, Title, Description, ScreenshotId, UId, Tags, CreatedAt, UpdatedAt}) ->
    #shortcut{
        id              = Id,
        url             = Url,
        title           = Title,
        description     = Description,
        screenshot_id   = ScreenshotId,
        user_id         = UId,
        tags            = jiffy:decode(Tags, [return_maps]),
        created_at      = erlang:trunc(CreatedAt),
        updated_at      = erlang:trunc(UpdatedAt)
    }.
