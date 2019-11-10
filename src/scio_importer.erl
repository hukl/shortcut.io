-module(scio_importer).

-include("include/scio.hrl").
-export([
    extract_bookmarks/1,
    randomize_dates/1,
    fetch_screenshots/1
]).

-spec extract_bookmarks( list() ) -> [ #shortcut{} | 'error' ].
extract_bookmarks(Path) ->
    {ok, File}  = file:read_file(Path),
    Result      = html_parser:parse(File),
    {ok, Links} = html_parser:tags_by_name(<<"A">>, Result),

    MapFun = fun
        ({tag, <<"A">>, [{text, Title}], [{<<"HREF">>, [Url]}]}) ->
            io:format("TITLE ~p~n", [Title]),
            Params = #{
                <<"title">> => Title,
                <<"url">> => Url,
                <<"user_id">> => 1,
                <<"description">> => <<"">>,
                <<"tags">> => []
             },

            case scio_shortcut:create(Params) of
                {ok, Shortcut} ->
                    Shortcut;
                Error ->
                    io:format("NOMATCH ~p~n", [Error]),
                    error
            end
    end,

    lists:map(MapFun, Links).


-spec randomize_dates([ #shortcut{} ]) -> [ #shortcut{} ].
randomize_dates(Shortcuts) ->
    Query = "UPDATE shortcuts "
            "SET (created_at, updated_at) = ($1, $2) "
            "WHERE id = $3;",

    RandFun = fun
        (#shortcut{ id = Id }) ->
            Date = random_date(),

            case scio_sql:equery(pg, Query, [Date, Date, Id]) of
            {ok, _Count, _Colums, [_Row]} ->
                io:format("ok");
            Error ->
                io:format("Error: ~p~n~n", [Error])
            end;
        (_) -> noop
    end,

    lists:foreach(RandFun, Shortcuts),
    Shortcuts.


random_date() ->
    A = 1565447400,
    B = 1573396200,
    M = 1569421800,
    S = 7948800,

    [TimeInSecondsFloat] = rstats:rtruncnorm(1, A, B, M, S),
    TimeInSeconds        = erlang:floor(TimeInSecondsFloat),

    BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds  = BaseDate + TimeInSeconds,
    calendar:gregorian_seconds_to_datetime(Seconds).


fetch_screenshots(Shortcuts) ->
    ScreenshotFun = fun
        (error) ->
            noop;
        (Shortcut) ->
            spawn(scio_screenshot, fetch_url, [Shortcut]),
            timer:sleep(800)
    end,

    lists:foreach(ScreenshotFun, Shortcuts).
