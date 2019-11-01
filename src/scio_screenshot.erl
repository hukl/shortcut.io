-module(scio_screenshot).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO This needs to be pooled/queued or otherwise prevented from being mass spawned
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([fetch_url/1]).

-include("scio.hrl").

-spec fetch_url(#shortcut{}) -> any().
fetch_url(Shortcut) ->
    Url                  = erlang:binary_to_list(Shortcut#shortcut.url),
    Uuid                 = erlang:binary_to_list(Shortcut#shortcut.screenshot_id),
    {ok, ScreenshotPath} = application:get_env(scio, screenshot_path),

    ImageDirPath = lists:join("/", [
        ScreenshotPath,
        string:substr(Uuid, 1, 4),
        string:substr(Uuid, 5, 4),
        ""
    ]),

    ImagePath = lists:join("/", [ImageDirPath, Uuid]),

    ok = filelib:ensure_dir(ImageDirPath),

    Operations = [
        fun() -> fetch_cmd(Url, ImagePath) end,
        fun() -> convert_cmd(ImagePath, "500x500") end,
        fun() -> convert_cmd(ImagePath, "300x300") end
    ],

    case scio_config:env() of
        "test" -> ok;
        _      -> process_url(Operations)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec fetch_cmd(string(), string()) -> 'ok'.
fetch_cmd(Url, Path) ->
    {ok, PuppeteerFile} = application:get_env(scio, puppeteer_file),
    PuppeteerPath       = lists:join("/", [code:priv_dir(scio), "screenshot", PuppeteerFile]),

    Command = lists:join(" ", [
        "/usr/local/bin/node",
        PuppeteerPath,
        Path,
        Url
    ]),

    Result = os:cmd(Command),
    logger:debug("Get Screenshot Output: ~p~n", [Result]),
    ok.


-spec convert_cmd(string(), string()) -> 'ok'.
convert_cmd(Path, Size) ->
    Command = lists:join(" ", [
        "/usr/local/bin/convert",
        Path ++ ".jpg",
        "-resize",
        Size,
        Path ++ "_" ++ string:substr(Size, 1, 3) ++ ".jpg"
    ]),

    io:format("CONVERT ~p~n", [Command]),

    Result = os:cmd(Command),
    logger:debug("Resize Screenshot output: ~p~n", [Result]),
    ok.


process_url([]) ->
    ok;

process_url([Operation|Rest]) ->
    try
        Operation()
    catch
        Error:Message ->
            io:format("ERROR ~p: ~p~n", [Error, Message]),
            logger:error("~p: ~p", [Error, Message])
    end,

    process_url(Rest).
