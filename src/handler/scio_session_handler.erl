-module(scio_session_handler).

-export([handle_request/4]).

-include("scio.hrl").


handle_request(<<"POST">>, [], Request, _) ->
    {ok, Json, _RequestWithBody} = cowboy_req:read_body(Request),

    Params = jiffy:decode(Json, [return_maps]),

    case login(Params, Request) of
        {ok, NewRequest} ->
            {ok, 201, #{<<"location">> => <<"/">>}, <<"Login Success">>, NewRequest};
        {error, _Reason} ->
            scio_default_handler:error_response(403, <<"Forbidden">>, Request)
    end;


handle_request(_, _, Request, _) ->
    scio_default_handler:error_response(404, <<"Not Found">>, Request).



% ##############################################################################
% # Request Helpers                                                           #
% ##############################################################################


find_user({ok, User, Params, Request, Session}) ->
    Email = maps:get(<<"email">>, Params),

    case scio_user:find_by_email(Email) of
        {ok, UserFromDB} ->
            {ok, UserFromDB, Params, Request, Session};
        {error, _} ->
            {error, User, Params, Request, Session}
    end.

validate_password({ok, User, Params, Request, Session}) ->
    Password = maps:get(<<"password">>, Params),

    case scio_user:verify_password(Password, User) of
        true  -> {ok, User, Params, Request, Session};
        false -> {error, User, Params, Request, Session}
    end.


create_session({ok, User, Params, Request, undefined}) ->
    case scio_session:new(User) of
        {ok, Session} ->
            {ok, User, Params, Request, Session};
        {error, _} ->
            {error, User, Params, Request, undefined}
    end.


save_session({ok, User, Params, Request, Session}) ->
    case scio_session_store:save(Session) of
        {ok, Session} ->
            {ok, User, Params, Request, Session};
        {error, _} ->
            {error, User, Params, Request, undefined}
    end.



write_cookie({ok, User, Params, Request, Session}) ->
    RequestWithSession = cowboy_req:set_resp_cookie(
        <<"session">>, Session#session.session_id,
        Request,
        #{path => <<"/">>}
    ),
    {ok, User, Params, RequestWithSession, Session}.




-spec login(map(), cowboy:req()) -> {'ok', cowboy:req()} | {'error', atom()}.
login(Params, Request) ->
    Steps = [
        fun find_user/1,
        fun validate_password/1,
        fun create_session/1,
        fun save_session/1,
        fun write_cookie/1
    ],

    FoldFun = fun
        (Step, {ok, _, _, _, _} = Acc) ->
            Step(Acc);
        (_Step, {error, _, _, _, _} = Acc) ->
            Acc
    end,
    case lists:foldl(FoldFun, {ok, undefined, Params, Request, undefined}, Steps) of
        {ok, _, _, NewRequest, _} ->
            {ok, NewRequest};
        {error, _, _, _, _}    ->
            {error, invalid_login}
    end.
