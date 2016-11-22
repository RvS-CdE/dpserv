-module(dpserv_handler).
-behaviour(cowboy_rest).

-include("config.hrl").
-include("common_functions.hrl").
-include_lib("kernel/include/file.hrl").

-export([init/2
        ,allowed_methods/2
        ,allow_missing_post/2
        ,resource_exists/2
        ,content_types_provided/2
        ,expires/2
        ,forbidden/2
        ,is_authorized/2
        ,generate_etag/2
        ,known_methods/2
        ,last_modified/2
        ,charsets_provided/2
        ,multiple_choices/2
        ,service_available/2
        ]).

-export([to_pdf/2
        ,to_text/2
        ,to_html/2
        ,terminate/3
        ]).

-record(state, {opts, adv_id, ext, client,session}).
-define(CL, io_lib:format("~s|~s",[S#state.client,S#state.session])).

init(Req, Opts) ->
    Number = cowboy_req:binding(number,Req),
    Client = get_client_id(Req),
    Session = get_session_id(Req),
    log_session(Req,Session),
    %?DBG([headers, peer],[cowboy_req:headers(Req), cowboy_req:peer(Req)]),
    {cowboy_rest, Req, #state{opts = Opts
                             ,adv_id = number_prefix(Number)
                             ,ext = number_suffix(Number)
                             ,client = Client
                             ,session = Session}}.

allowed_methods(Req, S) ->
    dps:debug("~s: Allowed_methods",[?CL]),
    {[<<"GET">>], Req, S}.

allow_missing_post(Req, S) ->
    {false, Req,S}.

resource_exists(Req, S) ->
    dps:debug("~s: Resource exist check (state: ~p)",[?CL,S]),
    OPath = original_path(S#state.adv_id,maps:get(lang,S#state.opts), maps:get(collection,S#state.opts)),
    case filelib:is_file(OPath) of
        true -> {true, Req,S };
        false -> R = ?ERR_404(<<"Just Not There">>,Req),
                 {stop,R,S}
    end.

expires(Req,S) ->
    {LD, LT} = calendar:local_time(),
    ED = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(LD) + ?EXPIRATION_DAYS),
    dps:debug("~s: Expires = ~p",[?CL, ED]),
    {{ED,LT}, Req, S}.

generate_etag(Req,S) ->
    OPath = original_path(S#state.adv_id,maps:get(lang,S#state.opts), maps:get(collection,S#state.opts)),
    {ok,I} = file:read_file_info(OPath),
    Etag = {strong, integer_to_binary(erlang:phash2({I#file_info.size, I#file_info.mtime}, 16#ffffff))},
    dps:debug("~s: Etag = ~p",[?CL, Etag]),
    {Etag,Req,S}.

known_methods(Req,S) ->
    {[<<"GET">>,<<"OPTIONS">>], Req,S}.

charsets_provided(Req,S) ->
    {[<<"utf-8">>], Req,S}.

last_modified(Req,S) ->
    OPath = original_path(S#state.adv_id, maps:get(lang,S#state.opts), maps:get(collection,S#state.opts)),
    {ok,I} = file:read_file_info(OPath),
    {I#file_info.ctime, Req,S}.

multiple_choices(Req,S) ->
    %% If no content type is specified (ie: no extension provided)
    %% return true along prefered representation (pdf) and other representations (text,html)
    {false,Req,S}.

forbidden(Req,S) ->
    Auth = case cowboy_req:method(Req) of
            <<"GET">> -> true;
            _ -> {false, <<"Sorry, bad method: Thanks for trying !">>} end,
    dps:debug("~s: Forbidden = ~p",[?CL, Auth]),
    {Auth, Req, S}.

is_authorized(Req,S) ->
    %% This is the place to handle IP logging and temporary banning
    Client = S#state.client,
    {true, Req, S}.

content_types_provided(Req, S) ->
    Pdf = {<<"application/pdf">>, to_pdf},
    Txt = {<<"text/plain">>, to_text},
    Html = {<<"text/html">>, to_html},
    Out = case S#state.ext of
            <<"pdf">> -> [Pdf];
            <<"txt">> -> [Txt];
            <<"html">> -> [Html];
            undefined -> [Pdf,Html,Txt]
          end,
    {Out, Req, S}.

to_pdf(Req,S) ->
    OPath = original_path(S#state.adv_id,maps:get(lang,S#state.opts), maps:get(collection,S#state.opts)),
    Out = case file:read_file(OPath) of
            {ok, Bin} -> Bin;
            {error, enoent} -> throw({dpserv,nosuchfile,OPath});
            {error, R} -> throw({dpserv,filereaderror,[R,OPath]})
          end,
    {Out, Req, S}.

terminate(normal,_Req,S) ->
    dps:info("~s: served ~s.~s",[?CL,S#state.adv_id, S#state.ext]),
    ok;
    terminate({throw,{dpserv,Err,Data}},_Req,S) ->
        dps:error("~s: dpserv experienced error ~p\n\tData: ~p\n\tState: ~p\n",[?CL,Err,Data,S]),
        ok;
    terminate(Reason,_Req,S) ->
        dps:error("dpserv handler terminating because \"~p\"\nState:~p",[S,Reason]),
        ok.

to_text(Req,S) ->
    OPath = original_path(S#state.adv_id,maps:get(lang,S#state.opts), maps:get(collection,S#state.opts)),
    case tika_query(OPath,"text/plain") of
        {ok, Text} -> {Text
                      ,Req
                      %,?SH(<<"content-type">>,<<"text/plain; charset=utf-8">>,Req)
                      ,S };
        {error, Reason} -> dps:error("~s: Tika conversion failed: ~s",[?CL,Reason]),
                           R = ?ERR_500("File conversion failed.",Req),
                           {stop,R,S}
    end.

to_html(Req,S) ->
    OPath = original_path(S#state.adv_id,maps:get(lang,S#state.opts), maps:get(collection,S#state.opts)),
    case tika_query(OPath,"text/html") of
        {ok, Raw} -> {Raw
                     ,?SH(<<"content-type">>,<<"text/html; charset=utf-8">>,Req)
                     ,S };
        {error, Reason} -> dps:error("~s: Tika conversion failed: ~s",[?CL,Reason]),
                           R = ?ERR_500("File conversion failed.",Req),
                           {stop,R,S}
    end.

service_available(Req,S) ->
    case S#state.ext of
        <<"pdf">> -> {true,Req,S};
        _ -> case httpc:request(get, {"http://localhost:10004/tika",[]}, [], []) of
                {error,_} -> dps:warning("~s: Tika not available !",[?CL]),
                             {false,Req,S};
                {ok,_} ->    {true,Req,S}
             end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
original_path(Number, de, adv) when is_binary(Number) ->
    RPath = list_to_binary(?GET_ENV(srv_dir)),
    <<RPath/binary, "/", Number/binary, "_german.pdf">>;
original_path(Number, _, adv) when is_binary(Number) ->
    RPath = list_to_binary(?GET_ENV(srv_dir)),
    <<RPath/binary, "/", Number/binary, ".pdf">>;

original_path(Number, _Ln, adv_proj) when is_binary(Number) ->
    RPath = list_to_binary(?GET_ENV(srv_dir)),
    <<RPath/binary, "/", Number/binary, "_project.pdf">>;

original_path(Number, Lang, Collection) when is_binary(Number) ->
    dps:critical("Could not find file for ~s according to provided specifications\n\tLanguage: ~p\n\tCollection:~p\n"
             ,[Number,Lang,Collection]),
    nook.

get_client_id(Req) ->
    case cowboy_req:header(<<"x-forwarded-for">>, Req, undefined) of
        undefined -> {{A,B,C,D},_} = cowboy_req:peer(Req),
                    P = <<".">>,
                    [integer_to_binary(A),P
                    ,integer_to_binary(B),P
                    ,integer_to_binary(C),P
                    ,integer_to_binary(D)];
        ClientIp -> <<ClientIp/binary, "@w">>
    end.

get_session_id(Req) ->
    H = erlang:phash2({cowboy_req:header(<<"user_agent">>,Req)
                      ,get_client_id(Req)
                      ,calendar:local_time()
                      }
                     ,16#ffffffff),
    integer_to_list(H,16).

log_session(Req,Session) ->
    dps:info("~s|~s \"~s\" ~s ~s ~s"
            ,[get_client_id(Req)
             ,Session
             ,cowboy_req:header(<<"user-agent">>,Req)
             ,cowboy_req:uri(Req)
             ,cowboy_req:header(<<"x-original-url">>,Req)
             ,cowboy_req:qs(Req)
             ]).

number_prefix(Number) ->
    P = number_parts(Number),
    maps:get(prefix,P,undefined).

number_suffix(Number) ->
    P = number_parts(Number),
    maps:get(suffix,P,undefined).

number_parts(Number) ->
    number_parts(Number,<<>>).

    number_parts(<<>>,Acc) ->
        #{prefix => Acc, suffix => undefined};
    number_parts(<<".",R/binary>>,Acc) ->
        #{prefix => Acc, suffix => R};
    number_parts(<<C,R/binary>>,Acc) ->
        number_parts(R,<<Acc/binary,C>>).

tika_query(Path,Format) ->
    {ok,B} = file:read_file(Path),
    R = httpc:request(put
                     ,{"http://localhost:10004/tika",[{"Accept",Format},{"Accept-Encoding","identity"}],"application/pdf",B}
                     ,[{timeout,5000},{connect_timeout,300}]
                     ,[{full_result,true},{body_format,binary}]),
    case R of
        {error, Reason} -> {error, io_lib:format("request error: ~p",[Reason])};
        {ok, {{_,200,_}, _, Body}} -> {ok,Body};
        {ok, {Status,_, _}} -> {error, io_lib:format("unexpected response status: ~p",[Status])}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Testing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
number_prefix_test_() ->
    Test1 = <<"51134">>,
    Test2 = <<"51134.pdf">>,
    Test3 = <<"51134.html">>,
    [?_assertEqual(<<"51134">>, number_prefix(Test1))
    ,?_assertEqual(<<"51134">>, number_prefix(Test2))
    ,?_assertEqual(<<"51134">>, number_prefix(Test3))
    ].

number_suffix_test_() ->
    Test1 = <<"51134">>,
    Test2 = <<"51134.pdf">>,
    Test3 = <<"51134.html">>,
    [?_assertEqual(undefined, number_suffix(Test1))
    ,?_assertEqual(<<"pdf">>, number_suffix(Test2))
    ,?_assertEqual(<<"html">>, number_suffix(Test3))
    ].
-endif.
