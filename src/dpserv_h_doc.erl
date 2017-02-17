-module(dpserv_h_doc).
-behaviour(cowboy_rest).

-include("config.hrl").
-include("common_functions.hrl").
-include("error_pages.hrl").
-include_lib("kernel/include/file.hrl").

-export([init/2
        ,terminate/3]).

-export([allowed_methods/2
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
        ]).

-record(state, {opts :: list()
               ,adv_id :: binary()
               %,ext :: binary()
               ,rmime :: binary()
               ,raw :: binary()
               ,oPath :: binary()
               ,client :: list()
               ,session :: list()
               ,qs :: list()
               ,lang :: atom() }).

-define(MPDF, <<"application/pdf">>).
-define(MTXT, <<"text/plain">>).
-define(MHTML, <<"text/html">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISC OTP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Req, Opts) ->
    Number = cowboy_req:binding(number,Req),
    Lang = ?LN_TO_CODE(cowboy_req:binding(ln,Req)),
    Client = get_client_id(Req),
    Session = get_session_id(Req),
    OPath = dpserv_tools:original_path(number_prefix(Number), Lang, maps:get(store,Opts)),
    log_session(Req,Session),
    S = #state{opts = Opts
              ,adv_id = number_prefix(Number)
              %,ext = number_suffix(Number)
              ,raw = Number
              ,client = Client
              ,session = Session
              ,oPath = OPath
              ,qs = cowboy_req:parse_qs(Req)
              ,rmime = requested_mime(Req,number_suffix(Number))
              ,lang = Lang},
    {cowboy_rest, Req, S }.

terminate(normal,_Req,S) ->
    dps:info("~s: served ~s (~s)",[?CL,S#state.adv_id, S#state.rmime]), ok;
    terminate({throw,{dpserv,Err,Data}},_Req,S) ->
        dps:error("~s: dpserv experienced error ~p\n\tData: ~p\n\tState: ~p\n",[?CL,Err,Data,S]), ok;
    terminate(Reason,_Req,S) ->
        dps:error("dpserv handler terminating because \"~p\"\nState:~p",[S,Reason]), ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COWBOY Rest Handles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
allowed_methods(Req, S) ->
    dps:debug("~s: Allowed_methods",[?CL]),
    {[<<"GET">>], Req, S}.

allow_missing_post(Req, S) ->
    {false, Req,S}.

resource_exists(Req, S) ->
    dps:debug("~s: Resource exist check (state: ~p)",[?CL,S]),
    case filelib:is_file(S#state.oPath) of
        true -> {true, Req,S };
        false -> R = ?ERR_404(?PAGE_404,Req),
                 {stop,R,S}
    end.

expires(Req,S) ->
    {LD, LT} = calendar:local_time(),
    ED = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(LD) + ?EXPIRATION_DAYS),
    dps:debug("~s: Expires = ~p",[?CL, ED]),
    {{ED,LT}, Req, S}.

generate_etag(Req,S) ->
    {ok,I} = file:read_file_info(S#state.oPath),
    Etag = {strong, integer_to_binary(erlang:phash2({I#file_info.size, I#file_info.mtime}, 16#ffffff))},
    dps:debug("~s: Etag = ~p",[?CL, Etag]),
    {Etag,Req,S}.

known_methods(Req,S) ->
    {[<<"GET">>,<<"OPTIONS">>], Req,S}.

charsets_provided(Req,S) ->
    {[<<"utf-8">>], Req,S}.

last_modified(Req,S) ->
    {ok,I} = file:read_file_info(S#state.oPath),
    {I#file_info.ctime, Req,S}.

multiple_choices(Req,S) ->
    %% If no content type is specified (ie: no extension provided)
    %% return true along prefered representation (pdf) and other representations (text,html)
    {false,Req,S}.

forbidden(Req,S) ->
    Type = maps:get(S#state.rmime, #{?MPDF  => pdf_download
                              ,?MTXT  => txt_download
                              ,?MHTML => html_download }),

    case dpserv_limits:check(S#state.client,Type,proplists:get_value(<<"api_key">>,S#state.qs)) of
        ok -> {false, Req, S};
        nook -> R = ?ERR_403(<<"<h2>Forbidden by service limit</h2> See <a href=\"https://github.com/RvS-CdE/dpserv\">https://github.com/RvS-CdE/dpserv</a>">>,Req),
                dps:debug("~s: Forbidden by service limit \"~p\"",[?CL, Type]),
                {stop, R, S}
    end.

is_authorized(Req,S) ->
    %% This is the place to handle IP logging and temporary banning
    _Client = S#state.client,
    {true, Req, S}.

content_types_provided(Req, S) ->
    Pdf = {<<"application/pdf">>, to_pdf},
    Txt = {<<"text/plain">>, to_text},
    Html = {<<"text/html">>, to_html},

    Out = maps:get(S#state.rmime, #{?MPDF  => [Pdf]
                                   ,?MTXT  => [Txt]
                                   ,?MHTML => [Html] }),
    {Out, Req, S}.

service_available(Req,S) ->
    case S#state.rmime of
        ?MPDF -> {true,Req,S};
        _ -> case httpc:request(get, {"http://localhost:10004/tika",[]}, [], []) of
                {error,_} -> dps:warning("~s: Tika not available !",[?CL]),
                             {false,Req,S};
                {ok,_} ->    {true,Req,S}
             end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Exported Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_pdf(Req,S) ->
    dps:debug("~s: outputting in pdf",[?CL]),
    Out = case file:read_file(S#state.oPath) of
            {ok, Bin} -> Bin;
            {error, enoent} -> throw({dpserv,nosuchfile,S#state.oPath});
            {error, R} -> throw({dpserv,filereaderror,[R,S#state.oPath]})
          end,
    FN = filename:basename(S#state.oPath),
    ReqCh = cowboy_req:stream_reply(200
                                   ,#{<<"content-type">> => <<"application/pdf">>
                                     ,<<"content-disposition">> => <<"inline; filename=",FN/binary>>
                                     ,<<"content-transfer-encoding">> => <<"binary">>
                                     ,<<"content-length">> => integer_to_binary(size(Out))
                                     }
                                   , Req),
    serve_chunked(Out,ReqCh),
    {stop,Req,S}.

to_text(Req,S) ->
    dps:debug("~s: outputting in text",[?CL]),
    case tika_query(S#state.oPath,"text/plain") of
        {ok, Text} -> {Text
                      ,Req
                      %,?SH(<<"content-type">>,<<"text/plain; charset=utf-8">>,Req)
                      ,S };
        {error, Reason} -> dps:error("~s: Tika conversion failed: ~s",[?CL,Reason]),
                           R = ?ERR_500("File conversion failed.",Req),
                           {stop,R,S}
    end.

to_html(Req,S) ->
    dps:debug("~s: outputting in html",[?CL]),
    case tika_query(S#state.oPath,"text/html") of
        {ok, Raw} -> {Raw
                     ,?SH(<<"content-type">>,<<"text/html; charset=utf-8">>,Req)
                     ,S };
        {error, Reason} -> dps:error("~s: Tika conversion failed: ~s",[?CL,Reason]),
                           R = ?ERR_500("File conversion failed.",Req),
                           {stop,R,S}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default: 128kb
serve_chunked(<<D:131072/binary,R/binary>>,Req) ->
    cowboy_req:stream_body(D,nofin,Req),
    serve_chunked(R,Req);

serve_chunked(<<D/binary>>,Req) ->
    cowboy_req:stream_body(D,fin,Req).

%original_path(Num,Ln,Type) -> dpserv_tools:original_path(Num,Ln,Type).
get_client_id(Req) -> dpserv_tools:get_client_id(Req).
log_session(Req,Session) -> dpserv_tools:log_session(Req,Session).
get_session_id(Req) -> dpserv_tools:get_session_id(Req).
number_prefix(Number) -> dpserv_tools:number_prefix(Number).
number_suffix(Number) -> dpserv_tools:number_suffix(Number).

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

requested_mime(Req,undefined) ->
    ParsedAccept = cowboy_req:parse_header(<<"accept">>,Req),
    Accepted = [<<Type/binary,"/",Subtype/binary>> || {{Type,Subtype,_},_,_} <- ParsedAccept],

    case {lists:member(<<"*/*">>,Accepted)
         ,lists:member(?MTXT,Accepted)
         ,lists:member(?MHTML,Accepted)} of
         {true, _, _} -> ?MPDF;
         {_,true,_} -> ?MTXT;
         {_,_,true} -> ?MHTML
    end;
    
requested_mime(_,<<"txt">>) -> ?MTXT;
requested_mime(_,<<"html">>) -> ?MHTML;
requested_mime(_,_) -> ?MPDF.
