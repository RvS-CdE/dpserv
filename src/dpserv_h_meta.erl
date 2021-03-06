-module(dpserv_h_meta).
-behaviour(cowboy_rest).

-include("config.hrl").
-include("common_functions.hrl").
-include_lib("kernel/include/file.hrl").

-export([init/2
        ,terminate/3]).

-export([allowed_methods/2
        ,resource_exists/2
        ,known_methods/2
        ,charsets_provided/2
        ,content_types_provided/2
        ]).

-export([to_json/2
        ]).

-record(state, {opts :: list()
               ,adv_id :: binary()
               ,raw :: binary()
               ,oPath :: binary()
               ,client :: binary()
               ,session :: list()
               ,meta :: 'undefined' | map()
               ,lang :: atom() }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISC OTP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Req,Opts) ->
    Number = cowboy_req:binding(number,Req),
    Lang = ?LN_TO_CODE(cowboy_req:binding(ln,Req)),
    Client = dpserv_tools:get_client_id(Req),
    Session = dpserv_tools:get_session_id(Req),
    OPath = dpserv_tools:original_path(dpserv_tools:number_prefix(Number), Lang, maps:get(store,Opts)),
    dpserv_tools:log_session(Req,Session),
    {cowboy_rest, Req, #state{opts = Opts
                             ,adv_id = dpserv_tools:number_prefix(Number)
                             ,raw = Number
                             ,oPath = OPath
                             ,client = Client
                             ,session = Session
                             ,lang = Lang}}.

terminate(normal,_Req,S) ->
    dps:info("~s: served ~s meta",[?CL,S#state.adv_id]),
    ok;
    terminate({throw,{dpserv,Err,Data}},_Req,S) ->
        dps:error("~s: dpserv experienced error ~p\n\tData: ~p\n\tState: ~p\n",[?CL,Err,Data,S]),
        ok;
    terminate(Reason,_Req,S) ->
        dps:error("dpserv handler terminating because \"~p\"\nState:~p",[S,Reason]),
        ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COWBOY Rest Handles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
allowed_methods(Req, S) ->
    dps:debug("~s: Allowed_methods",[?CL]),
    {[<<"GET">>], Req, S}.

resource_exists(Req, S) ->
    dps:debug("~s: Resource exist check",[?CL]),
    case filelib:is_file(S#state.oPath) of
        true -> {true, Req,S };
        false -> R = ?ERR_404(<<"Just Not There">>,Req),
                 {stop,R,S}
    end.

known_methods(Req,S) ->
    {[<<"GET">>,<<"OPTIONS">>], Req,S}.

charsets_provided(Req,S) ->
    {[<<"utf-8">>], Req,S}.

content_types_provided(Req, S) ->
    Types = [{<<"application/json">>, to_json}
            ],
    {Types, Req, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Exported Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_json(Req,S) ->
    Payload = jiffy:encode(doc_meta(Req,S), [pretty]),
    {Payload, Req, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
doc_meta(R,S) ->
    %% The meta data generating part will probably need its own module, thread.
    PData = pdf_data(S#state.oPath),

    {ok,I} = file:read_file_info(S#state.oPath),
    #{<<"id">> => S#state.adv_id
     ,<<"size">> => I#file_info.size
     ,<<"mtime">> => mktime(I#file_info.mtime)
     ,<<"ctime">> => mktime(I#file_info.ctime)
     ,<<"md5">> => file_md5(S#state.oPath)
     ,<<"links">> => doc_links(R,S)
     ,<<"links">> => doc_links(R,S)
     ,<<"pages">> => erlang:list_to_integer(maps:get("pages",PData,"0"))
     }.

doc_links(R,S) ->
    %?DBG([headers, peer, path, path_info, host, host_info, port, uri, base_url]
    %    ,[cowboy_req:headers(R)
    %     ,cowboy_req:peer(R)
    %     ,cowboy_req:path(R)
    %     ,cowboy_req:path_info(R)
    %     ,cowboy_req:host(R)
    %     ,cowboy_req:host_info(R)
    %     ,cowboy_req:port(R)
    %     ,cowboy_req:uri(R)
    %     ,cowboy_req:header(<<"x-base-url">>,R,undefined)]),
    Base = get_hostbaseuri(R,S),
    lists:foldl(fun(Link,Acc) ->
                case make_link(Link,R,S,Base) of
                    undefined -> Acc;
                    Url -> [#{<<"rel">> => Link ,<<"href">> => Url } | Acc]
                end
              end,
            [],
            [<<"self">>
            ,<<"next">>
            ,<<"previous">>
            ,<<"collection">>
            ,<<"content">>
            ,<<"project">>
            ,<<"german_translation">>]).

    make_link(<<"self">>,_R,S,Base) ->
        Raw = S#state.raw,
        <<Base/binary,"/",Raw/binary,"/meta">>;

    make_link(<<"content">>,_R,S,Base) ->
        Raw = S#state.raw,
        <<Base/binary,"/",Raw/binary>>;

    make_link(<<"next">>,_R,S,Base) ->
        case dpserv_tools:getnext(S#state.adv_id, S#state.lang, maps:get(store,S#state.opts)) of
            undefined -> undefined;
            NextRaw -> <<Base/binary,"/",NextRaw/binary,"/meta">>
        end;
    make_link(<<"previous">>,_R,S,Base) ->
        case dpserv_tools:getprev(S#state.adv_id, S#state.lang, maps:get(store,S#state.opts)) of
            undefined -> undefined;
            PrevRaw -> <<Base/binary,"/",PrevRaw/binary,"/meta">>
        end;
    make_link(<<"german_translation">>,R,S,_Base) ->
        OPath = dpserv_tools:original_path(S#state.adv_id, de, maps:get(store,S#state.opts)),
        case filelib:is_file(OPath) of
            true -> NewB = get_hostbaseuri(R,S,?CODE_TO_LN(de)),
                    Raw = S#state.raw,
                    <<NewB/binary,"/",Raw/binary>>;
            false -> undefined
        end;
    make_link(<<"project">>,_R,S,Base) ->
        OPath = dpserv_tools:original_path(S#state.adv_id, S#state.lang, adv_proj),
        case filelib:is_file(OPath) of
            true -> Raw = S#state.raw,
                    Proj = case S#state.lang of
                            fr -> <<"projet">>;
                            nl -> <<"ontwerp">>;
                            de -> <<"entwurf">> end,
                    <<Base/binary,"/",Raw/binary, "/", Proj/binary>>;
            false -> undefined
        end;
    make_link(_,_,_,_) ->
        undefined.

file_md5(Path) ->
    Context = crypto:hash_init(md5),
    {ok,I} = file:read_file_info(Path),
    Size = I#file_info.size,
    {ok,FO} = file:open(Path, [raw, binary, read_ahead]),
    Hash = file_hash(Context, 0, Size, FO),
    file:close(FO),
    Hash.

    file_hash(Context, Pos, Max, FO) when Pos < Max ->
        NewC = case file:pread(FO, Pos, 4096) of
                {ok,Data} -> crypto:hash_update(Context, Data);
                eof -> Context;
                ebadf -> dps:error("File hasher could not read a file"),
                         Context;
                Err -> dps:error("File hasher had a problem: ~p",[Err]),
                       Context
               end,
        file_hash(NewC, Pos + 4096, Max, FO);

    file_hash(Context, _, _, _) ->
        Digest = crypto:hash_final(Context),
        dpserv_tools:bin2hex(Digest).

pdf_data(Path) ->
    Data = os:cmd(lists:concat(["pdfinfo ",erlang:binary_to_list(Path)])),
    parse_pdf_data(Data).

parse_pdf_data(Data) ->
    ToksCln = [string:to_lower(string:strip(T)) || T <- string:tokens(Data,":\n")],
    F = fun([Key,Val|T],Fun) ->
            [{Key,Val} | Fun(T,Fun)];
           ([Single],_) -> [{Single,true}];
           ([],_) -> [] end,
    maps:from_list(F(ToksCln,F)).

%% Inspiration : http://stackoverflow.com/users/1691583/viacheslav-kovalev @ http://stackoverflow.com/questions/28887695/erlang-converting-timestamp-to-year-month-daythourminsecz-format
mktime(Datetime) ->
    iso8601:format(Datetime).
    %list_to_binary(lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second]))).

get_hostbaseuri(R,S) -> dpserv_tools:get_hostbaseuri(R,maps:get(base,S#state.opts)).
get_hostbaseuri(R,S,L) -> dpserv_tools:get_hostbaseuri(R,maps:get(base,S#state.opts),L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Testing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(TESTFILE,<<"./LICENSE">>).
-define(TESTMD5,<<"65D26FCC2F35EA6A181AC777E42DB1EA">>).
-define(TEST_PDF_INFO,"Title:          Microsoft Word - 56372.docx\nAuthor:         lmi\nCreator:        PScript5.dll Version 5.2.2\nProducer:       Acrobat Distiller 9.0.0 (Windows)\nCreationDate:   Mon Jul  7 09:33:49 2014\nModDate:        Mon Jul  7 09:33:49 2014\nTagged:         no\nUserProperties: no\nSuspects:       no\nForm:           none\nJavaScript:     no\nPages:          5\nEncrypted:      no\nPage size:      595.22 x 842 pts (A4)\nPage rot:       0\nFile size:      59412 bytes\nOptimized:      yes\nPDF version:    1.5\n").


filemd5_test() ->
    ?assertEqual(?TESTMD5, file_md5(?TESTFILE)).

parse_pdf_info_test_() ->
    D = parse_pdf_data(?TEST_PDF_INFO),
    [?_assert(is_map(D))
    ,?_assertEqual("lmi",maps:get("author",D))
    ,?_assertEqual("5",maps:get("pages",D))
    ].

-endif.
