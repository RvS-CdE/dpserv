-module(dpserv_meta_handler).
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

-record(state, {opts, adv_id, oPath, client, session, meta}).
-define(CL, io_lib:format("~s|~s",[S#state.client,S#state.session])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISC OTP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Req,Opts) ->
    Number = cowboy_req:binding(number,Req),
    Client = dpserv_tools:get_client_id(Req),
    Session = dpserv_tools:get_session_id(Req),
    OPath = dpserv_tools:original_path(Number,maps:get(lang,Opts), maps:get(collection,Opts)),
    dpserv_tools:log_session(Req,Session),
    {cowboy_rest, Req, #state{opts = Opts
                             ,adv_id = dpserv_tools:number_prefix(Number)
                             ,oPath = OPath
                             ,client = Client
                             ,session = Session}}.

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
    dps:debug("~s: Resource exist check (state: ~p)",[?CL,S]),
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
    SNew = S#state{meta = doc_meta(Req,S)},
    {Types, Req, SNew}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Exported Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_json(Req,S) ->
    Payload = jiffy:encode(S#state.meta, [pretty]),
    {Payload, Req, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
doc_meta(R,S) ->
    %% The meta data generating part will probably need its own module, thread.
    {ok,I} = file:read_file_info(S#state.oPath),
    #{<<"id">> => S#state.adv_id
     ,<<"size">> => I#file_info.size
     ,<<"mtime">> => mktime(I#file_info.mtime)
     ,<<"md5">> => file_md5(S#state.oPath)
     ,<<"links">> => doc_links(R,S)
     }.

doc_links(R,S) ->
    lists:map(fun(Link) ->
                #{<<"rel">> => Link
                 ,<<"href">> => make_link(Link,R,S) }
              end,
            [<<"self">>
            ,<<"next">>
            ,<<"previous">>
            ,<<"collection">>
            ,<<"content">>
            ,<<"project">>
            ,<<"de">>]).

    make_link(_,R,S) ->
        <<"http://localhost/somewhere">>.

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

mktime(Datetime) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = Datetime,
    list_to_binary(lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second]))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Testing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(TESTFILE,<<"./LICENSE">>).
-define(TESTMD5,<<"65D26FCC2F35EA6A181AC777E42DB1EA">>).

filemd5_test() ->
    ?assertEqual(?TESTMD5, file_md5(?TESTFILE)).
-endif.
