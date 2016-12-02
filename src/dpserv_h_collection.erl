-module(dpserv_h_collection).
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
        ,malformed_request/2
        ]).

-export([to_json/2
        ]).

-record(state, {client :: binary()
               ,session :: list()
               ,col :: atom()
               ,opts :: map()
               ,content :: 'undefined' | map()
               ,params :: 'undefined' | map()
               }).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISC OTP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Req,Opts) ->
    S = #state{client = dpserv_tools:get_client_id(Req)
              ,session = dpserv_tools:get_session_id(Req)
              ,col = cowboy_req:binding(collection,Req)
              ,opts = Opts
              ,params = dpserv_tools:params_get([from,to],Req)
              },
    %?DBG([state],[S]),
    dps:debug("~s: Preparing query for collection ~p with parameters ~p",[?CL,S#state.col,S#state.params]),
    {cowboy_rest, Req, S}.

terminate(normal,_Req,S) ->
    dps:info("~s: served collection",[?CL]),
    ok;
    terminate({throw,{dpserv,Err,Data}},_Req,S) ->
        dps:error("~s: ~p experienced error ~p\n\tData: ~p\n\tState: ~p\n",[?CL,?MODULE,Err,Data,S]),
        ok;
    terminate(Reason,_Req,S) ->
        dps:error("~p terminating because \"~p\"\nState:~p",[?MODULE,S,Reason]),
        ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COWBOY Rest Handles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
allowed_methods(Req, S) ->
    dps:debug("~s: Allowed_methods",[?CL]),
    {[<<"GET">>], Req, S}.

malformed_request(Req, S) ->
    ParamsIn = S#state.params,
    try
        P1 = parse_indate(from,ParamsIn),
        P2 = parse_indate(to, P1),
        case lists:member(S#state.col, ?COL_DATE_LIMITED) of
            true -> case {maps:get(from,P2), maps:get(to,P2)} of
                        {undefined, undefined} -> Re = ?ERR_422("From and/or To field(s) missing.",Req),
                                                  {stop, Re, S};
                        _ -> {false, Req, S#state{ params = P2 }}
                    end;
            false -> {false,Req,S} 
        end
    catch
        error:{badarg,F} -> R = ?ERR_422(io_lib:format("Date field \"~p\" not valid iso8601 format.",[F]),Req),
                            {stop, R, S}
    end.

    parse_indate(Key,Params) ->
        case maps:get(Key,Params) of
            undefined -> Params;
            SomeDate -> try Params#{Key => iso8601:parse(SomeDate)}
                        catch error:badarg -> error({badarg,Key}) end
        end.

resource_exists(Req, S) ->
    Out = case lists:member(S#state.col,?COLLECTIONS) of
        true -> true;
        false -> dps:warning("~s: Collection ~p not available",[?CL,S#state.col]),
                false end,
    {Out, Req,S}.

known_methods(Req,S) ->
    {[<<"GET">>,<<"OPTIONS">>], Req,S}.

charsets_provided(Req,S) ->
    {[<<"utf-8">>], Req,S}.

content_types_provided(Req, S) ->
    Types = [{<<"application/json">>, to_json}
            ],
    NewS = case S#state.col of
                <<"all">> -> S#state{content = collection_make(all,Req,S)};
                <<"ctime">> -> S#state{content = collection_make(ctime,Req,S)};
                Else -> dps:debug("Unexpected collection found: ~p",[Else]),
                        S
           end,
    {Types, Req, NewS}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Exported Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_json(Req,S) ->
    Payload = jiffy:encode(S#state.content,[pretty]),
    {Payload, Req, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collection_make(ctime,R,S) ->
    {ok,All} = file:list_dir(?GET_ENV(srv_dir)),
    Params = S#state.params,
    AdvFiles = dpserv_tools:filter_filelist(adv,All),
    BefCheck = case maps:get(to,Params) of
                undefined -> fun(_) -> true end;
                Date -> Mark = calendar:datetime_to_gregorian_seconds(Date),
                        fun(_In) ->
                            Needle = calendar:datetime_to_gregorian_seconds(_In),
                            Needle < Mark end
              end,

    AftCheck = case maps:get(from,Params) of
                undefined -> fun(_) -> true end;
                Date2 -> Mark2 = calendar:datetime_to_gregorian_seconds(Date2),
                        fun(_In) ->
                            Needle = calendar:datetime_to_gregorian_seconds(_In),
                            Needle > Mark2 end
              end,

    RawList = lists:foldl(fun(FName,Acc) ->
                            {ok,I} = file:read_link_info(?GET_ENV(srv_dir) ++ "/" ++ FName),
                            Ctime = I#file_info.ctime,
                            case {BefCheck(Ctime),AftCheck(Ctime)} of
                                {true, true} -> dict:update(extractInt(FName), fun(Cnt) -> Cnt+1 end, 1, Acc);
                                _ -> Acc
                            end end
                          ,dict:new()
                          ,AdvFiles),

    List = dict:fetch_keys(RawList),
    Base = dpserv_tools:get_hostbaseuri(R,maps:get(base,S#state.opts)),
    #{<<"collection">> => <<"ctime">>
     ,<<"from">> => case maps:get(from,Params) of undefined -> <<"undefined">>; From -> iso8601:format(From) end
     ,<<"to">> => case maps:get(to,Params) of undefined -> <<"undefined">>; To -> iso8601:format(To) end
     ,<<"links">> => [ #{<<"rel">> => <<"self">>
                        ,<<"href">> => <<Base/binary,"/col/all">>} ]
     ,<<"content">> => list_generate(List,Base)
     };

collection_make(all,R,S) ->
    {ok,All} = file:list_dir(?GET_ENV(srv_dir)),
    RawList = lists:foldl(fun(FName,Acc) ->
                              Num = extractInt(FName),
                              dict:update(Num,fun(I) -> I+1 end, 1, Acc)
                              end
                         ,dict:new()
                         ,All),
    List = dict:fetch_keys(RawList),
    Base = dpserv_tools:get_hostbaseuri(R,maps:get(base,S#state.opts)),
    #{<<"collection">> => <<"all">>
     ,<<"links">> => [ #{<<"rel">> => <<"self">>
                        ,<<"href">> => <<Base/binary,"/col/all">>} ]
     ,<<"content">> => list_generate(List,Base)
     }.

list_generate(Numbers,BaseUri) ->
   lists:map(fun(N) ->
                #{<<"id">> => N
                 ,<<"links">> => [#{<<"rel">> => <<"self">> ,<<"href">> => <<BaseUri/binary,"/",N/binary,"/meta">>}
                                 ,#{<<"rel">> => <<"content">> ,<<"href">> => <<BaseUri/binary,"/",N/binary>>}]}
                end
            ,Numbers).

extractInt(Num) ->
    intParts(Num,[]).

    intParts([H|_],Acc) when H =:= $.; H =:= $_ ->
        list_to_binary(lists:reverse(Acc));
    intParts([],Acc) ->
        list_to_binary(lists:reverse(Acc));
    intParts([H|T],Acc) ->
        intParts(T,[H|Acc]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Testing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
extractInt_test_() ->
    [?_assertEqual(<<"12">>,extractInt("12.qwp"))
    ,?_assertEqual(<<"12409">>,extractInt("12409_test.pdf"))
    ,?_assertEqual(<<"1409">>,extractInt("1409"))
    ].

-endif.
