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
        ]).

-export([to_json/2
        ]).

-record(state, {client :: binary()
               ,session :: list()
               ,col :: atom()
               ,opts :: map()
               ,content :: 'undefined' | map() 
               }).

-define(CL, io_lib:format("~s|~s",[S#state.client,S#state.session])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISC OTP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Req,Opts) ->
    S = #state{client = dpserv_tools:get_client_id(Req)
              ,session = dpserv_tools:get_session_id(Req)
              ,col = maps:get(collection, Opts)
              ,opts = Opts
              },

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

resource_exists(Req, S) ->
    {true, Req, S}.

known_methods(Req,S) ->
    {[<<"GET">>,<<"OPTIONS">>], Req,S}.

charsets_provided(Req,S) ->
    {[<<"utf-8">>], Req,S}.

content_types_provided(Req, S) ->
    Types = [{<<"application/json">>, to_json}
            ],
    NewS = case S#state.col of
                all -> S#state{ content = collection_make(all,Req,S)}
           end,
    {Types, Req, NewS}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Exported Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_json(Req,S) ->
    Payload = jiffy:encode(S#state.content,[pretty]),
    {Payload, Req, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
                 ,<<"links">> => [ #{<<"rel">> => <<"self">> ,<<"href">> => <<BaseUri/binary,"/",N/binary,"/meta">>}
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
