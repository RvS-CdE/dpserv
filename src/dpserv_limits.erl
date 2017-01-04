-module(dpserv_limits).
%% An early attempt at rate-limiting access to documents,
%% meant to be containted within a separate OTP app, so it's
%% structured in a way to be refactored in the future.
-include("config.hrl").
-include("common_functions.hrl").

-export([check/2    %% Check if an IP is allowed to acces resource
        ,list/0     %% List IP's and some data
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXPORTED API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check(Ip,Def) ->
    check(Ip,Def,?RATE_LIMIT_ETS,?SERVICE_LIMITS).

    check(Ip,Def,Table,Conf) ->
        RawRec = rec_get(Ip,Table),
        ClnRec = rec_clean(RawRec),
        Rec = rec_increment(ClnRec,Def),
        rec_store(Rec,Table),
        Max   = maps:get(Def,Conf),
        Check = rec_type_count(Def,Rec),
        if Check >  Max -> nook;
           true -> ok
        end.

list() ->
    Job = fun({Ip,Store},Acc) ->
                [{Ip,length(Store)} | Acc]
                end,
    ets:foldl(Job
             ,[]
             ,?RATE_LIMIT_ETS).
    
            

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rec_get(Ip,Table) ->
    Exists = ets:member(Table, Ip),
    if not Exists -> {Ip,[]};
       true -> [R] = ets:take(Table,Ip),
               R 
    end.

rec_increment({Ip,StampStore},Type) ->
    {Ip,[{Type,stamp_get()} | StampStore]}.

rec_clean({Ip,StampStore}) ->
    Now = stamp_get(),
    NewST = lists:foldl(fun({Typ,Stamp},Acc) ->
                            Chk = Now - Stamp,
                            if Chk >= ?SERVICE_LIMIT_PERIOD ->
                                    Acc;
                               true ->
                                    [{Typ,Stamp} | Acc] end
                            end
                        ,[]
                        ,StampStore),
    {Ip,NewST}.

rec_store(Rec={_,St},Table) ->
    if St =:= [] -> ok;
       true -> ets:insert(Table,Rec)
    end.
       

rec_type_count(Type,{_,Store}) ->
    Tfun = fun({Tst,_},Acc) ->
            if Tst =/= Type -> Acc;
               true -> Acc + 1 end end,
    lists:foldl(Tfun
               ,0
               ,Store).
    

stamp_get() ->
    os:system_time(second).

%% Create ETS dict, stored on IP:
%% IP -> {access_type, access_timestamp)
%% On check, filter per type, delete older results, count remaining ones
%% Compare that result to defined limits for that type.
%%
%% Ets should be pruned once in a while. (start a pruning thread somewhere)
%% -> iterate on ips, remove old timestamps, and if none remain, juste erase (use ets:take and ets:insert_new)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UNIT TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(TST_ETS,tst_ets).

-define(IP1,<<"127.127.127.127">>).
-define(TST_CONF,#{tst => 2}).

rec_clean_test_() ->
    Now = stamp_get(),
    OldRec = {?IP1,[{tst, Now - 120}
                   ,{tst, Now - 3600*24}
                   ,{tst, Now - 49}
                   ,{tst, Now }]},
    ExpRec = {?IP1,[{tst, Now }
                   ,{tst, Now - 49 }]},
    {_,St} = NewR = rec_clean(OldRec),
    [?_assertEqual(2, length(St))
    ,?_assertEqual(ExpRec, NewR)
    ].

rec_count_test_() ->
    TstRec = {?IP1, [{tst,1},{tst,2},{tst,3},{pad,4}]},
    [?_assertEqual(3, rec_type_count(tst,TstRec))
    ,?_assertEqual(1, rec_type_count(pad,TstRec))
    ,?_assertEqual(0, rec_type_count(sxe,TstRec))
    ].

    

tst_exists(T) ->
    [?_assert(ets:info(T) =/= undefined)].

tst_getifnew(T) ->
    [?_assertEqual(ok, check(?IP1, tst, T, ?TST_CONF))].

tst_getifthere(T) ->
    Now = stamp_get(),
    rec_store({?IP1,[{tst,Now-10}]}, T),
    [?_assertEqual(ok, check(?IP1, tst, T, ?TST_CONF))].

tst_shouldblock(T) ->
    Now = stamp_get(),
    Rec = {?IP1,[{tst, Now - 120}
                ,{tst, Now - 3600*24}
                ,{tst, Now - 49}
                ,{tst, Now - 39}
                ,{tst, Now - 29}
                ,{tst, Now}]},
    rec_store(Rec,T),
    [?_assertEqual(nook, check(?IP1, tst, T, ?TST_CONF))].
    
tst_shouldnotblock(T) ->
    Now = stamp_get(),
    Rec = {?IP1,[{tst, Now - 120}
                ,{tst, Now - 3600*24}
                ,{tst, Now - 29}
                ]},
    rec_store(Rec,T),
    [?_assertEqual(nook, check(?IP1, tst, T, ?TST_CONF))].

tst_store_full(T) ->
    rec_store({?IP1,[{tst,1}]}, T),
    [?_assert(ets:member(T,?IP1))].

tst_store_empty(T) ->
    rec_store({?IP1,[]}, T),
    [?_assert(not ets:member(T,?IP1))].
    


basic_rate_limit_test_() ->
    {foreach
        ,fun tst_start/0
        ,fun tst_stop/1
        ,[fun tst_exists/1
         ,fun tst_store_full/1
         ,fun tst_store_empty/1
         ,fun tst_getifnew/1
         ,fun tst_getifthere/1
         ,fun tst_shouldblock/1
         ]
    }.

tst_start() ->
    %ets:new(?TST_ETS,[named_table,public,{write_concurrency,true},{read_concurrency,true}]),
    ets:new(?TST_ETS,[public,{write_concurrency,true},{read_concurrency,true}]).

tst_stop(Table) ->
    ets:delete(Table),
    ok.

-endif.
