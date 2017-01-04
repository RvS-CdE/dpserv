-module(dpserv_tools).

-include("config.hrl").
-include("common_functions.hrl").

-export([cfg_get/1
        ,print_debug/3
        ,err_403/2
        ,err_404/2
        ,err_422/2
        ,err_500/2
        ,get_client_id/1
        ,get_session_id/1
        ,log_session/2
        ,number_prefix/1
        ,number_suffix/1
        ,original_path/3
        ,hex2bin/1
        ,bin2hex/1
        ,getprev/3
        ,getnext/3
        ,get_hostbaseuri/2 ,get_hostbaseuri/3
        ,params_get/2
        ,link_factory/3
        ,filter_filelist/2
        ,get_env_def/0
        ]).

cfg_get(Key) ->
    case proplists:get_value(Key,get_env_def()) of
        undefined -> throw({dpserv,undefined_cfg,Key});
        Val -> Val
    end.

print_debug(Module,Fields,Data) ->
    io:format("\n== DEBUGGING in ~s ============\n~s== END ==============\n"
            ,[Module, print_debugg(Fields,Data,[])]).

   print_debugg([],_,Acc) -> lists:reverse(Acc);
   print_debugg([Field|Fields],[D|Data],Acc) ->
    Txt = io_lib:format("==\t~w:\t~p\n",[Field,D]),
    print_debugg(Fields,Data,[Txt|Acc]).

err_500(Msg,Req) -> err_cust(Msg, 500, Req).
err_404(Msg,Req) -> err_cust(Msg, 404, Req).
err_422(Msg,Req) -> err_cust(Msg, 422, Req).
err_403(Msg,Req) -> err_cust(Msg, 403, Req).

err_cust(Msg, Status, Req) when is_list(Msg) ->
    err_cust(list_to_binary(Msg), Status, Req);
err_cust(Msg, Status, Req) ->
    dps:debug("~p: ~s",[Status,Msg]),
    Body = Msg,
    R1 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/html">>,Req),
    R2 = cowboy_req:set_resp_header(<<"content-length">>, byte_size(Body), R1),
    R3 = cowboy_req:set_resp_body(Body,R2),
    cowboy_req:reply(Status,R3).


get_client_id(Req) ->
    case cowboy_req:header(<<"x-forwarded-for">>, Req, undefined) of
        undefined -> {{A,B,C,D},_} = cowboy_req:peer(Req),
                    P = <<".">>,
                    list_to_binary(lists:concat([A,".",B,".",C,".",D]));
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

getnext(N,L,C) -> getsibling(next, N,L,C).
getprev(N,L,C) -> getsibling(prev, N,L,C).

getsibling(Dir, Number, Lang, Collection) ->
    PathFun = fun(_I) ->
        original_path(_I, Lang, Collection)
        end,
    IncrFun = fun(_I) -> 
                _Iint = erlang:binary_to_integer(_I),
                case Dir of
                    next -> erlang:integer_to_binary(_Iint + 1);
                    prev -> erlang:integer_to_binary(_Iint - 1)
                end end,
    getter(IncrFun(Number), IncrFun, PathFun,0).

getter(_,_, _, ?SIBLING_SEARCH_RANGE) -> undefined;
getter(I,IncrFun, PathFun,Tries) ->
    case filelib:is_file(PathFun(I)) of
        true -> I;
        false -> getter(IncrFun(I),IncrFun,PathFun, Tries + 1)
    end.

%% Source : http://stackoverflow.com/users/2760050/himangshuj @ http://stackoverflow.com/a/23587192
bin2hex(Bin) ->
    << <<Y>> ||<<X:4>> <= Bin, Y <- integer_to_list(X,16)>>.
hex2bin(Hex) ->
    <<<<Z>> || <<X:8,Y:8>> <= Hex,Z <- [binary_to_integer(<<X,Y>>,16)]>>.

get_hostbaseuri(R,Base) ->
    LnCode = cowboy_req:binding(ln,R),
    get_hostbaseuri(R,Base,LnCode).

get_hostbaseuri(R,Base,LnCode) ->
    case cowboy_req:header(<<"x-base-url">>,R,undefined) of
        undefined -> Port = case cowboy_req:port(R) of
                             80 -> <<"">>;
                             Oth -> I2B = integer_to_binary(Oth),
                                    <<":",I2B/binary>> end,
                     Host = cowboy_req:host(R),
                     <<"http://",Host/binary,Port/binary,Base/binary, "/", LnCode/binary>>;
        BaseURL -> <<BaseURL/binary, "/", LnCode/binary>>
    end.

params_get(Pars,R) ->
    params_get(Pars,cowboy_req:parse_qs(R),#{}).

    params_get([],_,Acc) -> Acc;
    params_get([H|T],QS,Acc) when is_atom(H) ->
        Key = list_to_binary(atom_to_list(H)),
        NAcc = Acc#{ H => proplists:get_value(Key,QS,undefined)},
        %NAcc = [{H,proplists:get_value(Key,QS,undefined)} | Acc],
        params_get(T,QS,NAcc).

link_factory(Def, R ,Opts) ->
    Base = get_hostbaseuri(R,maps:get(base,Opts)),
    lists:map(fun(K) ->
                Lnk = maps:get(K, Def),
                #{<<"rel">> => K, <<"href">> => <<Base/binary, Lnk/binary>>}
                end
             ,maps:keys(Def)).

filter_filelist(adv,In) ->
    {ok,Re} = re:compile("^[0-9]*\.(pdf|PDF)$"),
    lists:filter(fun(Name) ->
                    Res = re:run(Name,Re),
                    Res =/= nomatch
                    end
                ,In).


get_env_def() ->
    case application:get_env(dpserv,env) of
        {ok,Env} -> {ok,Def} = application:get_env(dpserv,Env),
                    Def;
        _ -> {ok,Def} = application:get_env(dpserv,prod),
             Def
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

filterFiles_test_() ->
    L = ["47345_project.pdf","60348.pdf","48805_project.pdf", "48805.pdf","47351_project.pdf","53605.pdf"],
    [?_assertEqual(["60348.pdf","48805.pdf","53605.pdf"], dpserv_tools:filter_filelist(adv,L))
    ].
-endif.
