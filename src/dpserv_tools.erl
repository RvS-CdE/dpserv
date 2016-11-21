-module(dpserv_tools).

-export([cfg_get/1
        ,print_debug/3
        ,err_404/2
        ]).

cfg_get(Key) ->
    case application:get_env(dpserv, Key) of
        {ok,Val} -> Val;
        undefined -> throw({dpserv,undefined_cfg,Key})
    end.

print_debug(Module,Fields,Data) ->
    io:format("\n== DEBUGGING in ~s ============\n~s== END ==============\n"
            ,[Module, print_debugg(Fields,Data,[])]).

   print_debugg([],_,Acc) -> lists:reverse(Acc);
   print_debugg([Field|Fields],[D|Data],Acc) ->
    Txt = io_lib:format("==\t~w:\t~p\n",[Field,D]),
    print_debugg(Fields,Data,[Txt|Acc]).

err_404(Msg,Req) ->
    dps:debug("404: ~s",[Msg]),
    Body = Msg,
    R1 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>,Req),
    R2 = cowboy_req:set_resp_header(<<"content-length">>, byte_size(Body), R1),
    R3 = cowboy_req:set_resp_body(Body,R2),
    cowboy_req:reply(404,R3).
