-module(dpserv_tools).

-export([cfg_get/1
        ,print_debug/3
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
