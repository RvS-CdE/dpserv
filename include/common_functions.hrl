-define(GET_ENV, dpserv_tools:cfg_get).


-define(DBG(F12,D12),fun() ->  {current_function, {_M5, _F5, _A5}} = process_info(self(), current_function),
                           dpserv_tools:print_debug(io_lib:format("~p:~p/~p",[_M5,_F5,_A5]),F12,D12) end()).

-define(ERR_403,dpserv_tools:err_403).
-define(ERR_404,dpserv_tools:err_404).
-define(ERR_422,dpserv_tools:err_422).
-define(ERR_500,dpserv_tools:err_500).
-define(SH, cowboy_req:set_resp_header).
