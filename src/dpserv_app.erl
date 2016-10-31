%%%-------------------------------------------------------------------
%% @doc dpserv public API
%% @end
%%%-------------------------------------------------------------------

-module(dpserv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", test_handler, []}
              % Idea: use coll name as var, use constraint to orient
              ,{"/avis/:number",        dpserv_handler,#{collection => adv, lang => fr}}
              ,{"/adviezen/:number",    dpserv_handler,#{collection => adv, lang => nl}}
              ,{"/gutachten/:number",   dpserv_handler,#{collection => adv, lang => de}}

              ,{"/dbx/:collection/:number", dpserv_handler,#{}}
              ]}
    ]),

    {ok, _} = cowboy:start_clear(http_listener, 100,
        [{port,9088}],
        #{env => #{dispatch => Dispatch}}
    ),

    dpserv_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
