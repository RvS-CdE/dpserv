-module(dpserv_h_browsing).
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
               ,page :: atom()
               ,store ::atom()
               ,content :: 'undefined' | map()
               }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MISC OTP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Req,Opts) ->
    S = #state{client = dpserv_tools:get_client_id(Req)
              ,session = dpserv_tools:get_session_id(Req)
              ,opts = Opts
              ,page = maps:get(page,Opts)
              ,store = maps:get(store,Opts)
              },
    %?DBG([state],[S]),
    dpserv_tools:log_session(Req,S#state.session),
    dps:debug("~s: Preparing query for page ~p",[?CL,S#state.page]),
    {cowboy_rest, Req, S}.

terminate(normal,_Req,S) ->
    dps:info("~s: served page ~p",[?CL,S#state.page]),
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
    Out = case lists:member(S#state.page,?PAGES) of
        true -> true;
        false -> dps:warning("~s: Page ~p not available",[?CL,S#state.page]),
                false end,
    {Out, Req,S}.

known_methods(Req,S) ->
    {[<<"GET">>,<<"OPTIONS">>], Req,S}.

charsets_provided(Req,S) ->
    {[<<"utf-8">>], Req,S}.

content_types_provided(Req, S) ->
    Types = [{<<"application/json">>, to_json}
            ],
    NewS = S#state{content = page_make(Req, S)},
    {Types, Req, NewS}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Exported Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_json(Req,S) ->
    Payload = jiffy:encode(S#state.content,[pretty]),
    {Payload, Req, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
page_make(R, S = #state{page = home_page}) ->
    Def = #{<<"self">> => <<"/">>
            ,<<"collections">> => <<"/collections">>
            },
    Base = dpserv_tools:get_hostbaseuri(R, maps:get(base,S#state.opts)),
    #{<<"date">> => iso8601:format(calendar:local_time())
    ,<<"last_update">> => iso8601:format(dpserv_store:last_modified(S#state.store))
    ,<<"api_version">> => ?API_VERSION
    ,<<"req_limiting_triggered">> => false
    ,<<"service_limits">> => ?SERVICE_LIMITS
    ,<<"links">> => dpserv_tools:link_factory(Def,R,S#state.opts)
    ,<<"content">> => [ #{<<"id">> => <<"document_url_mask">>
                         ,<<"placeholder">> => <<"{DOCID}">>
                         ,<<"url_template">> => <<Base/binary,"/{DOCID}/meta">>
                         }]
    };

page_make(R, S = #state{page = collection_list}) ->
    Def = #{<<"self">> => <<"/collections">>},
    Content = lists:map(fun(Col) ->
                             CDef = #{<<"self">> => <<"/col/", Col/binary>>},
                             #{<<"id">> => Col
                              ,<<"links">> => dpserv_tools:link_factory(CDef, R, S#state.opts)
                              }
                             end
                          ,?COLLECTIONS),
    #{<<"colllection_count">> => length(?COLLECTIONS)
     ,<<"collections">> => ?COLLECTIONS
     ,<<"time_constrained">> => ?COL_DATE_LIMITED
     ,<<"links">> => dpserv_tools:link_factory(Def,R, S#state.opts)
     ,<<"content">> => Content
     }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Testing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
