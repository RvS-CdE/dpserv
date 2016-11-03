-module(dpserv_handler).
-behaviour(cowboy_rest).

-include("common_functions.hrl").

-export([init/2
        ,allowed_methods/2
        ,resource_exists/2
        ,content_types_provided/2
        ]).

-export([to_pdf/2
        ,to_text/2
        ,to_html/2
        ,terminate/3
        ]).

-record(state, {opts, adv_id}).

init(Req, Opts) ->
    io:format("\n - Rest INIT",[]),
    AdvId = cowboy_req:binding(number,Req),
    {cowboy_rest, Req, #state{opts = Opts, adv_id = AdvId}}.

allowed_methods(Req, State) ->
    io:format("\n - Allowed Methods",[]),
    {[<<"GET">>], Req, State}.

resource_exists(Req, S) ->
    io:format("\n - Resource exist check",[]),
    OPath = original_path(S#state.adv_id),
    Result = filelib:is_file(OPath),
    ?DBG([opath,exists],[OPath,Result]),
    {Result, Req, S}.


content_types_provided(Req, State) ->
    {[
        {<<"application/pdf">>, to_pdf}
%       {{<<"application">>, <<"pdf">>, []}, to_pdf}
%       ,{{<<"text">>, <<"plain">>, []}, to_text}
%       ,{{<<"text">>, <<"html">>, []}, to_html}
    ], Req, State}.

to_pdf(Req,S) ->
    OPath = original_path(S#state.adv_id),
    Out = case file:read_file(OPath) of
            {ok, Bin} -> Bin;
            {error, enoent} -> throw({dpserv,nosuchfile,OPath});
            {error, R} -> throw({dpserv,filereaderror,[R,OPath]})
          end,
    {Out, Req, S}.

terminate(normal,_Req,_State) ->
    ok;
    terminate({throw,{dpserv,Err,Data}},_Req,S) ->
        io:format(" !! dpserv experienced error ~p\n\tData: ~p\n\tState: ~p\n",[Err,Data,S]),
        ok;
    terminate(Reason,_Req,_State) ->
        io:format("dpserv handler terminating because \"~p\"\n",[Reason]),
        ok.

to_text(Req,State) ->
    {<<"This should be a pure text version.">>,Req, State}.

to_html(Req,State) ->
    {<<"This should be a html version.">>,Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
original_path(Number) when is_binary(Number) ->
    RPath = list_to_binary(?GET_ENV(srv_dir)),
    <<RPath/binary, "/", Number/binary, ".pdf">>.
