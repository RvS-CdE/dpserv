-module(dpserv_handler).
-behaviour(cowboy_rest).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

-export([to_pdf/2
        ,to_text/2
        ,to_html/2
        ]).

-record(state, {opts}).

init(Req, Opts) ->
    io:format("\n - Rest INIT",[]),
    {cowboy_rest, Req, #state{opts = Opts}}.

allowed_methods(Req, State) ->
    io:format("\n - Allowed Methods",[]),
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"pdf">>, []}, to_pdf}
%       ,{{<<"text">>, <<"plain">>, []}, to_text}
%       ,{{<<"text">>, <<"html">>, []}, to_html}
    ], Req, State}.

to_pdf(Req,State) ->
    io:format("~p\n",[State]),
    {<<"ok">>, Req, State}.

to_text(Req,State) ->
    {<<"This should be a pure text version.">>,Req, State}.

to_html(Req,State) ->
    {<<"This should be a html version.">>,Req, State}.
