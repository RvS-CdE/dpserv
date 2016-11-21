-module(dpserv_handler).
-behaviour(cowboy_rest).

-include("config.hrl").
-include("common_functions.hrl").
-include_lib("kernel/include/file.hrl").

-export([init/2
        ,allowed_methods/2
        ,allow_missing_post/2
        ,resource_exists/2
        ,content_types_provided/2
        ,expires/2
        ,is_authorized/2
        ,generate_etag/2
        ,known_methods/2
        ,last_modified/2
        ,multiple_choices/2
        ]).

-export([to_pdf/2
        ,to_text/2
        ,to_html/2
        ,terminate/3
        ]).

-record(state, {opts, adv_id, ext, client}).
-define(CL, S#state.client).

init(Req, Opts) ->
    Number = cowboy_req:binding(number,Req),
    Client = number_prefix(Number),
    dps:info("~s: Rest query initializing, serving ~s",[Client,Number]),
    {cowboy_rest, Req, #state{opts = Opts, adv_id = number_prefix(Number), ext = number_suffix(Number), client = Client }}.

allowed_methods(Req, S) ->
    dps:debug("~s: Allowed_methods",[?CL]),
    {[<<"GET">>], Req, S}.

allow_missing_post(Req, S) ->
    {false, Req,S}.

resource_exists(Req, S) ->
    dps:debug("~s: Resource exist check (state: ~p)",[?CL,S]),
    OPath = original_path(S#state.adv_id,maps:get(lang,S#state.opts), maps:get(collection,S#state.opts)),
    case filelib:is_file(OPath) of
        true -> {true, Req,S };
        false -> R = ?ERR_404(<<"Just Not There">>,Req),
                 {stop,R,S}
        %false -> {stop, ?ERR_404(<<"Just Not There">>,Req), S}
    end.

expires(Req,S) ->
    {LD, LT} = calendar:local_time(),
    ED = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(LD) + ?EXPIRATION_DAYS),
    dps:debug("~s: Expires = ~p",[?CL, ED]),
    {{ED,LT}, Req, S}.

generate_etag(Req,S) ->
    OPath = original_path(S#state.adv_id,maps:get(lang,S#state.opts), maps:get(collection,S#state.opts)),
    {ok,I} = file:read_file_info(OPath),
    Etag = {strong, integer_to_binary(erlang:phash2({I#file_info.size, I#file_info.mtime}, 16#ffffff))},
    dps:debug("~s: Etag = ~p",[?CL, Etag]),
    {Etag,Req,S}.

known_methods(Req,S) ->
    {[<<"GET">>,<<"OPTIONS">>], Req,S}.

last_modified(Req,S) ->
    OPath = original_path(S#state.adv_id,maps:get(lang,S#state.opts), maps:get(collection,S#state.opts)),
    {ok,I} = file:read_file_info(OPath),
    {I#file_info.ctime, Req,S}.

multiple_choices(Req,S) ->
    {true,Req,S}.

is_authorized(Req,S) ->
    Auth = case cowboy_req:method(Req) of
            <<"GET">> -> true;
            _ -> {false, <<"Sorry, bad method: Thanks for trying !">>} end,
    dps:debug("~s: Authorized = ~p",[?CL, Auth]),
    {Auth, Req, S}.

content_types_provided(Req, State) ->
    {[
        {<<"application/pdf">>, to_pdf}
%       ,{{<<"text">>, <<"plain">>, []}, to_text}
%       ,{{<<"text">>, <<"html">>, []}, to_html}
    ], Req, State}.

to_pdf(Req,S) ->
    OPath = original_path(S#state.adv_id,maps:get(lang,S#state.opts), maps:get(collection,S#state.opts)),
    Out = case file:read_file(OPath) of
            {ok, Bin} -> Bin;
            {error, enoent} -> throw({dpserv,nosuchfile,OPath});
            {error, R} -> throw({dpserv,filereaderror,[R,OPath]})
          end,
    {Out, Req, S}.

terminate(normal,_Req,_State) ->
    ok;
    terminate({throw,{dpserv,Err,Data}},_Req,S) ->
        dps:error("~s: dpserv experienced error ~p\n\tData: ~p\n\tState: ~p\n",[?CL,Err,Data,S]),
        ok;
    terminate(Reason,_Req,S) ->
        dps:error("~s: dpserv handler terminating because \"~p\"\n",[?CL,Reason]),
        ok.

to_text(Req,State) ->
    {<<"This should be a pure text version.">>, Req, State}.

to_html(Req,State) ->
    {<<"This should be a html version.">>, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
-endif.
