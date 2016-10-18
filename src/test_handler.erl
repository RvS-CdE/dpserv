-module(test_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(Req0, _Opts) ->
    Req = cowboy_req:reply(200,
            #{<<"content-type">> => <<"text/plain">>},
            <<"Test succeeded, again !">>,
            Req0),
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {ok, Req2} = cowboy_req:reply(200, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
