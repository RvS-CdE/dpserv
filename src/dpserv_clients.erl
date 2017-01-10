-module(dpserv_clients).
%% Manages API client keys, work in progress

-include("config.hrl").
-include("clients.hrl").
-include("common_functions.hrl").

-export([keyToID/2      %% Convert ApiKey and Nonce to a useable ID
        ,keyToID/1      %% Search for a client ID
        ,settings/1     %% Read settings for a specific client
        ,newNonce/0     %% Generate a new Nonce
        ,newClient/1    %% Generate Nonce and ID for new client
        ,newClient/2    %% Generate Nonce and ID for new client, with provided IP (extra salty!)
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXPORTED API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

keyToID(ApiKey) ->
    Valid = lists:filter(fun({{H,N},_}) -> {H,N} =:= keyToID(ApiKey,N) end
                        ,?CLIENTS),
    case Valid of
        [{ID,_}|_] -> ID;
        [] -> undefined
    end.

keyToID(ApiKey,Nonce) ->
    H = crypto:hash(?CLIENT_HASH_ALG, <<ApiKey/binary, Nonce/binary>>),
    {base64:encode(H),Nonce}.

settings(ID) ->
    proplists:get_value(ID,?CLIENTS).

newNonce() ->
   base64:encode(crypto:strong_rand_bytes(12)).

newClient(ApiKey) ->
    keyToID(ApiKey,newNonce()).

newClient(ApiKey,Ip) ->
    newClient(<<ApiKey/binary,Ip/binary>>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UNIT TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(TEST_KEY,<<"test_key">>).
-define(TEST_NONCE,<<"4SLBnyxA5rWrG70f">>).
-define(TEST_HASH,<<"JvkMzq7+gx1mOtfi0kdqBBbLqGhd/JRHYtPPmf2zG03JToQrqY9JVM/vRMATLyFbNiRPuENHlynOSbQrqd/4Mg==">>).

keyToID_test_() ->
    [?_assertEqual(undefined,keyToID(<<"poulet_jaune">>))
    ,?_assertEqual({?TEST_HASH,?TEST_NONCE},keyToID(?TEST_KEY,?TEST_NONCE))
    ,?_assertEqual({?TEST_HASH,?TEST_NONCE},keyToID(?TEST_KEY))
    ].

settings_test_() ->
    [?_assert(is_map(settings({?TEST_HASH,?TEST_NONCE})))
    ].

-endif.
