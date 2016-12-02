-module(dpserv_store).
%% Placeholder interface to the file store.
%% A decent, buffered, gen_server should take the job over later on.

-include("config.hrl").
-include("common_functions.hrl").

-export([last_modified/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% exported API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
last_modified(adv) ->
    RPath = list_to_binary(?GET_ENV(srv_dir)),
    filelib:last_modified(RPath).
