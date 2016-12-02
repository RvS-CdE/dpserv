-define(API_VERSION,1).
-define(EXPIRATION_DAYS,7).
-define(SIBLING_SEARCH_RANGE,1000).

-define(LN_TO_CODE(_X),(fun(<<"avis">>) -> fr;
                       (<<"adviezen">>) -> nl;
                       (<<"gutachten">>) -> de end)(_X)).

-define(CODE_TO_LN(_X),(fun(fr) -> <<"avis">>;
                           (nl) -> <<"adviezen">>;
                           (de) -> <<"gutachten">> end)(_X)).

-define(CL, io_lib:format("~s|~s",[S#state.client,S#state.session])).

-define(COLLECTIONS,[<<"all">>,<<"ctime">>]).
-define(COL_DATE_LIMITED,[<<"ctime">>]).
-define(PAGES,[home_page, collection_list]).

%% Limits downloads per minute
-define(SERVICE_LIMITS,#{html_download => 10
                        ,pdf_download => 40
                        ,txt_download => 10}).
