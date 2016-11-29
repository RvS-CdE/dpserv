-define(EXPIRATION_DAYS,7).
-define(SIBLING_SEARCH_RANGE,1000).

-define(LN_TO_CODE(_X),(fun(<<"avis">>) -> fr;
                       (<<"adviezen">>) -> nl;
                       (<<"gutachten">>) -> de end)(_X)).

-define(CODE_TO_LN(_X),(fun(fr) -> <<"avis">>;
                           (nl) -> <<"adviezen">>;
                           (de) -> <<"gutachten">> end)(_X)).

