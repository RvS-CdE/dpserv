%% Client : {{Hash, Nonce}, Settings}
-define(CLIENTS,[{{<<"JvkMzq7+gx1mOtfi0kdqBBbLqGhd/JRHYtPPmf2zG03JToQrqY9JVM/vRMATLyFbNiRPuENHlynOSbQrqd/4Mg==">> ,<<"4SLBnyxA5rWrG70f">>}
                 ,#{html_download => 0
                   ,pdf_download => 0
                   ,txt_download => 0
                   ,desc => "Test client"
                   }}
                ,{{<<"dc2m0kErWTB6F4D0aRNfRC50yNL7TpQMK0Suc2aHAMBvKl87R3lxtuisDFaTdEFKobhxSDXD+wtcB2KwlHD3kA==">> ,<<"l/DCPwJiHSO+PHS3">>}
                  ,#{html_download => 1
                    ,pdf_download => 1
                    ,txt_download => 1
                    ,desc => "Test client 2"
                    }} 
                ,{{<<"1WkhhxMFF0SPt43rH5+aa7Gz6WP7jDRAGEd9nFwp5ARUpD5EFyrbOzQKDzEs9XFUeyluKmtK07ycZ6rYA3BjPg==">>, <<"A97+uWxS1W2Fd/r3">>}
                  ,#{html_download => 0
                    ,pdf_download => 0
                    ,txt_download => '*'
                    ,desc => "Internal import client"
                    }}
                ]).
