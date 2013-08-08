-define(KIPPT, "https://kippt.com/api/").
-define(ACCOUNT, ?KIPPT ++ "account/").
-define(KIPPT_BASIC_AUTH, "https://~p:~p@kippt.com/api/").
-define(TIMEOUT, 20000).

-record(state, {url, headers}).
