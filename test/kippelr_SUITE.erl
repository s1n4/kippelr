-module(kippelr_SUITE).

%% Common Test callbacks
-export([all/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% test cases
-export([basic_auth/1]).
-export([is_auth/1]).
-export([token_auth/1]).


all() ->
    [{group, auth}].

groups() ->
    [{auth, [], [basic_auth, is_auth, token_auth]}].

init_per_group(_, Config) ->
    kippelr:start(),
    Config.

end_per_group(_, _Config) ->
    kippelr:stop().

basic_auth(_) ->
    ok = kippelr:auth({basic_auth, {"username", "password"}}),
    false = kippelr:is_authenticated().

is_auth(_) ->
    false = kippelr:is_authenticated().

token_auth(_) ->
    ok = kippelr:auth({token_auth, {"username", "token"}}),
    false = kippelr:is_authenticated().
