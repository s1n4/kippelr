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
-export([account_info/1]).
-export([gc/1]).
-export([gcf/1]).


all() ->
    [{group, auth}, {group, account}, {group, clips}].

groups() ->
    [
     {auth, [], [basic_auth, is_auth, token_auth]},
     {account, [], [account_info]},
     {clips, [], [gc, gcf]}
    ].

init_per_group(clips, Config) ->
    kippelr:start(),
    Token = os:getenv("KIPPT_TOKEN"),
    ok = kippelr:auth({token_auth, {"s1n4", Token}}),
    Config;
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
    Token = os:getenv("KIPPT_TOKEN"),
    ok = kippelr:auth({token_auth, {"s1n4", Token}}),
    true = kippelr:is_authenticated().

account_info(_) ->
    Token = os:getenv("KIPPT_TOKEN"),
    ok = kippelr:auth({token_auth, {"s1n4", Token}}),
    {ok, {200, _}} = kippelr:account().

gc(_) ->
    {ok, {200, _}} = kippelr:get_clips().

gcf(_) ->
    {ok, {200, _}} = kippelr:get_clips_favorites().
