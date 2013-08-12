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
-export([gcs/1]).
-export([gcsfs/1]).
-export([gcsf/1]).
-export([gc/1]).
-export([gcc/1])
-export([gcl/1]).


all() ->
    [{group, auth}, {group, account}, {group, clips}].

groups() ->
    [
     {auth, [], [basic_auth, is_auth, token_auth]},
     {account, [], [account_info]},
     {clips, [], [gcs, gcsfs, gcsf, gc, gcc, gcl]}
    ].

init_per_group(clips, Config) ->
    kippelr:start(),
    Username = os:getenv("KIPPT_USER"),
    Token = os:getenv("KIPPT_TOKEN"),
    ok = kippelr:auth({token_auth, {Username, Token}}),
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
    Username = os:getenv("KIPPT_USER"),
    Token = os:getenv("KIPPT_TOKEN"),
    ok = kippelr:auth({token_auth, {Username, Token}}),
    true = kippelr:is_authenticated().

account_info(_) ->
    Username = os:getenv("KIPPT_USER"),
    Token = os:getenv("KIPPT_TOKEN"),
    ok = kippelr:auth({token_auth, {Username, Token}}),
    {ok, {200, _}} = kippelr:account().

gcs(_) ->
    {ok, {200, _}} = kippelr:get_clips().

gcsfs(_) ->
    {ok, {200, _}} = kippelr:get_clips_favorites().

gcsf(_) ->
    {ok, {200, _}} = kippelr:get_clips_feed().

gc(_) ->
    {ok, {200, _}} = kippelr:get_clip(16332396).

gcc(_) ->
    {ok, {200, _}} = kippelr:get_clip_comments(16332396).

gcl(_) ->
    {ok, {200, _}} = kippelr:get_clip_likes(16332396).
