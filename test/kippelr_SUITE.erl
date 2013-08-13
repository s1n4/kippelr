-module(kippelr_SUITE).

%% Common Test callbacks
-export([all/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% test cases
-export([basic_auth/1]).
-export([token_auth/1]).
-export([account_info/1]).
-export([gcs/1]).
-export([gcsfs/1]).
-export([gcsf/1]).
-export([gc/1]).
-export([gcc/1]).
-export([gcl/1]).
-export([fav/1]).
-export([unfav/1]).
-export([like/1]).
-export([unlike/1]).
-export([c_crud/1]).
-export([notifications/1]).
-export([gls/1]).
-export([gl/1]).
-export([glcs/1]).
-export([ifl/1]).
-export([fl/1]).
-export([ufl/1]).
-export([l_crud/1]).


all() ->
    [
     {group, auth}, {group, account}, {group, clips}, {group, notifications},
     {group, lists}
    ].

groups() ->
    [
     {auth, [], [basic_auth, token_auth]},
     {account, [], [account_info]},
     {clips, [], [gcs, gcsfs, gcsf, gc, gcc, gcl, fav, unfav, like, unlike,
                  c_crud]},
     {notifications, [], [notifications]},
     {lists, [], [gls, gl, glcs, ifl, fl, ufl, l_crud]}
    ].

init_per_group(_, Config) ->
    kippelr:start(),
    Username = os:getenv("KIPPT_USER"),
    Token = os:getenv("KIPPT_TOKEN"),
    ok = kippelr:auth({token_auth, {Username, Token}}),
    Config.

end_per_group(_, _Config) ->
    kippelr:stop().

init_per_testcase(_, Config) ->
    ct:sleep(2000),
    Config.

end_per_testcase(_, _Config) ->
    ok.

basic_auth(_) ->
    ok = kippelr:auth({basic_auth, {"username", "password"}}),
    false = kippelr:is_authenticated().

token_auth(_) ->
    Username = os:getenv("KIPPT_USER"),
    Token = os:getenv("KIPPT_TOKEN"),
    ok = kippelr:auth({token_auth, {Username, Token}}),
    true = kippelr:is_authenticated().

account_info(_) ->
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

fav(_) ->
    {ok, {200, _}} = kippelr:favorite(16332396).

unfav(_) ->
    {ok, {200, _}} = kippelr:unfavorite(16332396).

like(_) ->
    {ok, {400, _}} = kippelr:like(16332396).

unlike(_) ->
    {ok, {400, _}} = kippelr:unlike(16332396).

c_crud(_) ->
    Clip = "{\"url\":\"https://github.com/s1n4/kippelr\","
        "\"notes\":\"Erlang library for the Kippt API\"}",
    Clip1 = "{\"url\":\"https://github.com/s1n4/kippelr\","
        "\"notes\":\"Erlang library for the Kippt API.\"}",
    Comment = "{\"body\":\"test...\"}",
    {ok, {201, Result}} = kippelr:create_clip(Clip),
    Id = proplists:get_value(<<"id">>, Result),
    {ok, {200, Result1}} = kippelr:modify_clip(Id, Clip1),
    Id1 = proplists:get_value(<<"id">>, Result1),
    Id = Id1,
    ct:sleep(2000),
    {ok, {200, Result2}} = kippelr:get_clip(Id1),
    {ok, {201, Result3}} = kippelr:create_comment(Id, Comment),
    Id2 = proplists:get_value(<<"id">>, Result3),
    {ok, {204, _}} = kippelr:delete_clip_comment(Id, Id2),
    {ok, {204, _}} = kippelr:delete_clip(Id).

notifications(_) ->
    {ok, {200, _}} = kippelr:notifications(),
    {ok, {200, _}} = kippelr:mark_notifications_as_read().

gls(_) ->
    {ok, {200, _}} = kippelr:get_lists().

gl(_) ->
    {ok, {200, _}} = kippelr:get_list(inbox).

glcs(_) ->
    {ok, {200, _}} = kippelr:get_list_clips(inbox).

ifl(_) ->
    {ok, {200, _}} = kippelr:is_following_list(inbox).

fl(_) ->
    {ok, {200, _}} = kippelr:follow_list(inbox).

ufl(_) ->
    {ok, {200, _}} = kippelr:unfollow_list(inbox).

l_crud(_) ->
    List = "{\"title\":\"kippelr\",\"description\":\"test..\"}",
    List1 = "{\"title\":\"kippelr\",\"description\":\"test1...\"}",
    {ok, {201, Result}} = kippelr:create_list(List),
    ct:sleep(2000),
    Id = proplists:get_value(<<"id">>, Result),
    {ok, {200, Result1}} = kippelr:modify_list(Id, List1),
    {ok, {204, _}} = kippelr:delete_list(Id).
