# kippelr [![Build Status](https://travis-ci.org/s1n4/kippelr.png?branch=master)](https://travis-ci.org/s1n4/kippelr)

Erlang library for the Kippt API

## Requirement

  * Erlang/OTP R14 or newer

## Build

```
./rebar get-deps compile
```

## QuickStart

```
erl -pa ebin deps/*/ebin -s kippelr
```

```erlang
1> kippelr:auth({basic_auth, {"username", "password"}}).
ok
2> true = kippelr:is_authenticated().
true
3> {ok, {200, Clips}} = kippelr:get_clips().
{ok,{200,
     [{<<"meta">>,
      [{<<"next">>,null},
       {<<"total_count">>,8},
       {<<"previous">>,null},
       {<<"limit">>,20},
       {<<"offset">>,0}]},
       {<<"objects">>, ...}]}
4> Clip = [{<<"url">>, <<"https://github.com/s1n4/kippelr">>}].
[{<<"url">>,<<"https://github.com/s1n4/kippelr">>}]
5> {ok, {201, Clip1}} = kippelr:create_clip(Clip).
{ok,{201,
     [{<<"via">>,null},
      {<<"saves">>,[{<<"count">>,0},{<<"data">>,[]}]},
      {<<"favicon_url">>,
       <<"https://www.google.com/s2/u/0/favicons?domain=github.com">>},
      {<<"is_favorite">>,false},
      {<<"likes">>,[{<<"count">>,0},{<<"data">>,[]}]},
      {<<"canonical_url">>,<<"https://github.com/s1n4/kippelr">>},
      {<<"app_url">>,<<"/s1n4/inbox/clips/16449765">>},
      {<<"title">>,<<"s1n4/kippelr . GitHub"/utf8>>},
      {<<"comments">>,[{<<"count">>,0},{<<"data">>,[]}]},
      {<<"id">>,16449765},
      {<<"type">>,<<"link">>},
      {<<"updated">>,1376637404},
      {<<"user">>,
       [{<<"username">>,<<"s1n4">>},
        {<<"bio">>,<<"Ruby, Python, JS and Erl"...>>},
        {<<"app_url">>,<<"/s1n4">>},
        {<<"avatar_url">>,<<"https://d19weqih"...>>},
        {<<"twitter">>,<<"sinasamavati">>},
        {<<"id">>,99157},
        {<<"github">>,<<"s1n4">>},
        {<<"webs"...>>,<<>>},
        {<<...>>,...},
        {...}|...]},
      {<<"url_domain">>,<<"github.com">>},
      {<<"created">>,1376637404},
      {<<"url">>,<<"https://github.com/s1n4/kipp"...>>},
      {<<"notes">>,null},
      {<<"list">>,<<"/api/lists/453505/">>},
      {<<"resource_uri">>,<<"/api/clips/16449"...>>}]}}
6> ClipId = proplists:get_value(<<"id">>, Clip1).
16449765
7> Comment = [{<<"body">>, <<"comment goes here">>}].
[{<<"body">>,<<"comment goes here">>}]
8> kippelr:create_comment(ClipId, Comment).
{ok,{201,
     [{<<"body">>,<<"comment goes here">>},
      {<<"created">>,1376638144},
      {<<"id">>,14663},
      {<<"resource_uri">>,
       <<"/api/clips/16449765/comments/14663/">>},
      {<<"user">>,
       [{<<"username">>,<<"s1n4">>},
        {<<"bio">>,
         <<"Ruby, Python, JS and Erlang for the rest of my time.\r\nI "...>>},
        {<<"app_url">>,<<"/s1n4">>},
        {<<"avatar_url">>,
         <<"https://d19weqihs4yh5u.cloudfront.net/avatars/db"...>>},
        {<<"twitter">>,<<"sinasamavati">>},
        {<<"id">>,99157},
        {<<"github">>,<<"s1n4">>},
        {<<"website_url">>,<<>>},
        {<<"full_name">>,<<"Sina Samavati">>},
        {<<"dribbble">>,<<>>},
        {<<"counts">>,[{<<"follows">>,-5},{<<"followed"...>>,-5}]},
        {<<"is_pro">>,false},
        {<<"resource_uri">>,<<"/api/users/9"...>>}]}]}}
9> kippelr:delete_clip(ClipId).
{ok,{204,[]}}
```

## Start application

First off, start kippelr application

```erlang
application:start(crypto),
application:start(public_key),
application:start(ssl),
application:start(inets),
application:start(kippelr).
```

## Index

All these belong to the [kippelr](src/kippelr.erl) module.

  * [Data types](#Data-types)
    - [status()](#status)
    - [json_term()](#json_term)
    - [id()](#id)
    - [result()](#result)
  * [Authentication](#Authentication)
    - [basic auth](#basic-auth)
    - [api token](#api-token)
  * [Endpoints](#Endpoints)
    - [account](#account)
    - [users](#users)
    - [clips](#clips)
    - [lists](#lists)
    - [notifications](#notifications)


### Data types

#### status()

```erlang
status() = 200 | 201 | 204 | 400 | 401 | 404
```

#### json_term()
> [jsx:json_term()](https://github.com/talentdeficit/jsx#json_term)

```erlang
json_term() = [json_term()]
    | [{binary() | atom(), json_term()}]
    | true
    | false
    | null
    | integer()
    | float()
    | binary()
```

#### id()

```erlang
id() = integer() | string()
```

#### result()

```erlang
result() = {ok, {status(), json_term()}}
```

### Authentication

#### basic auth

```erlang
auth({basic_auth, {Username :: string(), Password :: string()}}) -> ok
```

#### api token

```erlang
auth({token_auth, {Username :: string(), ApiToken :: string()}}) -> ok
```

### account

#### account/0

get logged-in user's profile

```erlang
account() -> result()
```

#### is_authenticated/0

check if authorization succeeds

```erlang
is_authenticated() -> boolean()
```

### users

#### get_user/1

get public user data based on user's ID or username

```erlang
get_user(UserId :: id()) -> result()
```

#### get_user_clips/1

get a user's public clips

```erlang
get_user_clips(UserId :: id()) -> result()
```

#### get_user_favorites/1

get a user's public favorites

```erlang
get_user_favorites(UserId :: id()) -> result()
```

#### get_user_likes/1

get a user's public likes

```erlang
get_user_likes(UserId :: id()) -> result()
```

#### get_user_followers/1

get a user's followers

```erlang
get_user_followers(UserId :: id()) -> result()
```

#### get_user_lists/1

get a user's public lists

```erlang
get_user_lists(UserId :: id()) -> result()
```

#### get_user_list/2

get a user's public list

```erlang
get_user_list(UserId :: id(), ListId :: id()) -> result()
```

#### is_following_user/1

get information about a relationship to another user

```erlang
is_following_user(UserId :: id()) -> result()
```

#### follow_user/1

follow a user

```erlang
follow_user(UserId :: id()) -> result()
```

#### unfollow_user/1

unfollow a user

```erlang
unfollow_user(UserId :: id()) -> result()
```

#### search_users/1

search for users

```erlang
search_users(Params :: string()) -> result()

%% e.g search_users("Sina Samavati")
```

### clips

#### get_clips/0

get logged-in user's clips

```erlang
get_clips() -> result()
```

#### get_clips_favorites/0

get logged-in user's favorites

```erlang
get_clips_favorites() -> result()
```

#### get_clips_feed/0

get logged-in user's feed

```erlang
get_clips_feed() -> result()
```

#### get_clip/1

get a clip

```erlang
get_clip(ClipId :: integer) -> result()
```

#### get_clip_comments/1

get a clip's comments

```erlang
get_clip_comments(ClipId :: integer()) -> result()
```

#### get_clip_likes/1

get a clip's likes

```erlang
get_clip_likes(ClipId :: integer()) -> result()
```

#### search_clips/1

search for clips

```erlang
search_clips(Params :: string()) -> result()

%% e.g search_clips("erlang lib")
```

#### delete_clip/1

delete a clip

```erlang
delete_clip(ClipId :: integer()) -> result()
```

#### delete_clip_comment/1

delete a comment

```erlang
delete_clip_comment(ClipId :: integer(), CommentId :: integer()) -> result()
```

#### favorite/1

favorite a clip

```erlang
favorite(ClipId :: integer()) -> result()
```

#### unfavorite/1

unfavorite a clip

```erlang
unfavorite(ClipId :: integer()) -> result()
```

#### like/1

like a clip

```erlang
like(ClipId :: integer()) -> result()
```

#### unlike/1

unlike a clip

```erlang
unlike(ClipId :: integer()) -> result()
```

#### create_clip/1

create a clip

```erlang
create_clip(Clip :: json_term()) -> result()

%% e.g create_clip([{<<"url">>, <<"https://github.com/s1n4/kippelr">>}])
```

#### create_comment/2

leave a comment on a clip

```erlang
create_comment(ClipId :: integer(), Comment :: json_term()) -> result()

%% e.g create_comment(ClipId, [{<<"body">>, <<"comment body..">>}]) -> result()
```

#### modify_clip/2

modify a clip

```erlang
modify_clip(ClipId :: integer(), Clip :: json_term()) -> result()
```

### lists

#### get_lists/0

get logged-in user's lists

```erlang
get_lists() -> result()
```

#### get_list/1

get a list

```erlang
get_list(ListId :: id()) -> result()
```

#### get_list_clips/1

get clips for a list

```erlang
get_list_clips(ListId :: id()) -> result()
```

#### is_following_list/1

get information about a relationship to a list

```erlang
is_following_list(ListId :: id()) -> result()
```

#### follow_list/1

follow a list

```erlang
follow_list(ListId :: id()) -> result()
```

#### unfollow_list/1

unfollow a list

```erlang
unfollow_list(ListId :: id()) -> result()
```

#### create_list/1

create a list

```erlang
create_list(ListObject :: json_term()) -> result()

%% e.g create_list([{<<"title">>, <<"erlang">>}])
```

#### modify_list/1

modify a list

```erlang
modify_list(ListObject :: json_term()) -> result()
```

#### delete_list/1

delete a list

```erlang
delete_list(ListId :: id()) -> result()
```

### notifications

#### notifications/0

get latest notifications

```erlang
notifications() -> result()
```

#### mark_notifications_as_read/0

mark notifications as read

```erlang
mark_notifications_as_read() -> result()
```

## License

MIT, see LICENSE file for more details.
