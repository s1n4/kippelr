# kippelr [![Build Status](https://travis-ci.org/s1n4/kippelr.png?branch=master)](https://travis-ci.org/s1n4/kippelr)

Erlang library for the Kippt API

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

## Index
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
kippelr:auth({basic_auth, {"username", "password"}})
```

#### api token

```erlang
kippelr:auth({token_auth, {"username", "api_token"}})
```

### account

#### account/0

```erlang
account() -> result()
```

#### is_authenticated/0

```erlang
is_authenticated() -> boolean()
```

### users

#### get_user/1

```erlang
get_user(UserId :: id()) -> result()
```

#### get_user_clips/1

```erlang
get_user_clips(UserId :: id()) -> result()
```

#### get_user_favorites/1

```erlang
get_user_favorites(UserId :: id()) -> result()
```

#### get_user_likes/1

```erlang
get_user_likes(UserId :: id()) -> result()
```

#### get_user_followers/1

```erlang
get_user_followers(UserId :: id()) -> result()
```
