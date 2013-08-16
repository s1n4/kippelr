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
5> {ok, {201, _}} = kippelr:create_clip(Clip).
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
      {<<"resource_uri">>,<<"/api/clips/16449"...>>}]}}.
```
