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
kippelr:auth({basic_auth, {"username", "password"}}).
true = kippelr:is_authenticated().
{ok, {200, Clips}} = kippelr:get_clips().
{ok, {201, Clip}} = kippelr:create_clip([{<<"url">>, <<"https://github.com/s1n4/kippelr">>}]).
```
