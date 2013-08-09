-module(kippelr).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([start/0]).
-export([start_link/0]).
-export([stop/0]).
-export([auth/1]).
-export([is_authenticated/0]).

-include("kippelr.hrl").


start() ->
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(inets),
    ok = application:start(kippelr).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, terminate).

auth(Options) ->
    gen_server:cast(?MODULE, Options).

is_authenticated() ->
    gen_server:call(?MODULE, is_authenticated, ?TIMEOUT).


%% gen_server
init([]) ->
    State = #state{url=?KIPPT, headers=[]},
    {ok, State, ?TIMEOUT}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call(is_authenticated, _From, State) ->
    {ok, Resp} = httpc:request(get, {State#state.url ++ "account/", State#state.headers}, [], []),
    {Status, _, _} = parse_resp(Resp),
    Res = if Status == 401 -> false;
             true -> true
          end,
    {reply, Res, State}.

handle_cast({basic_auth, {Username, Password}}, State) ->
    B64d = base64:encode_to_string(Username ++ ":" ++ Password),
    NewState = State#state{headers=[{"Authorization", "Basic " ++ B64d}]},
    {noreply, NewState};

handle_cast({token_auth, {Username, Token}}, State) ->
    NewState = State#state{headers=[{"X-Kippt-Username", Username}, {"X-Kippt-API-Token", Token}]},
    {noreply, NewState}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% private
parse_resp(Resp) ->
    {{_, Status, Msg}, _, Body} = Resp,
    {Status, Msg, Body}.
