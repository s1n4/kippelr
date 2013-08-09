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
-export([account/0]).
-export([clips/0]).
-export([get_clip/1]).
-export([delete_clip/1]).

-export([upgrade/0]).

-define(KIPPT, "https://kippt.com/api/").
-define(TIMEOUT, 20000).

-record(state, {headers}).


start() ->
    ok = ensure_started(crypto),
    ok = ensure_started(public_key),
    ok = ensure_started(ssl),
    ok = ensure_started(inets),
    ok = ensure_started(kippelr).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, terminate).

%% @doc authentication
%% @spec auth(Options :: authentication()) -> ok
%% authentication() = {basic_auth, {username(), password()}} | {token_auth, {username(), api_token()}}
%% username() = string()
%% password() = string()
%% api_token() = string()
auth(Options) ->
    gen_server:cast(?MODULE, Options).

%% @doc check if authorization succeeds
is_authenticated() ->
    gen_server:call(?MODULE, is_authenticated, ?TIMEOUT).

%% @doc get user's profile
account() ->
    gen_server:call(?MODULE, account, ?TIMEOUT).

%% @doc get user's clips
clips() ->
    gen_server:call(?MODULE, clips, ?TIMEOUT).

%% @doc get a clip
get_clip(Id) ->
    gen_server:call(?MODULE, {clip, Id}, ?TIMEOUT).

delete_clip(Id) ->
    gen_server:call(?MODULE, {delete, clips, Id}).


%% gen_server
init([]) ->
    State = #state{headers=[]},
    {ok, State, ?TIMEOUT}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call(is_authenticated, _From, State) ->
    Result = request(get, {url("account"), headers(State)}),
    {Status, _, _} = parse_resp(Result),
    Resp = if Status == 401 -> false;
             true -> true
          end,
    {reply, Resp, State};

handle_call(account, _From, State) ->
    {_, _, Body} = parse_resp(request(get, {url("account"), headers(State)})),
    {reply, Body, State};

handle_call(clips, _From, State) ->
    {_, _, Body} = parse_resp(request(get, {url("clips"), headers(State)})),
    {reply, Body, State};

handle_call({clip, Id}, _From, State) ->
    {_, _, Body} = parse_resp(request(get, {url("clips", Id), headers(State)})),
    {reply, Body, State};

handle_call({Method, Endpoint, Id}, _From, State) ->
    {_, Status, Body} = parse_resp(request(Method, {url(Endpoint, Id), headers(State)})),
    {reply, {Status, Body}, State}.

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


%% private functions
url(Endpoint) ->
    ?KIPPT ++ Endpoint ++ "/".

url(Endpoint, Id) ->
    Id1 = if is_integer(Id) -> integer_to_list(Id);
             true -> Id
          end,
    ?KIPPT ++ Endpoint ++ "/" ++ Id1.

headers(State) ->
    State#state.headers.

parse_resp(Resp) ->
    {{_, Status, Msg}, _, Body} = Resp,
    {Status, Msg, jsx:decode(list_to_binary(Body))}.

request(Method, Request) ->
    request(Method, Request, []).

request(Method, Request, HTTPOptions) ->
    request(Method, Request, HTTPOptions, []).

request(Method, Request, HTTPOptions, Options) ->
    {ok, Result} = httpc:request(Method, Request, HTTPOptions, Options),
    Result.

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        _ ->
            error
    end.


%% don't use it at home
upgrade() ->
    {ok, Vsn} = application:get_key(?MODULE, vsn),
    sys:suspend(?MODULE),
    code:purge(?MODULE),
    code:load_file(?MODULE),
    sys:change_code(?MODULE, ?MODULE, Vsn, []),
    sys:resume(?MODULE).
