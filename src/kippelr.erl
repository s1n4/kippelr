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
-export([get_clips/0]).
-export([get_clips_favorites/0]).
-export([get_clips_feed/0]).
-export([get_clip/1]).
-export([get_clip_comments/1]).
-export([get_clip_likes/1]).
-export([delete_clip/1]).
-export([delete_clip_comment/2]).
-export([favorite/1]).
-export([unfavorite/1]).
-export([like/1]).
-export([unlike/1]).
-export([create_clip/1]).
-export([create_comment/2]).
-export([modify_clip/2]).
-export([notifications/0]).
-export([mark_notifications_as_read/0]).
-export([get_lists/0]).
-export([get_list/1]).
-export([get_list_clips/1]).
-export([is_following_list/1]).
-export([create_list/1]).
-export([follow_list/1]).
-export([unfollow_list/1]).
-export([modify_list/2]).
-export([delete_list/1]).
-export([get_user/1]).

-export([upgrade/0]).

-define(KIPPT, "https://kippt.com/api/").
-define(TIMEOUT, 20000).

-record(state, {headers}).


%% @doc start kippelr
start() ->
    ok = ensure_started(crypto),
    ok = ensure_started(public_key),
    ok = ensure_started(ssl),
    ok = ensure_started(inets),
    ok = ensure_started(kippelr).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc stop kippelr
stop() ->
    gen_server:call(?MODULE, terminate).

%% @doc authentication
%% @spec auth(Options :: authentication()) -> ok
%% authentication() = {basic_auth, {username(), password()}} |
%%                    {token_auth, {username(), api_token()}}
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
    gen_server:call(?MODULE, {get, [account]}, ?TIMEOUT).

%% @doc get user's clips
get_clips() ->
    gen_server:call(?MODULE, {get, [clips]}, ?TIMEOUT).

%% @doc get user's favorites
get_clips_favorites() ->
    gen_server:call(?MODULE, {get, [clips, favorites]}, ?TIMEOUT).

%% @doc get user's feed
get_clips_feed() ->
    gen_server:call(?MODULE, {get, [clips, favorites]}, ?TIMEOUT).

%% @doc get a clip
get_clip(Id) ->
    gen_server:call(?MODULE, {get, [clips, Id]}, ?TIMEOUT).

%% @doc get a clip's comments
get_clip_comments(Id) ->
    gen_server:call(?MODULE, {get, [clips, Id, comments]}, ?TIMEOUT).

%% @doc get a clip's likes
get_clip_likes(Id) ->
    gen_server:call(?MODULE, {get, [clips, Id, likes]}, ?TIMEOUT).

%% @doc delete a clip
delete_clip(Id) ->
    gen_server:call(?MODULE, {delete, [clips, Id]}, ?TIMEOUT).

%% @doc delete a comment
delete_clip_comment(ClipId, CommentId) ->
    gen_server:call(?MODULE, {delete, [clips, ClipId, comments, CommentId]},
                    ?TIMEOUT).

%% @doc favorite a clip
favorite(Id) ->
    Data = [
            {endpoint, {clips, Id}},
            {collection, {favorite, ''}},
            {content, ""}
           ],
    gen_server:call(?MODULE, {post, Data}, ?TIMEOUT).

%% @doc unfavorite a clip
unfavorite(Id) ->
    gen_server:call(?MODULE, {delete, [clips, Id, favorite]}, ?TIMEOUT).

%% @doc like a clip
like(Id) ->
    Data = [
            {endpoint, {clips, Id}},
            {collection, {likes, ''}},
            {content, ""}
           ],
    gen_server:call(?MODULE, {post, Data}, ?TIMEOUT).

%% @doc unlike a clip
unlike(Id) ->
    gen_server:call(?MODULE, {delete, [clips, Id, likes]}, ?TIMEOUT).

%% @doc create a clip
create_clip(Clip) ->
    Data = [
            {endpoint, {clips, ''}},
            {collection, {'', ''}},
            {content, Clip}
           ],
    gen_server:call(?MODULE, {post, Data}, ?TIMEOUT).

%% @doc create a new comment for clip
create_comment(ClipId, Comment) ->
    Data = [
            {endpoint, {clips, ClipId}},
            {collection, {comments, ''}},
            {content, Comment}
           ],
    gen_server:call(?MODULE, {post, Data}, ?TIMEOUT).

%% @doc modify a clip
modify_clip(Id, Clip) ->
    Data = [
            {endpoint, {clips, Id}},
            {collection, {'', ''}},
            {content, Clip}
           ],
    gen_server:call(?MODULE, {put, Data}, ?TIMEOUT).

%% @doc get latest notifications
notifications() ->
    gen_server:call(?MODULE, {get, [notifications]}, ?TIMEOUT).

%% @doc mark notifications as read
mark_notifications_as_read() ->
    Data = [
            {endpoint, {notifications, ''}},
            {collection, {'', ''}},
            {content, "{\"action\":\"mark_seen\"}"}
           ],
    gen_server:call(?MODULE, {post, Data}, ?TIMEOUT).

%% @doc get user's lists
get_lists() ->
    gen_server:call(?MODULE, {get, [lists]}, ?TIMEOUT).

%% @doc get a list
get_list(Id) ->
    gen_server:call(?MODULE, {get, [lists, Id]}, ?TIMEOUT).

%% @doc get a list's clips
get_list_clips(Id) ->
    gen_server:call(?MODULE, {get, [lists, Id, clips]}, ?TIMEOUT).

%% @doc get information about a relationship to a list.
is_following_list(Id) ->
    gen_server:call(?MODULE, {get, [lists, Id, relationship]}, ?TIMEOUT).

%% @doc create a list
create_list(Object) ->
    Data = [
            {endpoint, {lists, ''}},
            {collection, {'', ''}},
            {content, Object}
           ],
    gen_server:call(?MODULE, {post, Data}, ?TIMEOUT).

%% @doc follow a list
follow_list(Id) ->
    Data = [
            {endpoint, {lists, Id}},
            {collection, {relationship, ''}},
            {content, "{\"action\":\"follow\"}"}
           ],
    gen_server:call(?MODULE, {post, Data}, ?TIMEOUT).

%% @doc unfollow a list
unfollow_list(Id) ->
    Data = [
            {endpoint, {lists, Id}},
            {collection, {relationship, ''}},
            {content, "{\"action\":\"unfollow\"}"}
           ],
    gen_server:call(?MODULE, {post, Data}, ?TIMEOUT).

%% @doc modify a list
modify_list(Id, Object) ->
    Data = [
            {endpoint, {lists, Id}},
            {collection, {'', ''}},
            {content, Object}
           ],
    gen_server:call(?MODULE, {put, Data}, ?TIMEOUT).

%% @doc delete a list
delete_list(Id) ->
    gen_server:call(?MODULE, {delete, [lists, Id]}, ?TIMEOUT).

%% @doc get public user data based on user's ID or username
get_user(Id) ->
    gen_server:call(?MODULE, {get, [users, Id]}, ?TIMEOUT).


%% gen_server
init([]) ->
    State = #state{headers=[]},
    {ok, State, ?TIMEOUT}.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};

handle_call(is_authenticated, _From, State) ->
    {Status, _, _} = request(get, {url([account]), headers(State)}),
    Resp = if Status == 401 -> false;
              true -> true
           end,
    {reply, Resp, State};

handle_call({Method, Segment}, _From, State) when Method =:= get;
                                                  Method =:= delete ->
    {Status, _, Body} = request(Method, {url(Segment), headers(State)}),
    {reply, {ok, {Status, Body}}, State};

handle_call({Method, Data}, _From, State) ->
    {Endpoint, Id} = proplists:get_value(endpoint, Data),
    {Collection, CollectionId} = proplists:get_value(collection, Data),
    Content = proplists:get_value(content, Data),
    {Status, _, Body} = request(Method, {url([Endpoint,
                                              Id, Collection, CollectionId]),
                                         headers(State), "application/json",
                                         Content}),
    {reply, {ok, {Status, Body}}, State}.

handle_cast({basic_auth, {Username, Password}}, State) ->
    B64d = base64:encode_to_string(Username ++ ":" ++ Password),
    NewState = State#state{headers=[{"Authorization", "Basic " ++ B64d}]},
    {noreply, NewState};

handle_cast({token_auth, {Username, Token}}, State) ->
    NewState = State#state{headers=[{"X-Kippt-Username", Username},
                                    {"X-Kippt-API-Token", Token}]},
    {noreply, NewState}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% private functions
url([]) ->
    [];
url(List) ->
    %% remove empty atoms
    List1 = lists:filter(fun(V) ->
                                 if V == '' -> false;
                                    true -> true
                                 end
                         end, List),
    url(List1, []).

url([], Acc) ->
    lists:concat([?KIPPT|Acc]);
url([H|T], Acc) ->
    url(T, Acc ++ [H] ++ ["/"]).

headers(State) ->
    State#state.headers.

parse_resp(Resp) ->
    {{_, Status, Msg}, _, Body} = Resp,
    Body1 = if Body =/= [] -> jsx:decode(list_to_binary(Body));
               true -> Body
            end,
    {Status, Msg, Body1}.

request(Method, Request) ->
    request(Method, Request, []).

request(Method, Request, HTTPOptions) ->
    request(Method, Request, HTTPOptions, []).

request(Method, Request, HTTPOptions, Options) ->
    {ok, Result} = httpc:request(Method, Request, HTTPOptions, Options),
    parse_resp(Result).

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
