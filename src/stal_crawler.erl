-module(stal_crawler).
-behaviour(gen_server).

-define(ACCEPT_HDR, {"Accept", "appliCation/vnd.github.preview"}).
-define(USER_AGENT, {"User-Agent", "stal/Erlang 0.01"}).
-define(GITHUB_REPO_SEARCH_URL, "https://api.github.com/search/repositories?q=language:erlang&sort=stars&order=desc&per_page=100&page=").
-define(HTTPOPTIONS, [{timeout, 5000}, {connect_timeout, 2000}]).
-define(OPTIONS, [{body_format, binary}]).

-include("stal.hrl").

-record(state, { page }).

-export([
    next/0
]).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% api
-spec next/0 :: () -> [#project{}].
next() ->
    gen_server:call(?MODULE, next).
 
%% gen_server callbacks
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init([]) ->
    {ok, #state{ page = 0 }}.
 
handle_call(next, _From, State = #state{ page = P }) ->
    Results = github_repo_search( P + 1 ), 
    {reply, Results, State#state{ page = P + 1 }};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.
 
handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions
github_repo_search(Page) when is_integer(Page) ->
    github_repo_search(integer_to_list(Page));

github_repo_search(Page) when is_list(Page) ->
    Url = ?GITHUB_REPO_SEARCH_URL ++ Page,
    error_logger:info_msg("Fetching page ~s", [Page]),

    {ok, {{_Ver, Status, _Msg}, Headers, Body}} = httpc:request(
        get, 
        {Url, [?ACCEPT_HDR, ?USER_AGENT]},
        ?HTTPOPTIONS,
        ?OPTIONS
    ),

    case Status of
        200 ->
            build_records(Body);
        _ ->
            error_logger:error_msg("Response> (~p) ~p~n~p~n", [Status, Headers, Body])
    end.

build_records(Body) ->
    Json = jsx:decode(Body),

    {_, Items} = lists:keyfind(<<"items">>, 1, Json),
    lists:map(fun filter_json/1, Items).

filter_json(Json) ->
    Fields = [<<"id">>, <<"name">>, <<"full_name">>, <<"description">>, <<"fork">>, 
                    <<"clone_url">>, <<"ssh_url">>,
                    <<"contents_url">>, <<"homepage">>
             ],
    DateFields = [<<"created_at">>, <<"pushed_at">>], 

    V0 = lists:map(fun(F) -> get_field(F, Json) end, Fields),
    V1 = lists:map(fun(F) -> get_date_field(F, Json) end, DateFields),
    V2 = calculate_popularity(get_field(<<"stargazers_count">>, Json), get_field(<<"forks">>, Json)),

    %% Terrible way to build a record, but y'know it's ok for now.
    list_to_tuple(lists:flatten([project, V0, V1, V2])).

get_field(Field, Json) ->
    case lists:keyfind(Field, 1, Json) of
        false ->
            undefined;
        {Field, Value} ->
            Value
    end.

get_date_field(Field, Json) ->
    case get_field(Field, Json) of
        undefined ->
            undefined;
        Value ->
            iso8601:parse(Value)
    end.

calculate_popularity(undefined, undefined) -> 0;
calculate_popularity(Stars, undefined) -> Stars;
calculate_popularity(undefined, Forks) -> Forks;
calculate_popularity(Stars, Forks) -> Stars + Forks.

