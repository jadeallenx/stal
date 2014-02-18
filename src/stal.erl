-module(stal).

-export([
    start/0,
    github_repo_search/0,
    github_repo_search/1
]).

-include("stal.hrl").

start() ->
    application:load(stal),
    stal_app:start_app_deps(stal),
    application:start(stal).

start_ets() ->
    Tid = ets:new(?TABLE_NAME, [named_table, {keypos, 2}]),
    put(ets, Tid).

github_repo_search() ->
    case get(ets) of
        undefined ->
            start_ets();
        _Tid ->
            ok
    end,
    lists:foreach(fun(P) -> github_repo_search(P) end, lists:seq(1,10)).

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
            error_logger:info_msg("Response> (~p) ~p~n", [Status, Headers]),
            store_projects(Body);
        _ ->
            error_logger:info_msg("Response> (~p) ~p~n~p~n", [Status, Headers, Body])
    end.

store_projects(Body) ->
    Json = jsx:decode(Body),
    {_, Count} = lists:keyfind(<<"total_count">>, 1, Json),

    {_, Items} = lists:keyfind(<<"items">>, 1, Json),
    error_logger:info_msg("Items in this batch> ~p Count> ~p", [length(Items), Count]),

    lists:foreach(fun(R) -> true = ets:insert(?TABLE_NAME, R) end, lists:map(fun filter_json/1, Items)),
    ok.

filter_json(Json) ->
    Fields = [<<"id">>, <<"full_name">>, <<"description">>, <<"fork">>, 
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
