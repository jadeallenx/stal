-module(stal).

-export([
    start/0,
    github_repo_search/0
]).

-include("stal.hrl").

start() ->
    application:load(stal),
    stal_app:start_app_deps(stal),
    application:start(stal).

github_repo_search() ->
    lists:foreach(fun() -> stal_crawler end, lists:seq(1,10)).


