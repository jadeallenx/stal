-module(stal).
-behaviour(application).
-export([start/0, start/2, stop/1]).

% curl -i -H'accept:application/vnd.github.preview' \
% https://api.github.com/search/repositories?q=language:erlang\&sort=stars\&order=desc

start() ->
    application:load(stal),
    start_app_deps(stal),
    application:start(stal).

start(_StartType, _StartArgs) ->
    {ok, _Pid} = stal_sup:start_link(),
    ok.

stop(_Arg) ->
    ok.

start_app_deps(App) ->
    {ok, DepApps} = application:get_key(App, applications),
    [ensure_started(A) || A <- DepApps],
    ok.
    
ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
