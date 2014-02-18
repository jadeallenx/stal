-module(stal_app).
-behaviour(application).
-export([start_app_deps/1, start/2, stop/1]).

start(_StartType, _StartArgs) ->
    start_app_deps(stal),
    stal_sup:start_link().
    
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
