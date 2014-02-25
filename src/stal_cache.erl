-module(stal_cache).
-behaviour(gen_server).

-include("stal.hrl").

%% API
-export([
    start_link/0,
    add/1,
    lookup/1
]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
 
-define(TABLE_NAME, github_projects).

-record(state, {tid}).

%% api implementation
-spec add/1 :: (
    P :: #project{} ) -> ok|error.

add(P = #project{}) ->
    case gen_server:call(?MODULE, {add, P}) of
        true -> ok;
        _ -> error
    end.

-spec lookup/1 :: (
    Id :: binary() | integer() ) -> [#project{}].
lookup(Id) when is_integer(Id) ->
    ets:lookup(?TABLE_NAME, Id);
lookup(Id) when is_binary(Id) ->
    ok.

%% gen_server callbacks
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init([]) ->
    Tid = ets:new(?TABLE_NAME, [named_table, {keypos, 2}]),
    {ok, #state{ tid = Tid }}.
 
handle_call({add, P}, _From, State) ->
    R = ets:insert(?TABLE_NAME, P),
    {reply, R, State};

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
 
%% Internal functions

