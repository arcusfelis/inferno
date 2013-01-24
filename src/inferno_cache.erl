-module(inferno_cache).
-export([start_link/1,
         get/2,
         put/4,
         purge/2]).

-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-record(cache_state, {table}).
-record(cache_entry, {key, seckey, value, date}).

%% ------------------------------------------------------------------
%% Client's API
%% ------------------------------------------------------------------

start_link(FileName) ->
    gen_server:start_link(FileName).

get(Server, Key) ->
    gen_server:call(Server, {get, Key}).

put(Server, Key, SecKey, Value) ->
    gen_server:cast(Server, {put, Key, SecKey, Value}).

purge(Server, SecKey) ->
    gen_server:cast(Server, {purge, SecKey}).

%% ------------------------------------------------------------------
%% Handlers
%% ------------------------------------------------------------------

init([FileName]) ->
    {ok, Table} = dets:open_file(FileName, [{keypos, #cache_entry.key}]),
    {ok, #cache_state{table=Table}}.

handle_call({get, Key}, _From, State=#cache_state{table=Table}) ->
    {reply, maybe_lookup_value(Table, Key), State}.

handle_cast({put, Key, SecKey, Value}, State=#cache_state{table=Table}) ->
    E = #cache_entry{key=Key, seckey=SecKey, 
                     value=Value, date=erlang:localtime()},
    dets:insert(Table, E),
    {noreply, State};
handle_cast({purge, SecKey}, State=#cache_state{table=Table}) ->
    dets:match_delete(Table, #cache_entry{_='_', seckey=SecKey}),
    {noreply, State}.

handle_info(_Mess, State) ->
    {noreply, State}.

terminate(_Reason, #cache_state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

maybe_lookup_value(Table, Key) ->
    case dets:lookup(Table, Key) of
        [#cache_entry{value=Val}] -> Val;
        [] -> undefined
    end.
