%%%-------------------------------------------------------------------
%%% @author jaroslav
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jun 2020 23:13
%%%-------------------------------------------------------------------
-module(base_manager).
-author("jaroslav").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-include("db_schema.hrl").

-define(SERVER, ?MODULE).
-define(MAX_CLIENTS, 5).

-record(base_manager_state, {state=state}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #base_manager_state{}} | {ok, State :: #base_manager_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #base_manager_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #base_manager_state{}) ->
  {reply, Reply :: term(), NewState :: #base_manager_state{}} |
  {reply, Reply :: term(), NewState :: #base_manager_state{}, timeout() | hibernate} |
  {noreply, NewState :: #base_manager_state{}} |
  {noreply, NewState :: #base_manager_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #base_manager_state{}} |
  {stop, Reason :: term(), NewState :: #base_manager_state{}}).
handle_call( {is_registered, Cid}, _From, State = #base_manager_state{state = state}) ->
  {reply, is_registered(Cid), State};
handle_call( {registration, Cid, Message}, _From, State = #base_manager_state{state = state}) ->
  {reply, registration(Cid, Message), State};
handle_call( {create_record, Cid, {MarkDate, MarkTime}}, _From, State = #base_manager_state{state = state}) ->
  case {is_registered(Cid), is_marked(Cid)} of
    {true, true} -> {reply, hes_marked, State};
    {true, false} -> {reply, create_record(Cid, MarkDate, MarkTime), State};
    {false, true} -> {reply, hes_cheater, State};
    {false, false} -> {reply, not_registered, State}
  end;
handle_call(_Request, _From, State = #base_manager_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #base_manager_state{}) ->
  {noreply, NewState :: #base_manager_state{}} |
  {noreply, NewState :: #base_manager_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #base_manager_state{}}).
handle_cast(_Request, State = #base_manager_state{}) ->
  {noreply, State}.

%% @private
  %% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #base_manager_state{}) ->
  {noreply, NewState :: #base_manager_state{}} |
  {noreply, NewState :: #base_manager_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #base_manager_state{}}).
handle_info(_Info, State = #base_manager_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #base_manager_state{}) -> term()).
terminate(_Reason, _State = #base_manager_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #base_manager_state{},
    Extra :: term()) ->
  {ok, NewState :: #base_manager_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #base_manager_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_registered(Cid) ->
  F = fun() -> builder:build_select_db_client('$1', '$2', '$3', '$4', Cid) end,
  case mnesia:transaction(F) of
    {atomic, []} -> false;
    {atomic, _Record} -> true
  end.

is_marked(Cid) ->
  F = fun() -> builder:build_select_db_mark(Cid, '$1', '$2') end,
  case mnesia:transaction(F) of
    {atomic, []} -> false;
    {atomic, _Record} -> true
  end.

%% registration(123456, <<"Пролетаров Владилен Карлович">>)
registration(Cid, Message) ->
  F = try binary:split(Message, [<<" "/utf8>>], [global]) of
        [LastName, FirstName, MiddleName] ->
          fun() ->
            case builder:build_select_db_client('$1', LastName, FirstName, MiddleName, '$2') of
              [] -> no_record;
              [[Rid, undefined]] -> builder:build_update_db_client(Rid, LastName, FirstName, MiddleName, Cid);
              [[_Rid, DbCid]] -> DbCid == Cid
            end
          end
      catch error:Reason1 -> no_name
      end,
  try mnesia:transaction(F)
  catch error:Reason2 -> {atomic, no_name}
  end.

%% create_table(123456, [<<"пн">>, <<"вт">>,...], [<<"10">>, <<"12">>,...])
create_record(Cid, MarkDate, MarkTime) ->
  F = fun
        () ->
          DateTimes = [ {Date, Time} || Date <- MarkDate, Time <- MarkTime],
          Func = fun
                   Loop([]) -> all_busy;
                   Loop(DateTimes) ->
                     Nth = rand:uniform(length(DateTimes)),
                     {Date, Time} = lists:nth(Nth, DateTimes),
                     [[Id, Clients]] = builder:build_select_db_list('$1', Date, Time, '$2'),
                     if
                       length(Clients) < ?MAX_CLIENTS ->
                         NewClients = Clients ++ [Cid],
                         builder:build_update_db_list(Id, Date, Time, NewClients),
                         builder:build_update_db_mark(Cid, Date, Time),
                         {Date, Time};
                       true ->
                         NewDateTimes = lists:delete({Date, Time}, DateTimes),
                         Loop(NewDateTimes)
                     end
                 end,
          Func(DateTimes)
      end,
  try mnesia:transaction(F)
  catch error:Reason -> oshibochka
  end.