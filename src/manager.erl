-module(manager).
-compile(export_all).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-include("db_schema.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

call() ->
  gen_server:call(?MODULE, ok, infinity).

handle_call(_Request, _From, State) ->
  case sendMessage(State) of
    {ok, {_, _, Body}} ->
      io:format("all good ~n");
    _Else ->
      Body = [],
      io:format("all bad :'( ~n")
  end,
  case parser:parse(Body, getUpdates) of
    empty ->
      NewState = {offset, 0},
      io:format("all bad :'( ~n");
    {offset, Uid} ->
      NewState = {offset, Uid},
      io:format("not text");
    {NewState, Msg} ->
      io:format("all good ~p ~n", [Msg]),
      sendWorker(Msg)
  end,
  {reply, ok, NewState}.

handle_cast(_Request, _State) ->
  ok.

init(_Args) ->
  ets:new(workers, [set, public, named_table]),
  spawn_link(fun F() -> call(), F() end),
  {ok, {offset, 0}}.

terminate(_Reason, _State) ->
  ok.

sendMessage(Params) ->
  Request = builder:build_request("getUpdates", [Params]),
  io:format("~p~n", [Request]),
  httpc:request(
    post,
    Request, 
    [],
    [{body_format, binary}]
  ).

sendWorker({Cid, Text}) ->
  case ets:member(workers, Cid) of
    true ->
      [{_, Pid}] = ets:lookup(workers, Cid),
      io:format("Old ~p~n", [{Cid, Pid}]),
      gen_server:cast(Pid, {Cid, Text});
    false ->
      {ok, Pid} = worker_sup:start_child(Cid),
      ets:insert(workers, {Cid, Pid}),
      io:format("New ~p~n", [{Cid, Pid}]),
      gen_server:cast(Pid, {Cid, Text})
  end.
