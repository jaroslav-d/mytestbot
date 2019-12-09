-module(manager).
-compile(export_all).

start() ->
  ets:new(workers, [set, public, named_table]),
  spawn(manager, looper, []).

looper() -> looper({offset, 0}).
looper(Params) ->
  case sendMessage(Params) of
    {ok, {_, _, Body}} ->
      io:format("all good ~n"),
      ok;
    _Else ->
      Body = [],
      io:format("all bad :'( ~n"),
      ok
  end,
  case parser:parse(Body, getUpdates) of
    {NewParams, Msg} ->
      io:format("all good ~n"),
      sendWorker(Msg),
      looper(NewParams);
    empty ->
      io:format("all bad :'( ~n"),
      looper({offset, 0})
  end.

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
      io:format("~p~n", [{Cid, Pid}]),
      Pid ! {Cid, Text};
    false ->
      Pid = worker:start(),
      ets:insert(workers, {Cid, Pid}),
      io:format("~p~n", [{Cid, Pid}]),
      Pid ! {Cid, Text}
  end.

