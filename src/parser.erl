-module(parser).
-compile(export_all).

parse(Body, getUpdates) ->
  [{<<"ok">>,true}, {<<"result">>, Updates}] = jsx:decode(Body),
  parseUpdates(Updates).

parseUpdates([]) -> empty;
parseUpdates([Head|Tail]) ->
  {_, Uid} = findRec(Head, <<"update_id">>),
  Chat = findRec(Head, <<"chat">>),
  {_, Cid} = findRec(Chat, <<"id">>),
  {_, Text} = findRec(Head, <<"text">>),
  {{offset, Uid+1}, {Cid, Text}}.


findRec([], _) -> notfound;
findRec({_, Value}, TargetKey) when is_list(Value) -> findRec(Value, TargetKey);
findRec([{Key, Value}|_], TargetKey) when TargetKey =:= Key -> 
  {Key, Value};
findRec([{_, Value}|Tail], TargetKey) ->
  case is_list(Value) of
    true -> 
      Rec = findRec(Value, TargetKey);
    false ->
      Rec = notfound
  end,
  case Rec == notfound of
    true ->
      findRec(Tail, TargetKey);
    false ->
      Rec
  end.

  %[{<<"update_id">>, 121244},{<<"message">>, [{<<"message_id">>,22},{<<"from">>,[{<<"id">>,530167187},{<<"is_bot">>,false},{<<"first_name">>,<<"Yaroslav">>},{<<"last_name">>,<<"Drozdov">>},{<<"language_code">>,<<"ru">>}]},{<<"chat">>, [{<<"id">>, 121452},{<<"first_name">>,<<"Yaroslav">>},{<<"last_name">>,<<"Drozdov">>},{<<"type">>,<<"private">>}]},{<<"date">>,1575245772},{<<"text">>, <<"Text">>}]}].

