-module(parser).
-compile(export_all).

parse([], getUpdates) -> empty;
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

parse_exp(Bin) -> parse_exp(Bin, {0, 0}, []).
parse_exp(<<>>, _, []) -> empty;
parse_exp(<<>>, _, List) -> List;
parse_exp(<<Hbin/utf8, Tbin/binary>>, {Priority, Idx}, List) ->
  NewList = 
  case Hbin of
    43 -> %plus
      NewPriority = Priority,
      List ++ [{NewPriority + 1, Idx, Hbin, plus}];
    45 -> %minus
      NewPriority = Priority,
      List ++ [{NewPriority + 1, Idx, Hbin, minus}];
    42 -> %multiplicate
      NewPriority = Priority,
      List ++ [{NewPriority + 2, Idx, Hbin, mult}];
    47 -> %division
      NewPriority = Priority,
      List ++ [{NewPriority + 2, Idx, Hbin, division}];
    40 -> %bracket (
      NewPriority = Priority + 3,
      List ++ [{NewPriority, Idx, Hbin, bra}];
    41 -> %bracket )
      NewPriority = Priority - 3,
      List ++ [{NewPriority, Idx, Hbin, ket}];
    _Else ->
      NewPriority = Priority,
      List ++ [{NewPriority, Idx, Hbin, number}]
  end,
  parse_exp(Tbin, {NewPriority, Idx + 1}, NewList).

parse_my_list(List) ->
  F2 = fun({_, _, Token, _}, Acc) -> 
    if is_list(Token) -> 
      Acc ++ Token; 
    true -> 
      Acc ++ [Token] 
    end 
  end,
  list_to_binary(lists:foldl(F2, [], List)).

list_to_num([]) -> 0;
list_to_num(Num) ->
  try list_to_integer(Num)
  catch
    error:Reason -> 
      try list_to_float(Num) 
      catch 
        error:Reason -> [{'EXIT', 'no_number'}]; 
        Okk -> Okk
      end;
    Ok -> Ok
  end.

my_num_to_list(Num) ->
  if 
    Num < 0 -> "(" ++ num_to_list(Num) ++ ")"; 
    true -> num_to_list(Num) 
  end.
num_to_list(Num) when is_float(Num) ->
  float_to_list(Num, [{decimals, 4}, compact]);
num_to_list(Num) when is_integer(Num) ->
  integer_to_list(Num);
num_to_list(_) ->
  [{'EXIT', 'no_number'}].
