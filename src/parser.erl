-module(parser).
-compile(export_all).

parse([], getUpdates) -> empty;
parse(Body, getUpdates) ->
  [{<<"ok">>,true}, {<<"result">>, Updates}] = jsx:decode(Body),
  try parseUpdateText(Updates)
  catch error:Reason -> {parseUpdateDif(Updates), no_text}
  end;
parse(Body, sendMessage) ->
  DBody = jsx:decode(Body),
  findRec(DBody, <<"message_id">>).

parseUpdateDif([]) -> empty;
parseUpdateDif([Head|_Tail]) ->
  {_, Uid} = findRec(Head, <<"update_id">>),
  {offset,Uid+1}.

parseUpdateText([]) -> empty;
parseUpdateText([Head|_Tail]) ->
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
parse_exp(<<"">>, _, []) -> empty;
parse_exp(<<>>, _, List) -> List;
parse_exp(<<Hbin/utf8, Tbin/binary>>, {Priority, Idx}, List) ->
  
  {NewPriority, NewIdx, NewList} = case Hbin of
    43 -> %plus 
      {Priority, 
        Idx + 1, 
        List ++ [{Priority + 1, Idx, Hbin, plus}] };
    45 -> %minus
      {Priority,
        Idx + 1,
        List ++ [{Priority + 1, Idx, Hbin, minus}] };
    42 -> %multiplicate
      {Priority,
        Idx + 1,
        List ++ [{Priority + 2, Idx, Hbin, mult}] };
    47 -> %division
      {Priority,
        Idx + 1,
        List ++ [{Priority + 2, Idx, Hbin, division}] };
    40 -> %bracket (
      {Priority + 3,
        Idx + 1,
        List ++ [{Priority, Idx, Hbin, bra}] };
    41 -> %bracket )
      {Priority - 3,
        Idx + 1, 
        List ++ [{Priority, Idx, Hbin, ket}] };
    32 -> %space " "
      {Priority, Idx, List};
    _Else ->
      {Priority,
        Idx + 1, 
        List ++ [{Priority, Idx, Hbin, number}] }
  end,
  parse_exp(Tbin, {NewPriority, NewIdx}, NewList).

my_list_to_binary(List) ->
  F2 = fun({_, _, Token, _}, Acc) -> 
    if is_list(Token) -> 
      Acc ++ Token;
    true -> 
      Acc ++ [Token] 
    end 
    end,
  try list_to_binary(lists:foldl(F2, [], List)) catch error:Reason -> no_numbers end.

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
