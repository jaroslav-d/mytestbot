-module(calculator).
-compile(export_all).

calc(Bin) -> calc(lazy_calc(Bin), []).
calc(empty, _) -> empty;
calc(stop, Acc) -> Acc;
calc(Fun, Acc) ->
  [Bin|NewFun] = Fun(),
  calc(NewFun, Acc ++ [Bin]).

lazy_calc(Bin) ->
  List = parser:parse_exp(Bin),
  lazy_calc(List, continue).
lazy_calc(empty, continue) ->
  fun() -> [empty|stop] end;
lazy_calc(List, continue) ->
  fun() ->
    {NewList, Condition} = purgatory(List),
    NewBin = parser:my_list_to_binary(NewList),
    [NewBin|lazy_calc(NewList, Condition)]
  end;
lazy_calc(_, stop) ->
  stop.

purgatory(List) ->
  {_, IndexMax, _, Operator} = lists:max(List),
  {LeftList, [_|RightList]} = lists:split(IndexMax, List),
  F = fun({P, Idx, Token, Key}, {HList, TList, Acc, Way}) ->
    case {Key, Acc, Way} of
      {bra, number, left} ->
        {HList, TList ++ [{P, Idx, Token, Key}], stop, left};
      {bra, minus, left} ->
        {HList, TList ++ [{P, Idx, Token, Key}], stop, left};
      {number, number, left} ->
        {[Token|HList], TList ++ [{P, Idx, Token, Key}], number, left};
      {numberlist, number, left} ->
        {Token ++ HList, TList ++ [{P, Idx, Token, Key}], number, left};
      {minus, number, left} ->
        {[Token|HList], TList ++ [{P, Idx, Token, Key}], minus, left};
      {number, minus, left} ->
        {HList -- "-", TList -- [lists:last(TList)], stop, left};
      {ket, number, right} ->
        {HList, TList ++ [{P, Idx, Token, Key}], stop, right};
      {number, number, right} ->
        {HList ++ [Token], TList ++ [{P, Idx, Token, Key}], number, right};
      {numberlist, number, right} ->
        {HList ++ Token, TList ++ [{P, Idx, Token, Key}], number, right};
      _Else ->
        {HList, TList, stop, Way}
    end
  end,
  LeftFold = lists:foldr(F, {[], [], number, left}, LeftList),
  RightFold = lists:foldl(F, {[], [], number, right}, RightList),
  {StrLeft, LTrash, _, _} = LeftFold,
  {StrRight, RTrash, _, _} = RightFold,
  {LastLTrash, LastRTrash} = try {lists:last(LTrash),lists:last(RTrash)} 
  catch error:Reason -> {{null,null,null,null}, {null,null,null,null}}
  end,
  {{_,_,_,IsBra},{_,_,_,IsKet}} = {LastLTrash, LastRTrash},
  {NewLTrash, NewRTrash} = case {IsBra, IsKet} of
    {bra, ket} -> {LTrash, RTrash};
    {bra, _} -> {LTrash -- [LastLTrash], RTrash};
    {_, ket} -> {LTrash, RTrash -- [LastRTrash]};
    {_, _} -> {LTrash, RTrash}
  end,
  NewLeftList = LeftList -- NewLTrash,
  NewRightList = RightList -- NewRTrash,
  NumLeft = parser:list_to_num(StrLeft),
  NumRight = parser:list_to_num(StrRight),
  Result = case Operator of
    plus ->
      NumLeft + NumRight;
    minus ->
      NumLeft - NumRight;
    mult ->
      NumLeft * NumRight;
    division ->
      NumLeft / NumRight;
    _ ->
      error
  end,
  ResultElem = [{-1, 0, parser:num_to_list(Result), numberlist}],
  case {NewLeftList, NewRightList} of
    {[],[]} ->
      {ResultElem, stop};
    {_, _} ->
      Revision = fun({P, _, T, K}, Acc) -> {{P, Acc, T, K}, Acc + 1} end,
      {NewList, _} = lists:mapfoldl(Revision, 0, 
        NewLeftList ++ ResultElem ++ NewRightList), 
      {NewList, continue}
  end.
