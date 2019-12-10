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
        {HList, [{P, Idx, Token, Key}|TList], stop, left};
      {bra, minus, left} ->
        {HList, [{P, Idx, Token, Key}|TList], stop, left};
      {number, number, left} ->
        {[Token|HList], [{P, Idx, Token, Key}|TList], number, left};
      {numberlist, number, left} ->
        {Token ++ HList, [{P, Idx, Token, Key}|TList], number, left};
      {minus, number, left} ->
        {[Token|HList], [{P, Idx, Token, Key}|TList], minus, left};
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
  if LTrash /= [] -> [HeadLTrash|_] = LTrash; true -> HeadLTrash = {null,null,null,null} end,
  {LastLTrash, LastRTrash} = try {HeadLTrash, lists:last(RTrash)} 
  catch error:Reason -> {{null,null,null,null}, {null,null,null,null}}
  end,
  {{_,_,_,IsBra},{_,_,_,IsKet}} = {LastLTrash, LastRTrash},
  {NewLTrash, NewRTrash} = case {IsBra, IsKet} of
    {bra, ket} -> {LTrash, RTrash};
    {bra, _} -> {LTrash -- [LastLTrash], RTrash};
    {_, ket} -> {LTrash, RTrash -- [LastRTrash]};
    {_, _} -> {LTrash, RTrash}
  end,
  BLeftList = LeftList -- NewLTrash,
  NewLeftList = case NewLTrash of
    [{Pr,In,Tok,minus}|_] -> BLeftList ++ [{Pr,In,43,plus}];
    _Else -> BLeftList
  end,
  NewRightList = RightList -- NewRTrash,
  NumLeft = parser:list_to_num(StrLeft),
  NumRight = parser:list_to_num(StrRight),
  Result = case Operator of
    plus -> NumLeft + NumRight;
    minus -> NumLeft - NumRight;
    mult -> NumLeft * NumRight;
    division -> NumLeft / NumRight;
    _ -> error
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


%(1+2*(2+3*(1+1)))/(1+0.2)
%(45+47)*5+6-5*(2+3)
%92*5+6-5*5
%(-10+2)*5-3*10*(1*(-1+2))
%10 + 2 - 10*(-3) + 2*5 + 10
%-2-2-2
