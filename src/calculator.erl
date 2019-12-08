-module(calculator).
-compile(export_all).


lazy_calc(Bin) ->
  List = parser:parse_exp(Bin),
  Calc = lazy(List),
  Calc().
lazy(List) ->
  fun() ->
    NewList = purgatory(List),
    NewBin = parser:parse_my_list(NewList),
    [NewBin|lazy(NewList)]
  end.

purgatory(List) ->
  {_, IndexMax, _, Operator} = lists:max(List),
  {LeftList, [_|RightList]} = lists:split(IndexMax, List),
%  F = fun({P, Idx, Token, Key}, {HList, TList, Acc, Way, Delta}) ->
%    case {Key, Acc, Way} of
%      {number, number, left} ->
%        {[Token|HList], TList, number, left, Idx - 1};
%      {numberlist, number, left} ->
%        {Token ++ HList, TList, number, left, Idx - 1};
%      {minus, number, left} ->
%        {[Token|HList], TList, number, left, Idx - 1};
%      {number, number, right} ->
%        {HList ++ [Token], TList, number, right, Idx + 1};
%      {numberlist, number, right} ->
%        {HList ++ Token, TList, number, right, Idx + 1};
%      _Else ->
%        {HList, TList ++ [{P, Idx, Token, Key}], stop, Way, Delta}
%    end
%  end,
  F = fun({P, Idx, Token, Key}, {HList, TList, Acc, Way}) ->
    case {Key, Acc, Way} of
      {number, number, left} ->
        {[Token|HList], TList ++ [{P, Idx, Token, Key}], number, left};
      {numberlist, number, left} ->
        {Token ++ HList, TList ++ [{P, Idx, Token, Key}], number, left};
      {minus, number, left} ->
        {[Token|HList], TList ++ [{P, Idx, Token, Key}], number, left};
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
  %% let there be such an expression
  %% "(5+27*(22+36*(15+13)))/(21+0.2)"
  %% then
%  LeftFold = lists:foldr(F, {[], [], number, left, 0}, LeftList),
  %% {"15", "(*63+22(*72+5(", _, _, 14}
%  RightFold = lists:foldl(F, {[], [], number, right, 0}, RightList),
  %% {"13", ")))/(21+0.2)", _, _, 18}
  %% and you can see that LList is reverse
  %% than you must to know this fact
  {StrLeft, LList, CheckL, _, IdxL} = LeftFold,
  {StrRight, RList, CheckR, _, IdxR} = RightFold,
  case {CheckL, CheckR} of 
    {number, number} -> 
      HLListToken = null,
      HRListToken = null;
    _ ->
      [{_,_,_,HLListToken}|_] = LList,
      [{_,_,_,HRListToken}|_] = RList
  end,
  case {HLListToken, HRListToken} of
    {bra, ket} ->
      [_|NewLeftList] = LList,
      [_|NewRightList] = RList,
      IdxLeft = IdxL - 1,
      IdxRight = IdxR + 1;
    _ ->
      NewLeftList = LList,
      NewRightList = RList,
      IdxLeft = IdxL,
      IdxRight = IdxR
  end,
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
  Repeat = case {NumLeft, Operator} of 
    {0, minus} -> true; 
    {0.0, minus} -> true; 
    _ -> false 
  end,
  ResultElem = [{0, IdxLeft + 1, parser:num_to_list(Result), numberlist}],
  lists:reverse(NewLeftList) ++ ResultElem ++ NewRightList.
