-module(worker).
-behaviour(gen_server).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").
-include("db_schema.hrl").

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, state}.

handle_call(_Request, _From, _State) ->
  ok.

handle_cast( {Cid, <<"/start">>}, _State) ->
  case is_registered(Cid) of
    true -> sendMessageClient(Cid, <<"Повторная регистрация не требуется"/utf8>>, state);
    false ->
      Text = <<"Привет, друг! Введи пожалуйста свои ФИО, чтобы мы дальше могли общаться без проблем"/utf8>>,
      sendMessageClient(Cid, Text, registration)
  end;
handle_cast( {Cid, <<"Ja">>}, registration) ->
  sendMessageClient(Cid, <<"Введите пожалуйста ФИО снова"/utf8>>, hide_keyboard, registration);
handle_cast( {Cid, <<"Nein">>}, registration) ->
  sendMessageClient(Cid, <<"При желании, зарегистрироваться в сервисе можете набрав команду /start"/utf8>>, hide_keyboard, state);
handle_cast( {Cid, Message}, registration) ->
  case registration(Cid, Message) of
    {Text, state} -> sendMessageClient(Cid, Text, state);
    {Text, Buttons, registration} -> sendMessageClient(Cid, Text, keyboard, Buttons, registration)
  end;

handle_cast( {Cid, <<"/rec">>}, _State) ->
  sendMessageClient(Cid, <<"На какое время хотите записаться?"/utf8>>, state);

handle_cast( {Cid, <<"Привет"/utf8>>}, _State) ->
  sendMessageClient(Cid, <<"Привет"/utf8>>, state);
handle_cast( {Cid, <<"Hello"/utf8>>}, _State) ->
  sendMessageClient(Cid, <<"I need your skills"/utf8>>, trigger);
handle_cast( {Cid, <<"You son over bitch. I'm in">>}, trigger) ->
  sendMessageClient(Cid, <<"You are the Rick's friend">>, state);

handle_cast( {Cid, <<"/calc"/utf8>>}, _State) ->
  sendMessageClient(Cid, <<"Please, enter your expression">>, calc);
%% for OTP 21 and higher
%handle_cast( {Cid, Expression}, calc) ->
%  case calc(Expression) of
%    {Text, state} -> sendMessageClient(Cid, Text, state);
%    {Results, continue} -> {noreply, calc, {continue, {Cid, Results}}}
%  end;
handle_cast( {Cid, Expression}, calc) ->
  case calc(Expression) of
    {Text, state} -> sendMessageClient(Cid, Text, state);
    {Results, continue} -> 
      [sendMessageClient(Cid, Res, state) || Res <- Results],
      {noreply, state, infinity}
  end;

handle_cast( {Cid, <<"/lazy_calc"/utf8>>}, _State) ->
  sendMessageClient(Cid, <<"Please, enter your expression">>, lazy_calc);
handle_cast( {Cid, Expression}, lazy_calc) ->
  case lazy_calc(Expression) of
    {Text, state} -> sendMessageClient(Cid, Text, state);
    {Text, Fun, lazy_calc} -> sendMessageClient(Cid, Text, {Fun, lazy_calc})
  end;
handle_cast( {Cid, <<"/lazy_calc_next"/utf8>>}, {Fun, lazy_calc}) ->
  case lazy_calc(Fun) of
    {Text, state} -> sendMessageClient(Cid, Text, state);
    {Text, NewFun, lazy_calc} -> sendMessageClient(Cid, Text, {NewFun, lazy_calc})
  end;
handle_cast( {Cid, _Else}, _State) ->
  sendMessageClient(Cid, <<"Incorrect command">>, state).

%% for OTP 21 and higher
%handle_continue({Cid, Results}, calc) ->
%  case Results of
%    [] -> {noreply, state, infinity};
%    [Result|OtherRes] -> 
%      sendMessageClient(Cid, Result, ok),
%      {noreply, calc, {continue, {Cid, OtherRes}}}
%  end.

handle_info(_Info, _State) ->
  ok.

is_registered(Cid) ->
  F = fun() -> builder:build_select_db_client('$1', '$2', '$3', '$4', Cid) end,
  case mnesia:transaction(F) of
    {atomic, []} ->
      false;
    {atomic, _Record} ->
      true
  end.

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
  try mnesia:transaction(F) of
    {atomic, no_record} -> {<<"Таких ФИО нет в базе, хотите попробовать еще раз?"/utf8>>, [<<"Ja"/utf8>>, <<"Nein"/utf8>>], registration};
    {atomic, ok} -> {<<"Вы успешно зарегистрированны в базе"/utf8>>, state};
    {atomic, true} -> {<<"Таки вы уже в списке"/utf8>>, state};
    {atomic, false} -> {<<"Такой человек уже зарегистрирован, хотите попробовать еще раз?"/utf8>>, [<<"Ja"/utf8>>, <<"Nein"/utf8>>], registration}
  catch error:Reason2 -> {<<"Неверно введены ФИО, попробовать еще раз?"/utf8>>, [<<"Ja"/utf8>>, <<"Nein"/utf8>>], registration}
  end.

calc(Expression) ->
  try calculator:calc(Expression) of
    empty -> {<<"Your expression is empty">>, state};
    no_numbers -> {<<"Incorrect expression">>, state};
    Results -> {Results, continue}
  catch
    error:Reason -> {<<"Error">>, state}
  end.

lazy_calc(Expression) when is_binary(Expression) ->
  lazy_calc(calculator:lazy_calc(Expression));
lazy_calc(Fun) ->
  try Fun() of
    [empty|stop] -> {<<"Void expression">>, state};
    [Text|stop] -> {Text, state};
    [Text|NewFun] -> {Text, NewFun, lazy_calc}
  catch
    error:Reason -> {<<"Incorrect expression">>, state}
  end.

sendMessageClient(Cid, Text, State) ->
  sendMessageClient(builder:build_Msg(Cid, Text), State).
sendMessageClient(Cid, Text, keyboard, Buttons, State) ->
  sendMessageClient(builder:build_keyboard(Cid, Text, Buttons), State).
sendMessageClient(Cid, Text, hide_keybroad, State) ->
  sendMessageClient(builder:build_hide_keyboard(Cid, Text), State).

sendMessageClient(Msg, State) ->
  {ok, Result} = httpc:request(
    post,
    builder:build_request("sendMessage", Msg), 
    [],
    [{body_format, binary}]
  ),
  io:format("~p~n", [Result]),
  {noreply, State, infinity}.
