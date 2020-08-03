-module(worker).
-behaviour(gen_server).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").
-include("db_schema.hrl").

-define(BM, base_manager).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, state}.

handle_call(_Request, _From, _State) ->
  ok.

handle_cast( {Cid, <<"/start">>}, _State) ->
  case gen_server:call(?BM, {is_registered, Cid}) of
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
  Result = case gen_server:call(?BM, {registration, Cid, Message}) of
             {atomic, no_record} -> {<<"Таких ФИО нет в базе, хотите попробовать еще раз?"/utf8>>, [<<"Ja"/utf8>>, <<"Nein"/utf8>>], registration};
             {atomic, ok} -> {<<"Вы успешно зарегистрированны в базе"/utf8>>, state};
             {atomic, true} -> {<<"Таки вы уже в списке"/utf8>>, state};
             {atomic, false} -> {<<"Такой человек уже зарегистрирован, хотите попробовать еще раз?"/utf8>>, [<<"Ja"/utf8>>, <<"Nein"/utf8>>], registration};
             {atomic, no_name} -> {<<"Неверно введены ФИО, попробовать еще раз?"/utf8>>, [<<"Ja"/utf8>>, <<"Nein"/utf8>>], registration}
           end,
  case Result of
    {Text, state} -> sendMessageClient(Cid, Text, state);
    {Text, Buttons, registration} -> sendMessageClient(Cid, Text, keyboard, Buttons, registration)
  end;

handle_cast( {Cid, <<"/rec">>}, _State) ->
  Text = <<"На какое время хотите записаться? Можно отметить несколько промежутков времени"/utf8>>,
  Buttons = builder:build_buttons_inline_time(),
  sendMessageClient(Cid, Text, inline_keyboard, Buttons, recording_on_time);
handle_cast( {Cid, {ThisMsgID, Buttons, Button}}, {State, MsgID}) when ThisMsgID =:= MsgID ->
  handle_cast( {Cid, {Buttons, Button}}, {State, MsgID});
handle_cast( {Cid, {TimeButtons, <<"Enter">>}}, {recording_on_time, MsgID}) ->
  MarkTime = parser:get_marks(TimeButtons),
  DateButtons = builder:build_buttons_inline_date(),
  Text = <<"Промежуточки времени отмечены. Теперь выберем даты"/utf8>>,
  sendMessageClient(Cid, Text, inline_keyboard, DateButtons, {recording_on_date, MarkTime});
handle_cast( {Cid, {Buttons, <<"Cancel">>}}, {recording_on_time, MsgID}) ->
  sendMessageClient(Cid, <<"Запись отменена. Можем повторить командой /rec"/utf8>>, state);
handle_cast( {Cid, {Buttons, Button}}, {recording_on_time, MsgID}) ->
  NewButtons = checkbox_handler(Buttons, Button),
  editMessageClient(Cid, {recording_on_time, MsgID}, NewButtons);
handle_cast( {Cid, _Else}, {recording_on_time, _MsgID}) ->
  sendMessageClient(Cid, <<"Операция прервана, но можем повторить"/utf8>>, state);
handle_cast( {Cid, {DateButtons, <<"Enter">>}}, {{recording_on_date, MarkTime}, MsgID}) ->
  MarkDate = parser:get_marks(DateButtons),
  case gen_server:call(?BM, {create_record, Cid, {MarkDate, MarkTime}}) of
    hes_marked -> sendMessageClient(Cid, <<"Ты че опять записываешься, пёс? Иди гуляй"/utf8>>, state);
    oshibochka -> sendMessageClient(Cid, <<"Чет какая-то ошибочка. Я не знаю, что произошло"/utf8>>, state);
    {atomic, all_busy} -> sendMessageClient(Cid, <<"К сожалению, все занято. Попробуйте выбрать другое время и дату"/utf8>>, state);
    {atomic, {Date, Time}} ->
      Text = <<"Вы успешно записались в очередь на "/utf8, Date/binary, " в "/utf8, Time/binary>>,
      sendMessageClient(Cid, Text, state);
    hes_cheater -> sendMessageClient(Cid, <<"Ах ты читерская жопа, как тебе удалось записаться?"/utf8>>, state);
    not_registered -> sendMessageClient(Cid, <<"Слышь, пёс, катись регистрироваться"/utf8>>, state)
  end;
handle_cast( {Cid, {Buttons, <<"Cancel">>}}, {{recording_on_date, MarkTime}, MsgID}) ->
  sendMessageClient(Cid, <<"Запись отменена, но можем повторить командой /rec"/utf8>>, state);
handle_cast( {Cid, {Buttons, Button}}, {{recording_on_date, MarkTime}, MsgID}) ->
  NewButtons = checkbox_handler(Buttons, Button),
  editMessageClient(Cid, {{recording_on_date, MarkTime}, MsgID}, NewButtons);
handle_cast( {Cid, _Else}, {{recording_on_date, _MarkTime}, _MsgID}) ->
  sendMessageClient(Cid, <<"Операция прервана, но можем повторить"/utf8>>, state);

handle_cast( {Cid, <<"/not">>}, _State) ->
  sendMessageClient(Cid, <<"За сколько минут предупредить о начале события?"/utf8>>, notification_of_begin);
handle_cast( {Cid, Time}, notification_of_begin) ->
  sendMessageClient(Cid, <<"Уведомление установлено">>);

handle_cast( {Cid, <<"Привет"/utf8>>}, _State) ->
  sendMessageClient(Cid, << 226,152,173, <<"Привет"/utf8>>/binary>>, state);
handle_cast( {Cid, <<"Hello"/utf8>>}, _State) ->
  sendMessageClient(Cid, <<"I need your skills"/utf8>>, trigger);
handle_cast( {Cid, <<"You son over bitch. I'm in">>}, trigger) ->
  sendMessageClient(Cid, <<"You are the Rick's friend">>, state);

handle_cast( {Cid, <<"можем повторить?"/utf8>>}, _State) ->
  sendMessageClient(Cid, <<"конечно можем и не раз, весь мир в труху, но потом..."/utf8>>, state);

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

checkbox_handler([], _Label) -> [];
checkbox_handler([{<<"text">>, SValue},{<<"callback_data">>, Mark}], Label) ->
  case {Mark =:= Label, SValue} of
    {true, <<226,156,133,QSValue/binary>>} -> [{<<"text">>, <<QSValue/binary>>},{<<"callback_data">>, Mark}];
    {true, _} -> [{<<"text">>, << 226,156,133, SValue/binary>>},{<<"callback_data">>, Mark}];
    {false, _} -> [{<<"text">>, SValue},{<<"callback_data">>, Mark}]
  end;
checkbox_handler([Head|Tail], Label) ->
  [checkbox_handler(Head,Label)] ++ checkbox_handler(Tail,Label).

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

editMessageClient(Cid, {State, MsgID}, Buttons) ->
  Msg = [{<<"chat_id">>, Cid}, {<<"message_id">>, MsgID}] ++ [{<<"reply_markup">>, [{<<"inline_keyboard">>, Buttons}] }],
  {ok, Result} = httpc:request(
    post,
    builder:build_request("editMessageReplyMarkup", Msg),
    [],
    [{body_format, binary}]
  ),
  {noreply, {State, MsgID}, infinity}.

sendMessageClient(Cid, Text, State) ->
  sendMessageClient(builder:build_Msg(Cid, Text), State).
sendMessageClient(Cid, Text, keyboard, Buttons, State) ->
  sendMessageClient(builder:build_keyboard(Cid, Text, Buttons), State);
sendMessageClient(Cid, Text, inline_keyboard, Buttons, State) ->
  {ok, Result} = sendMessageClient(builder:build_inline_keyboard(Cid, Text, Buttons)),
  io:format("~p~n", [Result]),
  {_,_,Body} = Result,
  {_, MsgID} = parser:parse(Body, sendMessage),
  {noreply, {State, MsgID}, infinity}.
sendMessageClient(Cid, Text, hide_keybroad, State) ->
  sendMessageClient(builder:build_hide_keyboard(Cid, Text), State).

sendMessageClient(Msg) ->
  httpc:request(
    post,
    builder:build_request("sendMessage", Msg),
    [],
    [{body_format, binary}]
  ).
sendMessageClient(Msg, State) ->
  {ok, Result} = sendMessageClient(Msg),
  io:format("~p~n", [Result]),
  {noreply, State, infinity}.
