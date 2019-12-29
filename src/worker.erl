-module(worker).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").
-include("db_schema.hrl").

start() ->
  spawn_link(?MODULE, looper, []).

looper() ->
  receive
    {Cid, <<"/start">>} ->
      case is_registered(Cid) of
        true -> sendMessageClient(Cid, <<"Повторная регистрация не требуется"/utf8>>);
        false ->
          Text = <<"Привет, друг! Введи пожалуйста свои ФИО, чтобы мы дальше могли общаться без проблем"/utf8>>,
          sendMessageClient(Cid, Text),
          registration(Cid)
      end;
    {Cid, <<"/rec">>} ->
      sendMessageClient(Cid, <<"На какое время хотите записаться?"/utf8>>);
    {Cid, <<"Привет"/utf8>>} ->
      Text = <<"Привет"/utf8>>,
      sendMessageClient(Cid, Text);
    {Cid, <<"Hello"/utf8>>} ->
      Text = <<"I need your skills"/utf8>>,
      sendMessageClient(Cid, Text);
    {Cid, <<"You son over bitch. I'm in">>} ->
      Text = <<"You are the Rick's friend">>,
      sendMessageClient(Cid, Text);
    {Cid, <<"/calc"/utf8>>} ->
      sendMessageClient(Cid, <<"Please, enter your expression">>),
      loop_calc();
    {Cid, <<"/lazy_calc"/utf8>>} ->
      sendMessageClient(Cid, <<"Please, enter your expression">>),
      loop_lazy_calc();
    {Cid, Else} ->
      Text = <<"Incorrect command">>,
      io:format("~p~n", [Else]),
      sendMessageClient(Cid, Text)
  end,
  looper().

is_registered(Cid) ->
  F = fun() -> builder:build_select_db_client('$1', '$2', '$3', '$4', Cid) end,
  case mnesia:transaction(F) of
    {atomic, []} ->
      false;
    {atomic, Record} ->
      true
  end.

registration(Cid) ->
  receive
    {Cid, <<"Ja">>} ->
      sendMessageClient(Cid, <<"Введите пожалуйста ФИО снова"/utf8>>, hide_keyboard),
      registration(Cid);
    {Cid, <<"Nein">>} ->
      sendMessageClient(Cid, <<"При желании, зарегистрироваться в сервисе можете набрав команду /start"/utf8>>, hide_keyboard);
    {Cid, Message} ->
      Split = try builder:split_name(Message)
      catch error:Reason1 -> {no_name}
      end,
      F = case Split of
        {_, LastName, FirstName, MiddleName} ->
          fun() -> 
            case builder:build_select_db_client('$1', LastName, FirstName, MiddleName, '$2') of
              [] -> no_record;
              [[Rid, undefined]] ->
                  builder:build_update_db_client(Rid, LastName, FirstName, MiddleName, Cid);
              [[Rid, DbCid]] -> DbCid == Cid
            end
          end;
        {no_name} ->
          sendMessageClient(Cid, <<"Неверно введены ФИО"/utf8>>),
          no_name
      end,
      try mnesia:transaction(F) of
        {atomic, no_record} -> sendMessageClient(Cid, <<"Таких ФИО нет в базе, хотите попробовать еще раз?"/utf8>>, keyboard, [<<"Ja"/utf8>>, <<"Nein"/utf8>>]), registration(Cid);
        {atomic, ok} -> sendMessageClient(Cid, <<"Вы успешно зарегистрированны в базе"/utf8>>);
        {atomic, true} -> sendMessageClient(Cid, <<"Таки вы уже в списке"/utf8>>);
        {atomic, false} -> sendMessageClient(Cid, <<"Такой человек уже зарегистрирован, хотите попробовать еще раз?"/utf8>>, keyboard, [<<"Ja"/utf8>>, <<"Nein"/utf8>>]), registration(Cid)
      catch error:Reason2 -> io:format("error in db, method registration")
      end
  end.

loop_calc() ->
  receive
    {Cid, Expression} ->
      try calculator:calc(Expression) of
        empty ->
          Text = <<"Your expression is empty">>,
          sendMessageClient(Cid, Text);
        no_numbers ->
          Text = <<"Incorrect expression">>,
          sendMessageClient(Cid, Text);
        Result -> 
          DispFunc = fun(Elem) -> 
            timer:sleep(1000), 
            sendMessageClient(Cid, Elem) 
          end,
          [DispFunc(Elem) || Elem <- Result]
      catch
        error:Reason -> sendMessageClient(Cid, <<"Incorrect expression">>)
      end
  end.

loop_lazy_calc() ->
  receive
    {Cid, Expression} ->
      FirstFun = calculator:lazy_calc(Expression),
      try FirstFun() of
        [empty|stop] ->
          sendMessageClient(Cid, <<"empty">>);
        [Bin|stop] ->
          sendMessageClient(Cid, Bin);
        [Bin|Fun] ->
          sendMessageClient(Cid, Bin),
          loop_lazy_calc(Fun)
      catch 
        error:Reason -> sendMessageClient(Cid, <<"Incorrect expression">>)
      end;
    {Cid, Else} ->
      sendMessageClient(Cid, <<"You could see more if /lazy_calc_next call">>),
      io:format("~p~n", [binary_to_list(Else)])
  end.
loop_lazy_calc(stop) -> ok;
loop_lazy_calc(Fun) ->
  receive
    {Cid, <<"/lazy_calc_next"/utf8>>} ->
      try Fun() of
        [no_numbers|Fun] ->
          sendMessageClient(Cid, <<"no_numbers">>);
        [Bin|stop] ->
          sendMessageClient(Cid, Bin);
        [Bin|NewFun] ->
          sendMessageClient(Cid, Bin),
          loop_lazy_calc(NewFun)
      catch
        error:Reason -> sendMessageClient(Cid, <<"I cannot solve the next expression">>)
      end;
    {Cid, Else} ->
      sendMessageClient(Cid, <<"Ok, I will stop to calc next">>),
      io:format("~p~n", [binary_to_list(Else)])
  end.

sendMessageClient(Cid, Text) ->
  {ok, Result} = httpc:request(
    post,
    builder:build_request("sendMessage", builder:build_Msg(Cid, Text)), 
    [],
    [{body_format, binary}]
  ),
  io:format("~p~n", [Result]),
  ok.

sendMessageClient(Cid, Text, keyboard, Buttons) ->
  {ok, Result} = httpc:request(
    post,
    builder:build_request("sendMessage", builder:build_keyboard(Cid, Text, Buttons)),
    [],
    [{body_format, binary}]
  ),
  io:format("keyboard ~p~n", [Result]),
  ok.
sendMessageClient(Cid, Text, hide_keyboard) ->
  {ok, Result} = httpc:request(
    post,
    builder:build_request("sendMessage", builder:build_hide_keyboard(Cid, Text)),
    [],
    [{body_format, binary}]
  ),
  io:format("hide keyboard ~p~n", [Result]),
  ok.
