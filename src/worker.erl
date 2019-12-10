-module(worker).
-compile(export_all).
-include_lib("stdlib/include/qlc.hrl").
-include("db_schema.hrl").

start() ->
  spawn_link(?MODULE, looper, []).

looper() ->
  receive
    {Cid, <<"/start">>} ->
      Text = <<"Привет, друг! Введи пожалуйста свои ФИО, чтобы мы дальше могли общаться без проблем"/utf8>>,
      sendMessageClient(Cid, Text),
      is_registered(Cid),
      loop_registration(Cid);
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
  F = fun() ->
    mnesia:select(
      client, 
      [
        {#client{
          cid = Cid, 
          second_name = '$1', 
          first_name = '$2', 
          third_name = '$3'
        }, 
        [], 
        ['$1','$2','$3']}
      ]
    )
  end,
  case mnesia:transaction(F) of
    {atomic, []} ->
      false;
    {_, [Record]} -> 
      true
  end.

loop_registration(Cid) ->
  receive
    {Cid, Message} ->
      Parts = try split_record_db(Cid, Message)
      catch error:Reason -> bad_format
      end,
      if Parts == bad_format ->
        sendMessageClient(Cid,<<"Неверно введены ФИО"/utf8>>);
      true ->
        {_, SecondName, FirstName, ThirdName} = Parts,
        F = fun() -> 
          mnesia:write(
            #client{
              cid = Cid, 
              second_name = SecondName, 
              first_name = FirstName, 
              third_name = ThirdName
            }
          ) 
        end,
        mnesia:transaction(F)
      end
  end.

split_record_db(Cid, BMessage) ->
  Message = binary_to_list(BMessage),
  lists:foldl(
    fun(X, {Idx, List1, List2, List3}) -> 
      case {X,Idx} of 
        {32,_} -> {Idx + 1, List1, List2, List3}; 
        {_,0} -> {Idx, List1 ++ [X], List2, List3};
        {_,1} -> {Idx, List1, List2 ++ [X], List3};
        {_,2} -> {Idx, List1, List2, List3 ++ [X]} 
      end
    end,
    {0, [], [], []},
    Message
  ).

loop_calc() ->
  receive
    {Cid, Expression} ->
      try calculator:calc(Expression) of
        empty ->
          Text = <<"Your expression is empty">>,
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
      case FirstFun() of
        [empty|stop] ->
          Text = <<"Please, enter an expression separated by a space">>,
          sendMessageClient(Cid, Text);
        [Bin|stop] ->
          sendMessageClient(Cid, Bin);
        [Bin|Fun] ->
          sendMessageClient(Cid, Bin),
          loop_lazy_calc(Fun)
      end;
    {Cid, Else} ->
      io:format("~p~n", [binary_to_list(Else)])      
  end.
loop_lazy_calc(stop) -> ok;
loop_lazy_calc(Fun) ->
  receive
    {Cid, <<"/lazy_calc_next"/utf8>>} ->
      case Fun() of
        [Bin|stop] ->
          sendMessageClient(Cid, Bin);
        [no_numbers|stop] ->
          ok;
        [Bin|NewFun] ->
          sendMessageClient(Cid, Bin),
          loop_lazy_calc(NewFun)
      end;
    {Cid, Else} ->
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
