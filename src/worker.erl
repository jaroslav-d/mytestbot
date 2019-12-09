-module(worker).
-compile(export_all).

start() ->
  spawn_link(?MODULE, looper, []).

looper() ->
  receive
    {Cid, <<"/start">>} ->
      Text = <<"I need your skills">>,
      io:format("Hi! ~n"),
      sendMessageClient(Cid, Text);
    {Cid, <<"привет"/utf8>>} ->
      Text = <<"привет"/utf8>>,
      sendMessageClient(Cid, Text);
    {Cid, <<"You son over bitch. I'm in">>} ->
      Text = <<"You are a friend Rick's team">>,
      sendMessageClient(Cid, Text);
    {Cid, <<"/calc"/utf8>>} ->
      sendMessageClient(Cid, <<"Please, enter your expression">>),
      loop_calc();
    {Cid, <<"/lazy_calc "/utf8, Expression/bytes>>} ->
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
      Text = <<"Incorrect command">>,
      io:format("~p~n", [Else]),
      sendMessageClient(Cid, Text)
  end,
  looper().

loop_calc() ->
  receive
    {Cid, Expression} ->
      case calculator:calc(Expression) of
        empty ->
          Text = <<"Your expression is empty">>,
          sendMessageClient(Cid, Text);
        Result -> 
          DispFunc = fun(Elem) -> 
            timer:sleep(1000), 
            sendMessageClient(Cid, Elem) 
          end,
          [DispFunc(Elem) || Elem <- Result]
      end
  end.

loop_lazy_calc(stop) -> ok;
loop_lazy_calc(Fun) ->
  receive
    {Cid, <<"/lazy_calc next"/utf8>>} ->
      case Fun() of
        [empty|stop] ->
          Text = <<"Please, enter an expression separated by a space">>,
          sendMessageClient(Cid, Text);
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
