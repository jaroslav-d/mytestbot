-module(worker).
-compile(export_all).

start() ->
  spawn_link(?MODULE, looper, []).

looper() ->
  receive
    {Cid, <<"/start">>} ->
      Text = <<"I need your skills">>,
      io:format("Hi! ~n"),
      sendMessageCient(Cid, Text);
    {Cid, <<"привет"/utf8>>} ->
      Text = <<"привет"/utf8>>,
      sendMessageCient(Cid, Text);
    {Cid, <<"You son over bitch. I'm in">>} ->
      Text = <<"You are a friend Rick's team">>,
      sendMessageCient(Cid, Text);
    {Cid, Else} ->
      Text = <<"Incorrect command">>,
      io:format("~p~n", [Else]),
      sendMessageCient(Cid, Text)
  end,
  looper().

sendMessageCient(Cid, Text) ->
  {ok, Result} = httpc:request(
    post,
    builder:build_request("sendMessage", builder:build_Msg(Cid, Text)), 
    [],
    [{body_format, binary}]
  ),
  io:format("~p~n", [Result]).
