-module(builder).
-compile(export_all).

-define(URL, "https://api.telegram.org/bot932261333:AAGc0RpQwtFnGDWFYZcvrhBghe1i-wldh0k").

build_request(Command, Params) ->
  Url = ?URL ++ "/" ++ Command,
  Headers = [{"Content-Type","application/json"}],
  ContentType = "application/json",
  io:format("~p~n", [Params]),
  {Url, Headers, ContentType, jsx:encode(Params)}.

build_Msg(Cid, Text) ->
  [{<<"chat_id">>, Cid}, {<<"text">>, Text}].
