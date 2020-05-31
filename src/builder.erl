-module(builder).
-compile(export_all).
-include("db_schema.hrl").
-include("token.hrl").

build_request(Command, Params) ->
  Url = ?URL ++ "/" ++ Command,
  Headers = [{"Content-Type","application/json"}],
  ContentType = "application/json",
  io:format("~p~n", [Params]),
  {Url, Headers, ContentType, jsx:encode(Params)}.

build_Msg(Cid, Text) ->
  [{<<"chat_id">>, Cid}, {<<"text">>, Text}].
build_keyboard(Cid, Text, Buttons) ->
  Iter = fun(X, Acc) -> Acc ++ [[[{<<"text">>, X}]]] end,
  build_Msg(Cid, Text) ++ [{<<"reply_markup">>, [{<<"keyboard">>, lists:foldl(Iter, [], Buttons) }] }].
build_inline_keyboard(Cid, Text, Buttons) ->
  build_Msg(Cid, Text) ++ [{<<"reply_markup">>, [{<<"inline_keyboard">>, Buttons}] }].
build_hide_keyboard(Cid, Text) ->
  build_Msg(Cid, Text) ++ [{<<"reply_markup">>, [{<<"hide_keyboard">>, true}]}].

build_select_db_client(Rid, LastName, FirstName, MiddleName, Cid) ->
  mnesia:select(client, [{build_rd_client(Rid, LastName, FirstName, MiddleName, Cid), [], ['$$']}]).

build_update_db_client(Rid, LastName, FirstName, MiddleName, Cid) ->
  mnesia:write(build_rd_client(Rid, LastName, FirstName, MiddleName, Cid)).

build_rd_client(Rid, LastName, FirstName, MiddleName, Cid) ->
  #client{
    rid = Rid,
    last_name = LastName,
    first_name = FirstName,
    middle_name = MiddleName,
    cid = Cid
  }.

build_write_db_new_client(Rid, BMessage) ->
  case binary:split(BMessage, [<<" "/utf8>>], [global]) of
    [LastName, FirstName, MiddleName] ->
      F = fun() -> build_update_db_client(Rid,LastName,FirstName,MiddleName,undefined) end,
      mnesia:transaction(F);
    _Else -> no_names
  end.