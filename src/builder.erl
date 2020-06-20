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

build_buttons_inline_time() ->
  [
    [
      [{<<"text">>, <<"10:00-11:00">>},{<<"callback_data">>, <<"10">>}],
      [{<<"text">>, <<"11:00-12:00">>},{<<"callback_data">>, <<"11">>}],
      [{<<"text">>, <<"12:00-13:00">>},{<<"callback_data">>, <<"12">>}]
    ],
    [
      [{<<"text">>, <<"13:00-14:00">>},{<<"callback_data">>, <<"13">>}],
      [{<<"text">>, <<"14:00-15:00">>},{<<"callback_data">>, <<"14">>}],
      [{<<"text">>, <<"15:00-16:00">>},{<<"callback_data">>, <<"15">>}]
    ],
    [
      [{<<"text">>, <<"16:00-17:00">>},{<<"callback_data">>, <<"16">>}],
      [{<<"text">>, <<"17:00-18:00">>},{<<"callback_data">>, <<"17">>}],
      [{<<"text">>, <<"18:00-19:00">>},{<<"callback_data">>, <<"18">>}]
    ],
    [
      [{<<"text">>, <<"19:00-20:00">>},{<<"callback_data">>, <<"19">>}],
      [{<<"text">>, <<"20:00-21:00">>},{<<"callback_data">>, <<"20">>}],
      [{<<"text">>, <<"21:00-22:00">>},{<<"callback_data">>, <<"21">>}]
    ],
    [
      [{<<"text">>, <<"Принять"/utf8>>},{<<"callback_data">>, <<"Enter">>}],
      [{<<"text">>, <<"Отменить"/utf8>>},{<<"callback_data">>, <<"Cancel">>}]
    ]
  ].