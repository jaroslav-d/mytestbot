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

build_select_db_mark(Cid, Date, Time) ->
  mnesia:select(mark, [{build_rd_mark(Cid, Date, Time), [], ['$$']}]).

build_select_db_list(Id, Date, Time, Clients) ->
  mnesia:select(list, [{build_rd_list(Id, Date, Time, Clients), [], ['$$']}]).

build_update_db_client(Rid, LastName, FirstName, MiddleName, Cid) ->
  mnesia:write(build_rd_client(Rid, LastName, FirstName, MiddleName, Cid)).

build_update_db_mark(Cid, Date, Time) ->
  mnesia:write(build_rd_mark(Cid, Date, Time)).

build_update_db_list(Id, Date, Time, Clients) ->
  mnesia:write(build_rd_list(Id, Date, Time, Clients)).

build_rd_client(Rid, LastName, FirstName, MiddleName, Cid) ->
  #client{
    rid = Rid,
    last_name = LastName,
    first_name = FirstName,
    middle_name = MiddleName,
    cid = Cid
  }.

build_rd_list(Id, Date, Time, Clients) ->
  #list{
    id = Id,
    date = Date,
    time = Time,
    clients = Clients
  }.

build_rd_mark(Cid, Date, Time) ->
  #mark{
    cid = Cid,
    date = Date,
    time = Time
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
      [{<<"text">>, <<"10:00-11:00">>},{<<"callback_data">>, <<"10:00">>}],
      [{<<"text">>, <<"11:00-12:00">>},{<<"callback_data">>, <<"11:00">>}],
      [{<<"text">>, <<"12:00-13:00">>},{<<"callback_data">>, <<"12:00">>}]
    ],
    [
      [{<<"text">>, <<"13:00-14:00">>},{<<"callback_data">>, <<"13:00">>}],
      [{<<"text">>, <<"14:00-15:00">>},{<<"callback_data">>, <<"14:00">>}],
      [{<<"text">>, <<"15:00-16:00">>},{<<"callback_data">>, <<"15:00">>}]
    ],
    [
      [{<<"text">>, <<"16:00-17:00">>},{<<"callback_data">>, <<"16:00">>}],
      [{<<"text">>, <<"17:00-18:00">>},{<<"callback_data">>, <<"17:00">>}],
      [{<<"text">>, <<"18:00-19:00">>},{<<"callback_data">>, <<"18:00">>}]
    ],
    [
      [{<<"text">>, <<"19:00-20:00">>},{<<"callback_data">>, <<"19:00">>}],
      [{<<"text">>, <<"20:00-21:00">>},{<<"callback_data">>, <<"20:00">>}],
      [{<<"text">>, <<"21:00-22:00">>},{<<"callback_data">>, <<"21:00">>}]
    ],
    [
      [{<<"text">>, <<"Принять"/utf8>>},{<<"callback_data">>, <<"Enter">>}],
      [{<<"text">>, <<"Отменить"/utf8>>},{<<"callback_data">>, <<"Cancel">>}]
    ]
  ].

build_buttons_inline_date() ->
  [
    [
      [{<<"text">>, <<"понедельник"/utf8>>},{<<"callback_data">>, <<"пн"/utf8>>}],
      [{<<"text">>, <<"вторник"/utf8>>},{<<"callback_data">>, <<"вт"/utf8>>}]
    ],
    [
      [{<<"text">>, <<"среда"/utf8>>},{<<"callback_data">>, <<"ср"/utf8>>}],
      [{<<"text">>, <<"четверг"/utf8>>},{<<"callback_data">>, <<"чт"/utf8>>}]
    ],
    [
      [{<<"text">>, <<"пятница"/utf8>>},{<<"callback_data">>, <<"пт"/utf8>>}],
      [{<<"text">>, <<"суббота"/utf8>>},{<<"callback_data">>, <<"сб"/utf8>>}]
    ],
    [
      [{<<"text">>, <<"воскресенье"/utf8>>},{<<"callback_data">>, <<"вс"/utf8>>}]
    ],
    [
      [{<<"text">>, <<"Принять"/utf8>>},{<<"callback_data">>, <<"Enter">>}],
      [{<<"text">>, <<"Отменить"/utf8>>},{<<"callback_data">>, <<"Cancel">>}]
    ]
  ].