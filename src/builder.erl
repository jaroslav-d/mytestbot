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
build_hide_keyboard(Cid, Text) ->
  build_Msg(Cid, Text) ++ [{<<"reply_markup">>, [{<<"hide_keyboard">>, true}]}].

build_select_db_client(Rid, LastName, FirstName, MiddleName, Cid) ->
  mnesia:select(client, [{build_rd_client(Rid, LastName, FirstName, MiddleName, Cid), [], ['$$']}]).

build_update_db_client(Rid, LastName, FirstName, MiddleName, Cid) ->
  mnesia:write(build_rd_client(Rid, LastName, FirstName, MiddleName, Cid)).

build_rd_client(Rid, LastName, FirstName, MiddleName, Cid) ->
  #client{rid = Rid, last_name = LastName, first_name = FirstName, middle_name = MiddleName, cid = Cid}.

build_write_db_new_client(Rid, BMessage) ->
  try split_name(BMessage) of
    {_, LastName, FirstName, MiddleName} ->
      mnesia:transaction(fun() -> mnesia:write(#client{rid = Rid,last_name = LastName,first_name = FirstName,middle_name = MiddleName}) end)
  catch error:Reason -> no_names
  end.

split_name(BMessage) ->
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
