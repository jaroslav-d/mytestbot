-record(client, {rid, last_name, first_name, middle_name, cid}).
-record(mark, {cid, date, time}).
-record(list, {id, date, time, clients=[]}).

%%mnesia:create_table(client, [{disc_copies, [nonode@nohost]}, {attributes, record_info(fields, client)}]).
%%mnesia:create_table(mark, [{disc_copies, [nonode@nohost]}, {attributes, record_info(fields, mark)}]).
%%mnesia:create_table(list, [{disc_copies, [nonode@nohost]}, {attributes, record_info(fields, list)}]).


%%DateFromFile = [<<"пн"/utf8>>,<<"вт"/utf8>>,<<"ср"/utf8>>,<<"чт"/utf8>>,<<"пт"/utf8>>,<<"сб"/utf8>>,<<"вс"/utf8>>].
%%TimeFromFile = [<<"10:00"/utf8>>,<<"11:00"/utf8>>,<<"12:00"/utf8>>,<<"13:00"/utf8>>,<<"14:00"/utf8>>,<<"15:00"/utf8>>,<<"16:00"/utf8>>,<<"17:00"/utf8>>,<<"18:00"/utf8>>,<<"19:00"/utf8>>,<<"20:00"/utf8>>,<<"21:00"/utf8>>].
%%mnesia:transaction(
%%  fun () ->
%%    [
%%      mnesia:write(
%%          #list{
%%            id=Id,
%%            date=Date,
%%            time=Time
%%          }
%%      )
%%      ||
%%      {Id, {Date, Time}} <-
%%        lists:zip(
%%          lists:seq(1,length(DateFromFile)*length(TimeFromFile)),
%%          [ {Date, Time} || Date <- DateFromFile, Time <- TimeFromFile]
%%        )
%%    ]
%%  end
%%).

%%mnesia:transaction(fun () -> [mnesia:write(#list{id=Id, date=Date, time=Time})|| {Id, {Date, Time}} <- lists:zip(lists:seq(1,length(DateFromFile)*length(TimeFromFile)), [ {Date, Time} || Date <- DateFromFile, Time <- TimeFromFile])] end).

%%if you want to see table in db when write command in terminal observer:start()