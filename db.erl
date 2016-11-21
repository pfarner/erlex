-module(db).
-export([start/0, stop/0, create_table/2, create_table/3, insert/2, print/1, get_by/3, get_max/3]).

% start up databases in the abstract
start() ->
  Schema = mnesia:create_schema([node()]),
  case Schema of
    ok ->
      ok;
    {error, {Name, {already_exists, Name}}} ->
      ok;
    SchemaError ->
      throw(io_lib:format("Failed to create schema: ~p~n", [SchemaError]))
  end,
  case application:start(mnesia) of
    ok ->
      ok;
    StartError ->
      throw(io_lib:format("Failed to start mnesia: ~p~n", [StartError]))
  end.

stop() ->
  application:stop(mnesia).

% create the table if it does not exist, with a default timeout
create_table(Table, Record) ->
  create_table(Table, Record, 10000).

% create the table if it does not exist
create_table(Table, Record, Timeout) ->
  case mnesia:wait_for_tables([filesystem], Timeout) of
    ok ->
      io:format("Found table: ~p~n", [Table]),
      ok;
    {timeout, BadTabList} ->
      io:format("Timed out on tables: ~p~n", BadTabList),
      mnesia:create_table(Table, [{attributes, Record}, {disc_copies, [node()]}]),
      ok;
    {error, Reason} ->
      throw(io_lib:format("Failed to wait for ~p: ~p~n", [Table, Reason]))
    end,
  ok.

% Add the Row to the Table as an upsert
insert(Table, Row) ->
  io:format("Inserting ~p into ~p~n", [Row, Table]),
  F = fun() ->
    io:format("Write ~p~n", [Row]),
    mnesia:write(Row)
    end,
  case mnesia:transaction(F) of
    {atomic, ok} ->
      ok;
    Error ->
      throw(io_lib:format("Failed to insert ~p into ~p: ~p~n", [Row, Table, Error]))
  end.

% Print the Table to standard output
print([Head | List]) ->
  print(iolist_to_binary([Head | List])),
  io:format("List: ~p~n", [List]);
print(Table) ->
  io:format("Table ~p:~n", [Table]),
  io:format("print/2 dirty_first on ~p~n", [Table]),
  case mnesia:dirty_first(Table) of
    '$end_of_table' ->
      io:format("print/2 end~n", []),
      ok;
    First ->
      io:format("print/1 First: ~p~n", [First]),
      print(Table, First),
      ok
  end.

% Print the Table to standard output, starting at Prev
print(Table, Prev) ->
  case mnesia:dirty_next(Table, Prev) of
    '$end_of_table' ->
      ok;
    First ->
      io:format("print/2 First: ~p~n", [First]),
      [Row] = mnesia:dirty_read({Table, First}),
      io:format("  ~p~n", [Row]),
      print(Table, First),
      ok
  end.

% Load the entire table
load(Table) ->
  load(Table, []).

load(Table, Accum) ->
  case mnesia:dirty_first(Table) of
    '$end_of_table' ->
      Accum;
    First ->
      [Row] = mnesia:dirty_read({Table, First}),
      [Row | Accum]
    end.

%% Find all rows with the specified Value for the Field
%find_all_matches(Table, Field, Value) ->
%  find_all_matches(Table, Field, Value, []).
%
%find_all_matches(Table, Field, Value, List) ->
%  case mnesia:dirty_first(Table) of
%    '$end_of_table' ->
%      io:format("End of table~n", []),
%      List;
%    First ->
%      [Row] = mnesia:dirty_read({Table, First}),
%      io:format("Row: ~p~n", [Row]),
%      %must read the rows, filter for the field, and recurse
%      ok
%  end,
%  [1].
%
%% Find the single row with the specified Value for the Field
%find(Table, Field, Value) ->
%  All = find_all_matches(Table, Field, Value),
%  case All of
%    [Result] ->
%      Result;
%    [First | Others] ->
%      throw(io_lib:format("Too many results: ~p~n", [First | Others]));
%    Error ->
%      throw(io_lib:format("Strange result ~p~n", [Error]))
%  end.

get_by(Table, Type, Field) ->
  List = load(Table),
  group_list(List, index(Type, Field)+1).

index(List, Element) ->
  case index(List, Element, 1) of
    not_found ->
      throw(io_lib:format("Unable to find ~p in ~p~n", [Element, List]));
    Index ->
      Index
  end.

index([], _, _) ->
  not_found;
index([First | List], Element, NextIndex) ->
  case First of
    Element ->
      NextIndex;
    _ ->
      index(List, Element, NextIndex+1)
  end.

%group_map(InputMap, Index) ->
%  Fun = fun(K, V, AccIn) ->
%    Key = element(Index, V),
%    maps:put(Key, V, AccIn)
%    end,
%  io:format("HERE~n", []),
%  maps:fold(Fun, #{}, InputMap).

group_list(InputList, Index) ->
  Fun = fun(V, AccIn) ->
    Key = element(Index, V),
    try maps:get(Key, AccIn) of
      Existing ->
        throw(io_lib:format("Conflicting values for ~p: ~p and ~p~n", [Key, Existing, V]))
    catch
      error:bad_key ->
        maps:put(Key, V, AccIn)
    end
    end,
  lists:foldl(Fun, #{}, InputList).

get_max(Table, Type, Field) ->
  List  = load(Table),
  Index = index(Type, Field)+1,
  get_max_internal(List, Index, 0).

get_max_internal([], _, Max) ->
  Max;
get_max_internal([First | Rest], Index, Max) ->
  Key = element(Index, First),
  if
    Key > Max ->
      get_max_internal(Rest, Index, Max);
    true ->
      get_max_internal(Rest, Index, Key)
  end.
