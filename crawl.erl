-module(crawl).
-export([print/1,map/2]).

print(Root) ->
  map(Root, fun(File) ->
    io:format("~s~n", [File])
    end).

map(Root, Function) ->
  Function(Root),
  case file:list_dir(Root) of
    {ok, Files} ->
      lists:map(
        fun(File) ->
	  map(Root ++ "/" ++ File, Function)
	  end, Files);
    Error -> Error
  end.
