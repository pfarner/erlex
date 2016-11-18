-module(mounts).
-export([get_mounts/0,get_types/0,get_mount/1,get_type/1]).

get_mounts() ->
  Data = raw_read_file("/etc/mtab"),
  parse_mounts(binary:split(Data, [<<"\n">>], [global]), #{}).

get_types() ->
  Data = raw_read_file("/etc/mtab"),
  parse_types(binary:split(Data, [<<"\n">>], [global]), #{}).

% See also http://git.savannah.gnu.org/cgit/coreutils.git/tree/src/df.c
get_mount(Path) ->
  AbsPath = abspath(Path),
  try maps:get(AbsPath, get_mounts())
  catch error:bad_key ->
    case AbsPath of
      <<"/">> ->
        throw("no_mount");
      _ ->
        get_mount(filename:dirname(AbsPath))
    end
  end.

get_type(Path) ->
  AbsPath = abspath(Path),
  try maps:get(AbsPath, get_types())
  catch error:bad_key ->
    case AbsPath of
      <<"/">> ->
        throw("no_type");
      _ ->
        get_type(filename:dirname(AbsPath))
    end
  end.

abspath(Path) ->
  AbsPath = filename:absname(iolist_to_binary(Path)),
  case binary:at(AbsPath, 0) of
    $/ ->
      AbsPath;
    _  ->
      throw(io_lib:format("absolute path not absolute: ~p became ~p", [Path, AbsPath]))
  end.


raw_read_file(Path) ->
  {ok, File} = file:open(Path, [read, binary]),
  raw_read_loop(File, []).

raw_read_loop(File, Acc) ->
  case file:read(File, 1024) of
    {ok, Bytes} ->
      raw_read_loop(File, [Acc | Bytes]);
    eof ->
      file:close(File),
      iolist_to_binary(Acc);
    {error, Reason} ->
      file:close(File),
      erlang:error(Reason)
    end.

parse_mounts([<<>> | Rest], Map) ->
  Map;
parse_mounts([Blob | Rest], Map) ->
  [Partition | [Mount | _]] = binary:split(Blob, [<<" ">>], [global]),
  parse_mounts(Rest, maps:put(Mount, Partition, Map));
parse_mounts([], Map) ->
  Map.

parse_types([<<>> | Rest], Map) ->
  Map;
parse_types([Blob | Rest], Map) ->
  [Partition | [Mount | [Type | _]]] = binary:split(Blob, [<<" ">>], [global]),
  parse_types(Rest, maps:put(Mount, Type, Map));
parse_types([], Map) ->
  Map.
