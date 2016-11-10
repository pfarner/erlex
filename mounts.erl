-module(mounts).
-export([get_mounts/0,get_mount/1]).

get_mounts() ->
  Data = raw_read_file("/etc/mtab"),
  parse_mounts(binary:split(Data, [<<"\n">>], [global]), #{}).
  % Must split and parse the lines

get_mount(Path) ->
  AbsPath = iolist_to_binary(filename:absname(Path)),
  try maps:get(AbsPath, get_mounts())
  catch error:bad_key ->
    case AbsPath of
      "/" ->
        throw("no_mount");
      <<"/">> ->
        throw("no_mount");
      _ ->
        get_mount(filename:dirname(AbsPath))
    end
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
