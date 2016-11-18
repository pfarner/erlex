-module(mounts).
-export([get_mounts/0,get_mount/1,get_type/1,get_options/1,get_fs_freq/1,get_fs_passno/1]).

get_mtab_data(Index) ->
  Data = raw_read_file("/etc/mtab"),
  parse_field(binary:split(Data, [<<"\n">>], [global]), Index, #{}).

get_mounts() ->
  get_mtab_data(1).

% See also http://git.savannah.gnu.org/cgit/coreutils.git/tree/src/df.c
get_mount(Path) ->
  AbsPath = abspath(Path),
  try maps:get(AbsPath, get_mtab_data(1))
  catch error:bad_key ->
    case AbsPath of
      <<"/">> ->
        throw(no_mount);
      _ ->
        get_mount(filename:dirname(AbsPath))
    end
  end.

get_type(Path) ->
  AbsPath = abspath(Path),
  try maps:get(AbsPath, get_mtab_data(3))
  catch error:bad_key ->
    case AbsPath of
      <<"/">> ->
        throw("no_type");
      _ ->
        get_type(filename:dirname(AbsPath))
    end
  end.

get_options(Path) ->
  AbsPath = abspath(Path),
  try maps:get(AbsPath, get_mtab_data(4))
  catch error:bad_key ->
    case AbsPath of
      <<"/">> ->
        throw("no_options");
      _ ->
        get_options(filename:dirname(AbsPath))
    end
  end.

get_fs_freq(Path) ->
  AbsPath = abspath(Path),
  try maps:get(AbsPath, get_mtab_data(5))
  catch error:bad_key ->
    case AbsPath of
      <<"/">> ->
        throw("no_fs_freq");
      _ ->
        get_options(filename:dirname(AbsPath))
    end
  end.

get_fs_passno(Path) ->
  AbsPath = abspath(Path),
  try maps:get(AbsPath, get_mtab_data(6))
  catch error:bad_key ->
    case AbsPath of
      <<"/">> ->
        throw("no_fs_passno");
      _ ->
        get_options(filename:dirname(AbsPath))
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

parse_field([<<>> | Rest], Index, Map) ->
  parse_field(Rest, Index, Map);
parse_field([Blob | Rest], Index, Map) ->
  List = binary:split(Blob, [<<" ">>], [global]),
  Mount = lists:nth(2, List),
  Value = lists:nth(Index, List),
  parse_field(Rest, Index, maps:put(Mount, Value, Map));
parse_field([], _, Map) ->
  Map.
