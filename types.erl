-module(types).
-export([name/1]).

name(Value) ->
  case is_atom(Value) of
    true ->
      atom;
    _ ->
      case is_binary(Value) of
        true ->
          binary;
        _ ->
          case is_bitstring(Value) of
            true ->
              bitstring;
            _ ->
              case is_boolean(Value) of
                true ->
                  boolean;
                _ ->
                  case is_float(Value) of
                    true ->
                      float;
                    _ ->
                      case is_function(Value) of
                        true ->
                          function;
                        _ ->
                          case is_integer(Value) of
                            true ->
                              integer;
                            _ ->
                              case is_list(Value) of
                                true ->
                                  list;
                                _ ->
                                  case is_map(Value) of
                                    true ->
                                      map;
                                    _ ->
                                      case is_number(Value) of
                                        true ->
                                          number;
                                        _ ->
                                          case is_pid(Value) of
                                            true ->
                                              pid;
                                            _ ->
                                              case is_port(Value) of
                                                true ->
                                                  port;
                                                _ ->
                                                case is_reference(Value) of
                                                  true ->
                                                    reference;
                                                  _ ->
                                                    case is_tuple(Value) of
                                                      true ->
                                                        tuple;
                                                      _ ->
                                                        unrecognized
                                                      end
                                                  end
                                              end
                                          end
                                      end
                                  end
                              end
                          end
                      end
                  end
              end
          end
      end
  end.
