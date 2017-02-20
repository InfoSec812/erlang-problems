-module(json).
-export([render/1]).

%% @doc Given a data structure consisting of (possibly nested) tuples/arrays/values, encode the data as a JSON string
render({Key, Value}) ->
  lists:flatten("{" ++ encode({Key, Value}) ++ "}");
render(Input) ->
  lists:flatten(encode(Input)).

-spec encode(Input :: term()) -> list().
%% @doc Given a data structure consisting of (possibly nested) tuples/arrays/values, encode the data as an array of
%% JSON strings
%%
%% @param The input term() which can be pretty much any Erlang type.
%%
%% @returns A <em>List</em> of Erlang terms which may contain strings, characters, and nested lists of the same
%%
%% @spec encode(Input::term()) -> list()
encode(true) ->
  "true";
encode(false) ->
  "false";
encode(null) ->
  "null";
encode(undefined) ->
  "null";
encode(B) when is_binary(B) ->
  encode_as_string(B);
encode(I) when is_integer(I) ->
  integer_to_list(I);
encode(F) when is_float(F) ->
  float_to_list(F);
encode(A) when is_atom(A) ->
  io_lib:format("\"~p\"", [A]);
encode({Key, Value}) ->
  encode(Key) ++ ":" ++ encode(Value);
encode({L}) when is_list(L) ->
  "{" ++ encode_array(L) ++ "}";
encode(L) when is_list(L) ->
  case misc_supp:is_string(L) of
    true  -> encode_as_string(L);
    false -> "[" ++ encode_array(L) ++ "]"
  end;
encode(Value) ->
  io:format("~p", [Value]),
  {error, "Invalid Data", Value}.

%% @doc Given an array of elements, encode those elements and return them as a comma-separated list of strings
encode_array(Array) ->
  lists:join(",", lists:map(fun(Element) -> encode(Element) end, Array)).

%% @doc Given a list of "characters", encode it as a quoted string
encode_as_string(B) ->
  case is_binary(B) of
    true  ->
      lists:concat(["\"", binary_to_list(B), "\""]);
    false ->
      lists:concat(["\"", B, "\""])
  end.
