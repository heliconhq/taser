%% Copyright (c) 2016, Gustaf Sjöberg <gs@trell.se>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(taser_utils).

-export([parse_uri/1]).
-export([absolute_location/5]).

-ifdef(TEST).
-export([tokenize_querystring/1]).
-export([urlencode_pairs/1]).
-endif.

parse_uri(URI) ->
    case http_uri:parse(URI) of
        {error, no_scheme} ->
            parse_uri("http://" ++ URI);
        {ok, {Protocol, Auth, Hostname, Port , Path, Query}} ->
            {ok, {Protocol, Auth, Hostname, Port, Path, Query,
                  Path ++ escape_querystring(Query)}};
        Error ->
            Error
    end.

escape_querystring("") ->
    "";
escape_querystring("?" ++ Query) ->
    "?" ++ urlencode_pairs(tokenize_querystring(Query)).

%% @doc Split a querystring into a list of key-value tuples.
tokenize_querystring(Query) ->
    Tokens = string:tokens(Query, "&"),
    Pairs = [string:tokens(Token, "=") || Token <- Tokens],
    [case Pair of
         [Key, Value] -> {Key, Value};
         [Value] -> {Value, ""};
         [] -> ""
     end || Pair <- Pairs].

%% @doc Process a list of key-value tuples, urlencode all keys and values and
%% separate them with an equal sign (=), separate all pairs with an ampersand
%% (&) and return a string.
urlencode_pairs(Tokens) ->
    lists:flatten(lists:join("&", [urlencode(Key) ++ "=" ++ urlencode(Value)
                                   || {Key, Value} <- Tokens])).

%% @doc Coerce value to a string and urlencode.
urlencode(S) when is_atom(S) ->
    urlencode(atom_to_list(S));
urlencode(S) when is_binary(S) ->
    urlencode(binary_to_list(S));
urlencode(S) when is_integer(S) ->
    urlencode(integer_to_list(S));
urlencode(S) when is_float(S) ->
    urlencode(float_to_list(S));
urlencode(S) ->
    edoc_lib:escape_uri(S).

%% TODO: Preserve auth details during redirects?
absolute_location(<<"http://", _/binary>> = Location, _Protocol, _Hostname,
                  _Port, _Path) ->
    Location;
absolute_location(<<"https://", _/binary>> = Location, _Protocol, _Hostname,
                  _Port, _Path) ->
    Location;
absolute_location(<<"/", _/binary>> = Location, Protocol,Hostname, Port,
                  _Path) ->
    atom_to_list(Protocol) ++ "://" ++ Hostname ++ ":" ++
        integer_to_list(Port) ++ binary_to_list(Location);
absolute_location(Location, Protocol, Hostname, Port, Path) ->
    %% We're assuming a location relative to the current path.
    atom_to_list(Protocol) ++ "://" ++ Hostname ++ ":" ++
        integer_to_list(Port) ++ Path ++ binary_to_list(Location).
