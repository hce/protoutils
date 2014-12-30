%%% @author Hans-Christian Esperer <hc@hcesperer.org>
%%% @copyright (C) 2014, Hans-Christian Esperer
%%% @doc
%%% Encode/decode erlang terms to/from protobuf
%%%
%%% @end
%%% Created : 30 Dec 2014 by Hans-Christian Esperer <hc@hcesperer.org>

-module(protoeterm).

-include("eterm_pb.hrl").

-export([term_to_protobin/1, protobin_to_term/1]).

-export([term_to_proto/1, proto_to_term/1]).

term_to_protobin(Term) ->
    eterm_pb:encode(term_to_proto(Term)).

protobin_to_term(Protobuf) ->
    proto_to_term(eterm_pb:decode_erlangvalue(Protobuf)).

term_to_proto(V) when is_atom(V) ->
    #erlangvalue{
		  type='ATOM',
		  stringval=atom_to_binary(V, utf8)
		};

term_to_proto(V) when is_binary(V)->
    #erlangvalue{
		  type='BINARY',
		  stringval=V
		};

term_to_proto(V) when is_bitstring(V) ->
    Size = bit_size(V),
    Padding = 8 - (Size rem 8) rem 8,
    Bytestring = << V/bitstring, 0:Padding >>,
    #erlangvalue{
		  type='BITSTRING',
		  stringval=Bytestring,
		  intval=Size
		};

term_to_proto(V) when is_float(V) ->
    #erlangvalue{
		  type='FLOAT',
		  doubleval=V
		};

term_to_proto(V) when is_integer(V) ->
    #erlangvalue{
		  type='INTEGER',
		  intval=V
		};

term_to_proto(V) when is_list(V) ->
    Val = lists:map(fun term_to_proto/1, V),
    #erlangvalue{
		  type='LIST',
		  listval=#erlanglist{ values=Val }
		};

term_to_proto(V) when is_tuple(V) ->
    Val = lists:map(fun term_to_proto/1, tuple_to_list(V)),
    #erlangvalue{
		  type='TUPLE',
		  listval=#erlanglist{ values=Val }
		};

%% catchall case
term_to_proto(V) ->
    Val = term_to_binary(V),
    #erlangvalue{
		  type='ERLTERM',
		  stringval=Val
		}.

proto_to_term(#erlangvalue{type='ATOM', stringval=A}) ->
    binary_to_existing_atom(A, utf8);
proto_to_term(#erlangvalue{type='BINARY', stringval=B}) ->
    B;
proto_to_term(#erlangvalue{type='BITSTRING', intval=L, stringval=B}) ->
    << Bitstring:L/bitstring, _Padding/bitstring >> = B,
    Bitstring;
proto_to_term(#erlangvalue{type='FLOAT', doubleval=D}) ->
    D;
proto_to_term(#erlangvalue{type='INTEGER', intval=I}) ->
    I;
proto_to_term(#erlangvalue{type='LIST', listval=#erlanglist{values=L}}) ->
    lists:map(fun proto_to_term/1, L);
proto_to_term(#erlangvalue{type='TUPLE', listval=#erlanglist{values=L}}) ->
    list_to_tuple(lists:map(fun proto_to_term/1, L));
proto_to_term(#erlangvalue{type='ERLTERM', stringval=T}) ->
    binary_to_term(T, [safe]). 




		  
