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

-export([term_to_proto/1, proto_to_term/2]).

term_to_protobin(Term) ->
    {Structure, Atomlist} = term_to_proto(Term),
    eterm_pb:encode(#erlangterm{
		       atomlist=Atomlist,
		       value=Structure}).
    
protobin_to_term(Protobuf) ->
    #erlangterm{atomlist=Atomlist,
		value=Value} = eterm_pb:decode_erlangterm(Protobuf),
    proto_to_term(Value, Atomlist).

term_to_proto(V) ->
    {Structure, Atomtable} = i_term_to_proto(V, dict:new()),
    Atomtable_O = dict:fold(fun(K, V_, A) ->
				    orddict:store(V_, K, A)
			    end, orddict:new(), Atomtable),
    Atomtable1 = #erlangatomlist{
      atomname = [atom_to_binary(Atomname, utf8) ||
		     {_Atomnumber, Atomname} <- orddict:to_list(Atomtable_O)]
     },
    {Structure, Atomtable1}.    

i_term_to_proto(V, A) when is_atom(V) ->
    {Atomtable1, AN} = case dict:find(V, A) of
			   {ok, Atomnumber} ->
			       {A, Atomnumber};
			   error ->
			       %% This has a constant runtime in erlang :-)
			       Atomnumber = dict:size(A),
			       {dict:store(V, Atomnumber, A), Atomnumber}
		       end,
    {#erlangvalue{
	type='ATOM',
	shortval=AN
       }, Atomtable1};

i_term_to_proto(V, A) when is_binary(V)->
    {#erlangvalue{
	type='BINARY',
	stringval=V
       }, A};

i_term_to_proto(V, A) when is_bitstring(V) ->
    Size = bit_size(V),
    Padding = 8 - (Size rem 8) rem 8,
    Bytestring = << V/bitstring, 0:Padding >>,
    {#erlangvalue{
	type='BITSTRING',
	stringval=Bytestring,
	shortval=Size
       }, A};

i_term_to_proto(V, A) when is_float(V) ->
    {#erlangvalue{
	type='FLOAT',
	doubleval=V
       }, A};

i_term_to_proto(V, A) when is_integer(V) ->
    {#erlangvalue{
	type='INTEGER',
	intval=V
       }, A};

i_term_to_proto(V, A) when is_list(V) ->
    {Val, Atomtable}  = lists:foldl(fun(E, {Values, Atomtable0}) ->
					    {Val1, Atomtable1} =
						i_term_to_proto(E, Atomtable0),
					    {[Val1|Values], Atomtable1}
				    end, {[], A}, V),
    {#erlangvalue{
	type='LIST',
	listval=#erlanglist{ values=lists:reverse(Val) }
       }, Atomtable};

i_term_to_proto(V, A) when is_tuple(V) ->
    {Val, Atomtable}  = lists:foldl(fun(E, {Values, Atomtable0}) ->
					    {Val1, Atomtable1} =
						i_term_to_proto(E, Atomtable0),
					    {[Val1|Values], Atomtable1}
				    end, {[], A}, tuple_to_list(V)),
    {#erlangvalue{
	type='TUPLE',
	listval=#erlanglist{ values=lists:reverse(Val) }
       }, Atomtable};

%% catchall case
i_term_to_proto(V, A) ->
    Val = term_to_binary(V),
    {#erlangvalue{
		  type='ERLTERM',
		  stringval=Val
       }, A}.

proto_to_term(Structure, #erlangatomlist{atomname=Atomlist}) ->
    Atomtable = lists:foldl(fun(E, A) ->
				    dict:store(dict:size(A), E, A)
			    end, dict:new(), Atomlist),
    i_proto_to_term(Structure, Atomtable).

i_proto_to_term(#erlangvalue{type='ATOM', shortval=Atomnumber}, Atomtable) ->
    {ok, Atomname} = dict:find(Atomnumber, Atomtable),
    binary_to_existing_atom(Atomname, utf8);
i_proto_to_term(#erlangvalue{type='BINARY', stringval=B}, _Atomtable) ->
    B;
i_proto_to_term(#erlangvalue{type='BITSTRING', shortval=L, stringval=B}, _Atomtable) ->
    << Bitstring:L/bitstring, _Padding/bitstring >> = B,
    Bitstring;
i_proto_to_term(#erlangvalue{type='FLOAT', doubleval=D}, _Atomtable) ->
    D;
i_proto_to_term(#erlangvalue{type='INTEGER', intval=I}, _Atomtable) ->
    I;
i_proto_to_term(#erlangvalue{type='LIST', listval=#erlanglist{values=L}}, Atomtable) ->
    lists:map(fun(E) -> i_proto_to_term(E, Atomtable) end, L);
i_proto_to_term(#erlangvalue{type='TUPLE', listval=#erlanglist{values=L}}, Atomtable) ->
    list_to_tuple(lists:map(fun(E) -> i_proto_to_term(E, Atomtable) end, L));
i_proto_to_term(#erlangvalue{type='ERLTERM', stringval=T}, _Atomtable) ->
    binary_to_term(T, [safe]). 




		  
