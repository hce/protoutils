%%% @author Hans-Christian Esperer <hc@hcesperer.org>
%%% @copyright (C) 2014, Hans-Christian Esperer
%%% @doc
%%% Encode/decode json to protobuf
%%%
%%% @end
%%% Created : 30 Dec 2014 by Hans-Christian Esperer <hc@hcesperer.org>

-module(protojson).

-export([jsontoproto/1, prototojson/1]).

-include("json_pb.hrl").

prototojson(#jsonvalue{type='STRING', stringval=S}) ->
    S;
prototojson(#jsonvalue{type='INTEGER', intval=I}) ->
    I;
prototojson(#jsonvalue{type='DOUBLE', doubleval=D}) ->
    D;
prototojson(#jsonvalue{type='BOOLEAN', boolval=B}) ->
    B;
prototojson(#jsonvalue{type='UNDEFINED'}) ->
    undefined;
prototojson(#jsonvalue{type='NULL'}) ->
    null;
prototojson(#jsonvalue{type='ARRAY', arrayval=#jsonarray{values=A}}) ->
    lists:map(fun prototojson/1, A);
prototojson(#jsonvalue{type='OBJECT',objectval=#jsonobject{keys=K, values=V}}) ->
    F = fun({Key, Val}, A) ->
		K_D = prototojson(Key),
		V_D = prototojson(Val),
		[{K_D, V_D}|A]
	end,
    LoP = lists:reverse(lists:foldl(F, [], lists:zip(K, V))),
    {struct, LoP}.

jsontoproto({struct, KeyValueList}) ->
    F = fun({K, V}, {KL, VL}) ->
		KL2 = [jsontoproto(K)|KL],
		VL2 = [jsontoproto(V)|VL],
		{KL2, VL2}
	end,
    {Keys_R, Values_R} = lists:foldl(F, {[], []}, KeyValueList),
    Keys = lists:reverse(Keys_R),
    Values = lists:reverse(Values_R),
    Obj = #jsonobject{
	     keys=Keys,
	     values=Values},
    #jsonvalue {
       type='OBJECT',
       objectval=Obj};

jsontoproto(L) when is_list(L) ->
    Values = lists:map(fun jsontoproto/1, L),
    Obj = #jsonarray{values=Values},
    #jsonvalue {
       type='ARRAY',
       arrayval=Obj};

jsontoproto(S) when is_binary(S) ->
    #jsonvalue {
       type='STRING',
       stringval=S};

jsontoproto(I) when is_integer(I) ->
    #jsonvalue {
       type='INTEGER',
       intval=I};

jsontoproto(D) when is_float(D) ->
    #jsonvalue {
       type='DOUBLE',
       doubleval=D};

jsontoproto(B) when is_boolean(B) ->
    #jsonvalue {
       type='BOOLEAN',
       boolval=B};

jsontoproto(undefined) ->
    #jsonvalue { type='UNDEFINED' };

jsontoproto(null) ->
    #jsonvalue { type='NULL' }.

       





