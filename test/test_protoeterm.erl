%%% @author Hans-Christian Esperer <hc@operations.hcesperer.org>
%%% @copyright (C) 2014, Hans-Christian Esperer
%%% @doc
%%%
%%% @end
%%% Created : 30 Dec 2014 by Hans-Christian Esperer <hc@operations.hcesperer.org>

-module(test_protoeterm).

-include_lib("eunit/include/eunit.hrl").

test(Term) ->
    Encoded = protoeterm:term_to_protobin(Term),
    Encoded_Flattened = iolist_to_binary(Encoded),
    Decoded = protoeterm:protobin_to_term(Encoded_Flattened),
    io:format("Original: ~p~nDecoded: ~p~n", [Term, Decoded]),
    ?assertEqual(Term, Decoded).

atom_test() ->
    test(abc),
    test(true),
    test(false),
    test(undefined),
    test(hervorragend).

list_test() ->
    test("Hello World"),
    test([1, 2, 3, 4]),
    test([1]),
    test([]).

tuple_test() ->
    test({1}),
    test({1, 2}),
    test({1, 2, 3}),
    test({}).

bitstring_test() ->
    test(<< 1:1 >>),
    test(<< 1:2 >>),
    test(<< 1:3 >>),
    test(<< 1:4 >>),
    test(<< 1:5 >>),
    test(<< 1:6 >>),
    test(<< 1:7 >>),
    test(<< 1:8 >>),
    test(<< 1:9 >>),
    test(<< 1:10 >>),
    test(<< 1:11 >>),
    test(<< 1:12 >>),
    test(<< "Hello World" >>),
    test(<< >>).

complicated_test() ->    
    test({1, 2, foo, atom, <<"binary">>, <<"bitstring", 4:4>>, [1, 2, 3, 4, foo, bar]}).

pid_test() ->
    test(self()),
    test({self(), self(), spawn(fun() ->
					receive after 500 ->
							ok end end), self()}).

ref_test() ->
    test(make_ref()),
    test([make_ref(), make_ref(), {make_ref(), make_ref()}]).

fun_test() ->
    test(fun() ->
		 io:format("fun1")
	 end),
    Var = "abc",
    F2 = fun() ->
		 io:format("~s~n", [Var])
	 end,
    test(F2).
    
empty_dict_test() ->
    test(dict:new()).

simple_orddict_test() ->
    L = [{Key, now()} || Key <- lists:seq(1, 1000)],
    D = orddict:from_list(L),
    test(D).

simple_dict_test() ->
    L = [{Key, now()} || Key <- lists:seq(1, 1000)],
    D = dict:from_list(L),
    test(D).

complex_dict_test() ->
    D = dict:from_list([{a, b}, {b, a}, {d, c},
			{c, d}, {e, f}, {g, <<"h">>},
			{<<"i">>, j}, {"klm", dict:new()},
			{"nop", dict:store(a, b, dict:store(d, e, dict:new()))}]),
    test(D).
    
