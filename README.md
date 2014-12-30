# protoutils

Utility functions for working with protobuf.

protojson: convert json to protobuf and vice verca
protoeterm: convert arbitraty erlang terms to protobuf and vica versa

# protojson

Convert json to protobuf format and vice versa.

## Convenience functions

Calling `protojson:rawjsontoproto(JSON)` converts a json binary string
to a protobuf binary string.

Calling `protojson:rawprototojson(Proto)` reverses the operation.

## Other functions

`protojson:jsontoproto(MochiJson)` expects an erlang term as defined
by the mochijson2 json module. It will output structs that can
subsequently encoded using the `json_pb:encode()`
function. `protojson:prototojson(Proto)` expects a jsonvalue record
that is usually obtained by a call to `json_pb:decode_jsonvalue()` and
returns a mochijson2 compatible erlang term describing the json
object.

# protoeterm

`protoeterm:term_to_protobin(Term) :: binary()` and
`protoeterm:protobin_to_term(Bin) :: erlang_term()`

Convert arbitrary erlang terms to protobuf and vice versa.

Works with all erlang terms; however, if your erlang terms contain any
of the following, you will not be able to decode them with anything
but erlang:

* PIDs
* FUNs
* ports
* references (make_ref())

Note, term_to_binary encodes terms more efficiently than this module.
