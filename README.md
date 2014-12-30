# protoutils

Utility functions for working with protobuf.

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

