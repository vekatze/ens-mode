# xt dictionary notation

- no indentation-based syntax
- no implicit conversion
- no commas required when writing values

## Example
``` chart
title = "foo"
time = "12341234" -- comment
empty = {}
database = {
  server = "192.168.11.1"
  items = [ true false false ]
  ports = [
    8001
    8002
    { foo = "aoeu" } -- heterogeneous
    8003
  ]
  empty-list = []
  list-of-charts = [
    {
      foo = "some"
      hoge = true    -- bool
      pohe = "true"  -- string
    }
    {
      pohe = "lorem ipsum"
      aoeu = [ 10 20 30 40 ]
    }
  ]
  -- some comment.
  dependencies = [
    "https://github.com/veka41/chart"
    "https://github.com/hogehoge"
  ]
  connection-max = 5000
  real = 2.2322
  enabled = true
}
```

## Syntax
``` text
chart   ::= pair#
pair    ::= key = value
key     ::= <symbol>
value   ::= <int> | <float> | <bool> | <string> | { chart } | [ value# ]
```

## Semantics
A chart is interpreted into a hashmap, as indicated in the following quasi-code:
``` haskell
type Chart = [Pair]

type Pair = (Key, Value)

type Key = String

data Value
  = ValueInt Int
  | ValueFloat Double
  | ValueBool Bool
  | ValueString String
  | ValueChart Chart
  | ValueList [Value]

type Map = HashMap Key MapValue

data MapValue
 = MapValueInt Int
 | MapValueFloat Double
 | MapValueBool Bool
 | MapValueString String
 | MapValueMap Map
 | MapValueList [MapValue]

evalChart :: Chart -> Map
evalChart chart =
  evalChart' chart emptyMap

evalChart' :: Chart -> Map -> Map
evalChart' chart acc =
  case chart of
    [] ->
      acc
    (key, value) : pairList ->
      evalChart' (insert key (evalValue value) acc) pairList

evalValue :: Value -> MapValue
evalValue value =
  case value of
    ValueInt x ->
      MapValueInt x
    ValueFloat x ->
      MapValueFloat x
    ValueBool x ->
      MapValueBool x
    ValueString x ->
      MapValueString x
    ValueChart x ->
      MapValueMap (evalChart x)
    ValueList xs ->
      MapValueList (map evalValue xs)
```