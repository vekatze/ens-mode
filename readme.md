# kv

kv is a key-value notation with following properties:

- no indentation-based syntax
- no implicit conversion
- no commas required when writing values

## Example
``` kv
title = "foo"
time = "12341234" -- comment
binary-integer = 0b10101111
octal-integer = 0o81234
hex-integer = 0xdeadbeef
empty = {}
database = {
  server = "192.168.11.1"
  name = "install system packages"
  run = [
    "apt-get update"
    "apt-get install -y cron foo bar"
    "apt-get install hogehoge"
    "rm -rf /var/lib/apt/lists/*"
  ]
  items = [ true false false ]
  ports = [
    8001
    8002
    { foo = "aoeu" } -- heterogeneous
    8003
  ]
  empty-list = []
  list-of-entities = [
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
  connection-max = 5000
  real = 2.2322
}
```

## Syntax
``` text
key     ::= <symbol>
value   ::= <int> | <float> | <bool> | <string> | { (key = value)* } | [ value* ]
```

- encoding: utf-8
- newline: \n

## Semantics
`*.kv` is interpreted into a key-value map, as indicated in the following quasi-code:
``` haskell
type Map = HashMap Key MapValue

type Entity = [Pair]

type Pair = (Key, Value)

type Key = String

data Value
  = ValueInt Int
  | ValueFloat Double
  | ValueBool Bool
  | ValueString String
  | ValueEntity Entity
  | ValueList [Value]

data MapValue
 = MapValueInt Int
 | MapValueFloat Double
 | MapValueBool Bool
 | MapValueString String
 | MapValueMap Map
 | MapValueList [MapValue]

evalEntity :: Entity -> Map
evalEntity entity =
  evalEntity' entity emptyMap

evalEntity' :: Entity -> Map -> Map
evalEntity' entity acc =
  case entity of
    [] ->
      acc
    (key, value) : pairList ->
      evalEntity' (insert key (evalValue value) acc) pairList

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
    ValueEntity x ->
      MapValueMap (evalEntity x)
    ValueList xs ->
      MapValueList (map evalValue xs)
```
