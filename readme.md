# kv

kv is a notation for key-value pairs. This notation has following characteristics:

- no indentation-based syntax
- no implicit conversion
- no commas required when writing values

## Example
``` ento
table = {
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
    dependencies = [
      "https://github.com/veka41/ent"
      "https://github.com/hogehoge"
    ]
    connection-max = 5000
    real = 2.2322
    enabled = true
    foo = "bar" buz = "qux" -- technically correct, though not recommended
  }
}
```

## Syntax
``` text
entity  ::= pair*
pair    ::= key = value
key     ::= <symbol>
value   ::= <int> | <float> | <bool> | <string> | { entity } | [ value* ]
```

- encoding: utf-8
- newline: \n

## Semantics
`*.kv` is interpreted into a key-value map, as indicated in the following quasi-code:
``` haskell
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

type Map = HashMap Key MapValue

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
