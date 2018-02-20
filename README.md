# yesod-except

`yesod-except` is a haskell library which aims to help with error handling in `yesod`. 

# Example

> You can find complete example in [example/math/Main.hs](https://github.com/wowofbob/yesod-except/blob/master/example/math/Main.hs). Run it with `stack exec math`, use [curl/math.sh](https://github.com/wowofbob/yesod-except/blob/master/curl/math.sh) to play.

Suppose you want to create a JSON web service: it takes a JSON request and returns a JSON response. Our service will handle math operations: plus and minus.

At first, we must make some declarations to help themself:

```haskell
-- | Type of operation to handle.
data OpType = OpTypePlus | OpTypeMinus

-- | How to parse 'OpType'.
instance FromJSON OpType where
  parseJSON (String str) =
    case str of
      "plus"  -> pure OpTypePlus
      "minus" -> pure OpTypeMinus
      _       -> fail "unknown operation"
  parseJSON invalid =
    typeMismatch "OpType" invalid

-- | Wrapper for a number.
newtype Number = Number { toInt :: Int }

-- | How to parse 'Number'.
instance FromJSON Number where
  parseJSON = fmap Number . parseJSON

-- | How to serialize 'Number'.
instance ToJSON Number where
  toJSON = toJSON . toInt
```

Next, lets make a handler for this service:

```haskell
postMathServiceR :: Handler Value
postMathServiceR =
  -- Wrap handler's body into `MonadError` instance which
  -- returns a json object after evaluation. This object is
  -- constructed automatically.
  runExceptV .
    -- Require json object to be present in request body.
    withJsonObject $ do
      -- Get type of opearation to handle.
      opType <- askValue "type"
      -- Get first argument.
      nmLeft <- toInt <$> askValue "left"
      -- Get second argument. 
      nmRight <- toInt <$> askValue "right"
      -- Return result.
      pure . Number $
        case opType of
          OpTypePlus  -> nmLeft + nmRight
          OpTypeMinus -> nmLeft - nmRight
```

A user application gets back one of the objects listed below depending on errors:

---
##### No Errors
```javascript
{ data    : <integer-number>
, error   : false
, message : "OK"
}
```
---
##### No json object in request body.

```javascript
{ data    : null
, error   : true
, message : "json object expected"
}
```
---
##### Invalid "type"

```javascript
{ data    : null
, error   : true
, message : "cannot parse 'OpType' associated with key 'type': unknown operation"
}
```
---
##### Invalid "left"

```javascript
{ data    : null
, error   : true
, message : "cannot parse 'Number' associated with key 'left': expected Int, encountered String"
}
```
---
##### Invalid "right"

```javascript
{ data    : null
, error   : true
, message : "cannot parse 'Number' associated with key 'right': Int is either floating or will cause over or underflow: 201.9"
}
```
