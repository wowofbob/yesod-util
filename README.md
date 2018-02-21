# yesod-except

`yesod-except` is a haskell library which aims to help with error handling in `yesod`. 

## Example

> You can find complete example in [example/math/Main.hs](https://github.com/wowofbob/yesod-except/blob/master/example/math/Main.hs). Run it with `stack exec math`, use [example/math.sh](https://github.com/wowofbob/yesod-except/blob/master/example/math.sh) to play.

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

Our handler returns back to user a json object with result data, error flag and error message (`runExceptV`). It takes a json object as an argument (`withJsonObject`). This object must have `"type"`, `"left"`, and `"right"`keys (`askValue`). The `"type"` key must point to either `"plus"` or `"minus"` strings (`FromJSON` definition for `OpType`); both `"left"` and `"right"` keys must point to integers (`FromJSON` definition for `Number`). There might be other keys in json object, but, according to definition of our handler, these three must be present and be valid. Otherwise request will be rejected.

A user application gets back one of the objects listed below depending from errors:

---
##### No Errors
```bash
curl -w "\n" -i -H "Accept: application/json" -X POST -d '{"type":"minus","left":1,"right":1}' 'http://localhost:3000'
```
```javascript
{ data    : 0
, error   : false
, message : "OK"
}
```
##### Invalid json.
```bash
curl -w "\n" -i -H "Accept: application/json" -X POST -d '{{{{{' 'http://localhost:3000'
```
```javascript
{ data    : null
, error   : true
, message : "ParseError {errorContexts = [\"34\"], errorMessage = \"Failed reading: satisfy\", errorPosition = 1:2}"
}
```
---
##### No json object in request body.
```bash
curl -w "\n" -i -H "Accept: application/json" -X POST -d false 'http://localhost:3000'
```
```javascript
{ data    : null
, error   : true
, message : "json object expected"
}
```
---
##### Key is not present.
```bash
curl -w "\n" -i -H "Accept: application/json" -X POST -d '{"type":"minus","right":1}' 'http://localhost:3000'
```
```javascript
{ data    : null
, error   : true
, message : "cannot parse 'Number' associated with key 'left': key \"left\" not present"
}
```
---
##### Invalid "type"
```bash
curl -w "\n" -i -H "Accept: application/json" -X POST -d '{"type":"multiply","left":1,"right":1}' 'http://localhost:3000'
```
```javascript
{ data    : null
, error   : true
, message : "cannot parse 'OpType' associated with key 'type': unknown operation"
}
```
---
##### Invalid "left"
```bash
curl -w "\n" -i -H "Accept: application/json" -X POST -d '{"type":"minus","left":"123","right":1}' 'http://localhost:3000'
```
```javascript
{ data    : null
, error   : true
, message : "cannot parse 'Number' associated with key 'left': expected Int, encountered String"
}
```
---
##### Invalid "right"
```bash
curl -w "\n" -i -H "Accept: application/json" -X POST -d '{"type":"minus","left":1,"right":true}' 'http://localhost:3000'
```
```javascript
{ data    : null
, error   : true
, message : "cannot parse 'Number' associated with key 'right': expected Int, encountered Boolean"
}
```

## How it works.

Most of the code for dealing with json was lifted to `MonadError`. For instance, instead of writing

```haskell
someHandler = do
  json <- parseJsonBody
  case json of
    Success body -> onSuccess ...
    Error   err  -> onError   ...
```
you can do

```haskell
someHandler = runner $ do
  json <- parseJsonBody_
  ...
```

`runner` is a something which deals with error at the end. The behaviour is defined by a monad being used.

There are some special monads in `Yesod.Except.Wrappers` which you can use out of the box. But you can also use yours. This is possible because each function in `Yesod.Except.Json` and `Yesod.Except.Persist` depends from `mtl` style type classes only (with a little exception to `withJsonObject`). For example, lets look at the type of `parseJsonBody_`:

```haskell
parseJsonBody_
  :: forall m a .
       (MonadError Text m, MonadHandler m, FromJSON a, Typeable a) => m a
```
You are completely free to use it in any `MonadError Text` and `MonadHander` monad.

### ExceptV

`ExceptV` is a wrapper around `ExceptT` with a side effect of returning back to user application a json object of following format:

```javascript
{ data    : null or 'toJSON' on result.
, error   : true or false
, message : "OK" if 'error' is false; otherwise error message
}
```

Use `runExceptV` to run `ExceptV`.

### ExceptM

`ExceptM` is wrapper around `ExceptT` with a side effect of setting up a message in user session on error.

Use `runExceptM` to run `ExceptM`.

## Conclusion.

Using error monad to deal with errors lets you to establish how the user application gets notified about error. You get less headache because you know that your API support some strict policy for error handling. You can change it by using your own monad or by handling errors by hands in special cases. Error messages are not intended for a user of application. They show the developer what happened. That's why I can't come up with a good example for `ExceptM`: usually a message gets rendered automatically. Most of my code is JSON services so I use `ExceptV`. Anyway, one can use `ExceptM` if message is being used by client application and not being rendered automatically.
