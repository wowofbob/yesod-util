# yesod-util

Utility functions for `yesod`.

Most of the functions are `ExceptT` transformers. There is `answerToRequest`, which takes a handler wrapped into `ExceptT` and returns json value. But you can use your own runner for `ExceptT`. The idea is to use one json object to wrap data returned by server. For this purpose, the `Answer` data type is presented .

## Example

Return name of logged in user:

```haskell
getUserNameR :: Handler Value
getUserNameR = answerToRequest $
  withAuthEntity $ pure . userName
```

It produces three possible json objects:

  * When user is not logged in
    ```JSON
    { "error"   : true
    , "message" : "authentication is required"
    , "data"    : null
    }
    ```
  * When user record is missing (not found in database)
    ```JSON
    { "error"   : true
    , "message" : "authentication failure"
    , "data"    : null
    }
    ```
  * When user is logged in
    ```JSON
    { "error"   : false
    , "message" : "OK"
    , "data"    : "user-name-here"
    }
    ```

Default error message may be changed using `withExceptT`:

```haskell
getUserNameR :: Handler Value
getUserNameR = answerToRequest $
  withExceptT (const "user is not logged in") $
    withAuthEntity $ pure . userName
```

If user is not logged in, the server will return

```JSON
{ "error"   : true
, "message" : "user is not logged in"
, "data"    : null
}
```
