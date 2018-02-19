# yesod-except

Lift functions from `Maybe` to `MonadError`. On error, either set message in session (`setMessage`) or return back json object to user (`returnJson`).

## Short example.

Suppose you want to write a handler which creates a note on server. The note might be saved in file system or database. Our handler will expect a json object as an argument of form:

```json
{ name     : "name-of-note"
, comment  : "optional-comment-for-note"
, contents : "contents-of-note"
}
```

To deal with this, one can define a `Note` data type and provide corresponding
`FromJSON` instance:

```haskell
data Note = Note
  { noteName     :: Text
  , noteComment  :: Maybe Text
  , noteContents :: Text
  }

instance FromJSON Note where
  parseJSON (Object obj) =
    Note <*> obj .: "name" <$> obj .: "comment" <$> obj .: "contents"
  parseJSON invalid = typeMismatch "Note" invalid
```

But it takes time, and if later you'll decide to change handler's API, you'll have to change this data type too. I don't like it. So, I would write this handler like that:

```haskell
-- | Handler for creation of notes.
postCreateNoteR :: Handler Value
postCreateNoteR =
  
  -- Wrap handler into 'MonadError' instance which returns json ('returnJson') back to user.
  runExceptV .
  
    -- Require json object in request body.  
    withJsonObject $ do
    
      -- Get "name" field from json object.
      noteName <- askValue "name"

      -- Get "comment" field.
      noteComment <- askValue "comment"

      -- Get "contents" field.
      noteContents <- askValue "contents"

      -- Save to file system or database.
      lift . saveNote $
        noteName noteComment noteContents
```

If everything is OK, then user gets back object

```json
{ data    : []
, error   : false
, message : "OK"
}
```

If, for instance, there is no key `"name"` in json object, or if it references to wrong type, then user gets back object 

```json
{ data    : null
, error   : true
, message : "cannot parse 'Text' associated with key 'name'"
}
```

One can change the behaviour of this handler simply by replacing `ExceptV` by `ExceptM`. In this
case, user won't get back a json object. Instead, an error will be set in user session. This must
cover *post/redirect/get* usage case. For example:

```haskell
-- | Handler for creation of notes. On error, set message in session.
postCreateNoteR :: Handler ()
postCreateNoteR =

  -- Now object won'be returned. Instead, a message will be set on error.
  -- Also, not that handler's type changed too.
  runExceptM .
  
    -- Require json object in request body.
    withJsonObject $ do
    
      -- Get "name" field from json object.
      noteName <- askValue "name"

      -- Get "comment" field.
      noteComment <- askValue "comment"

      -- Get "contents" field.
      noteContents <- askValue "contents"

      -- Save to file system or database.
      lift . saveNote $
        noteName noteComment noteContents
```

It lets you not to worry much about json API because you do not declare it explicitly.

As a downside, most messages are fine for debugging, but user might not want to see them.

# More examples.

There is another example in `app`. It is a subsite which can create, delete, read and write files. Files are being saved on server in `_junk/file`. Type `stack exec app` to launch it. Use scripts from `curl/File` to play with it.

I should note that top-level functions which can be called in subsite handler have too complex type if they require json object as environment. This is implied by using `ReaderT`. I used to have special wrapper around `ReaderT` which did automatic lifting. It was nice when I had just `ExceptV` like wrapper. But now, when there is a way to set a message on error, such `ReaderT` wrapper must deal both with `ExceptV` and `ExceptM`. And I'm not sure how to do it right. So I just made it simple.
