# yesod-except

This library provides set of functions for dealing with json and database.
Basically, each functions corresponds to it's out-of-the-box analogue, but
lifted to `MonadError`. 

Anyway, writing class-dependent functions is great, but at some point they must
be executed. In `Wrappers` module one can find a three wrappers around `ExceptT`
which differs by their side effects.

Here is short example. Suppose you want to write a handler which creates a note
on server. The note might be saved in file system or database, but it's not the
point. Our handler will expect a json object as an argument of form:

```json
{ name     : "name-of-note"
, comment  : "optional-comment-for-note"
, contents : "contents-of-note"
}
```
To deal with this, one can define a `Note` data type and provide corresponding
`FromJSON` instance. But it takes time, and if later you'll decide to change
API, you'll have to change this data type too. I don't like it. So, I would
write this handler like that:

```haskell
-- | Handler for creation of notes.
postCreateNoteR :: Handler Value
postCreateNoteR =

  -- Explicitly say that there must be a json object provided.
  -- It doesn't matter what fields it contains.  
  withObjEnv
  
    -- Explicitly say how to report back about error to user.
    ExceptV $ do
  
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
postCreateNoteR = withObjEnv ExceptM $ do

  -- Explicitly say that there must be a json object provided.
  -- It doesn't matter what fields it contains.  
  withObjEnv
  
    -- Now object won'be returned. Instead, a message will be set on error.
    -- Also, not that the result type of handler changed too.
    ExceptM $ do
  
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

As a downside, most messages are fine for debugging, but user might not want to see them.

# Example

There is an example in `app`. It is a subsite which can create, delete, read and write files. Files are being saved on server in `_junk/file`.
