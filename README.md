# yesod-except

Lift functions returning `Maybe` to `ExceptT`. Use `Text` as an error type. On error, return information about what was expected using `Data.Typeable`. This information is useful for debugging. I'm not sure that user may want to see it in message after redirect. Anyway, running wrappers yields `Value` which then can be returned. There is no mechanics for setting a message yet.

I find it very useful for json parsing. Usually, one wrties a `newtype` or `data` and adds `FromJSON` instances. Sometimes, especially when you are experimenting, you are not so sure about which names to use for keys, or which data to except from a client. Each change to API requires changing underlying `newtype` or `data` used for json parsing. And I realy don't like it. So, now I can write a handlers like:

```haskell
-- | Handler for creation of notes.
postCreateNoteR :: Handler Value
postCreateNoteR = withJsonObjEnv $ do
  
  -- How to call a new note.
  noteName <- askValue "name"
  
  -- Short comment about a note.
  noteComment <- askValue "comment"
  
  -- Contents of this note.
  noteContents <- askValue "contents"
  
  -- Save to file system or database.
  lift . saveNote $
    Note noteName noteComment noteContents
```

This way, if I'll find that comment is a somthing I don't need, I can just do a minor refactoring. But this is a silly example.

Anyway, there is a downside. For example, if there is no key named "name" in request body, then the user will get back a json object like:

```json
{ data    : null
, error   : true
, message : "cannot parse 'Text' associated with key 'name'"
}
```

Again, this is usefull when you do not redirect after post and not otherwise.
