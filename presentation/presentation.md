% Duffer's Guide to `git` Internals
% Vaibhav Sagar

# What

## My Library

- an API for `git` repositories
- Pure Haskell

# Demo

# Code

## Types

```haskell
data GitObject
    = Blob {content :: B.ByteString}
    | Tree {entries :: Set TreeEntry}
    | Commit
        { treeRef       :: Ref
        , parentRefs    :: [Ref]
        , authorTime    :: PersonTime
        , committerTime :: PersonTime
        , message       :: B.ByteString
        }
    | Tag
        { objectRef  :: Ref
        , objectType :: String
        , tagName    :: String
        , tagger     :: PersonTime
        , annotation :: B.ByteString
        }
deriving (Eq)
```
<div class="notes">
- A Blob   represents a file
- A Tree   represents a directory listing
- A Commit represents a snapshot of a project at a point in time
- A Tag    names another `git` object.
</div>

## Types

```haskell
data TreeEntry = TreeEntry
    { entryPerms :: Int
    , entryName  :: B.ByteString
    , entryRef   :: Ref
    }
    deriving (Eq)

data PersonTime = PersonTime
    { personName :: B.ByteString
    , personMail :: B.ByteString
    , personTime :: B.ByteString
    , personTZ   :: B.ByteString
    }
    deriving (Eq)

type Ref  = B.ByteString
type Repo = FilePath
```
<div class="notes">
Included for completeness.
</div>

## Functor

```haskell
parseTree :: Parser GitObject
parseTree = Tree . fromList <$> many' parseTreeEntry
```

<div class="notes">
- `attoparsec` is easily the best library I've ever used
- provides a compelling reason to learn about Functor/Applicative/Monad
</div>

## Applicative

```haskell
parseMessage :: Parser B.ByteString
parseMessage = endOfLine *> takeByteString
```

<div class="notes">
I think this reads pretty well.
</div>

## Monad (Transformers)

```haskell
type WithRepo = ReaderT Repo IO

hasObject :: Ref -> WithRepo Bool
hasObject ref = do
    path <- asks (sha1Path ref)
    liftIO $ doesFileExist path
```

<div class="notes">
- I was threading through the repository path as a parameter until I found a
  random blog post talking about using the Reader monad for configuration. It's
  a perfect fit for this problem. Also convinced me that monad transformers are
  a good idea.
</div>

## Testing

- `duffer` uses itself to test itself.

<div class="notes">
- `git` hashes everything so if even one byte is off it is very obvious.
- Free test cases!
</div>

# Functional Git

## Data Structures

- Merkle trees & Merkle DAGs
- Not too different from an efficiently implemented functional data structure.

## Design

- Append-only object store and mutable ref store
- Separating the immutable and the mutable? Sounds like this language I know...

# More

## What's next?

- servant => type-safe web APIs
- lens    => a better API

## Ideas

- A git-based project management system
- Using git as the backend of an application

# My experience with Haskell

## Good

- Refactoring is a joy
- Testing is easy with small, mostly pure functions, and a git repository.
- Debugging is straightforward
- Compositional tools blow other languages out of the water
- Lots of support on IRC and r/haskell
- IHaskell is a godsend and everyone should use it
- types that encode side effects are genius
- My frustration dealing with deserialisation reminded me why we have types in
  the first place

## Bad

- Documentation is mostly terrible - random blog posts
- Hard to find the right libraries - more random blog posts
- Very demanding learning curve - difficult to see the motivation
- As a community, how can we improve?

## Some thoughts

- I thought Haskell was pretty silly the first time I encountered it
- Only after a couple of bad production experiences did I see the point
- How else do we provide a compelling reason to learn it?
- Maybe we just need to convince everyone that Haskell is a good idea and
  let them find it in their own time

# What can you do with this

## Historical Revisionism

- https://github.com/vaibhavsagar/git-internals-workshop

# Further Reading

## Better Implementations

- https://github.com/vincenthz/hit - a pure Haskell implementation of Git objects
- https://github.com/jwiegley/gitlib - A type-safe interface with many backends
- http://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/

## Interesting Projects

- https://github.com/mirage/irmin - A distributed database with Git-like design

## About this project

- Code is at <https://github.com/vaibhavsagar/duffer>
- Presentation is in the `gh-pages` branch or
  <http://www.vaibhavsagar.com/duffer>
- Started to give a presentation on `git` internals to the Canberra Functional
  Programming Group
- My first ever project in Haskell

# Questions?
