% Duffer's Guide to `git` Internals
% Vaibhav Sagar

<div class="notes">
I'm going to try to give two talks in one: a `git` internals talk as well as
a Haskell beginner talk. That is because this is the first ever Haskell code
I wrote from scratch. I hope that despite my relative inexperience you will
learn something new.

I have had zero success explaining either `git` or Haskell to people by
talking at them, so please feel free to stop me and ask questions. I'm also
a very fast talker expecially when I'm nervous, so don't hesitate to tell me
to slow down.
</div>

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

## Bit Twiddling

```haskell
setMSBs :: [Int] -> [Int]
setMSBs ints = let
    ints'  = reverse ints
    ints'' = head ints' : map (`setBit` 7) ( tail ints')
in reverse ints''
```

<div class="notes">
This is where most of my bugs live. I think this comes back to why we have
types in the first place: an invalid bytestring typechecks the same as a
valid bytestring, just like all data is bytes in memory. Serialisation was
much more straightforward.

I'm actually currently dealing with a bug where two different bytestrings
parse to the same value. I'm hoping that my deserialisation logic is the
culprit.
</div>

## Streaming

```haskell
getNextEntry = do
    Just (Right tLen) <- PA.parse parseTypeLen
    baseRef <- case fst tLen of
        OfsDeltaObject -> do
            Just (Right offset) <- PA.parse parseOffset
            return $ encodeOffset offset
        RefDeltaObject -> do
            Just (Right ref)    <- PA.parse parseBinRef
            return $ fst $ decode ref
        _              -> return ""
    remainder <- get
    let decompressed = PZ.decompress' PZ.defaultWindowBits remainder
    PB.drawByte
    Just levelByte <- PB.peekByte
    let level = getCompressionLevel levelByte
    return (uncurry encodeTypeLen tLen, baseRef, decompressed, level)
```

<div class="notes">
This code is gross, but it works! I asked for help with this and I think the
consensus was that the libraries for streaming `zlib` decompression are less
than ideal.
</div>

## Layout

```
src
├── Duffer
│   ├── Loose
│   │   ├── Objects.hs
│   │   └── Parser.hs
│   ├── Loose.hs
│   ├── Pack
│   │   ├── Entries.hs
│   │   ├── File.hs
│   │   ├── Parser.hs
│   │   └── Streaming.hs
│   ├── Pack.hs
│   └── Porcelain.hs
└── Duffer.hs
```

<div class="notes">
`git` has a packfile representation that is used for compression and network
transfer. I have parsers for the loose and packed representations and
additional logic for dealing with some quirks of the packfile format.
</div>

## Testing

- `duffer` uses its own repo to test itself.

<div class="notes">
- `git` hashes everything so if even one byte is off it is very obvious.
- Free test cases every time a new commit is created
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

- A `git` web server?
- A better API
- Multiple backends

<div class="notes">
`servant` seems like the obvious choice for writing a web server, and maybe
`lens` would help me come up with a better API for my library?

There's no reason the filesystem has to be the backing store of a repository.
</div>

## Ideas

- A git-based project management system
- Using git as the backend of an application

# My experience with Haskell

## Advice for beginners

- Don't get too hung up on understanding monads before you write any code
- Lots of support on IRC and r/haskell
- IHaskell is a godsend and everyone should use it

<div class="notes">
- What worked for me was learning how to desugar `do`-notation.
- Haskell stars like Gabriel Gonzalez, OCharles, and EKmett hang out there.
- It's the one thing keeping me from moving to GHC 8.
</div>

## Bad

- Documentation is mostly terrible
- Hard to find the right libraries
- Very demanding learning curve

<div class="notes">
- random blog posts or r/haskell comments
- more random blog posts
- difficult to see the motivation
</div>

## Some thoughts

- I thought Haskell was pretty silly the first time I encountered it
- Only after a couple of bad production experiences did I see the point
- How else do we provide a compelling reason to learn it?

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

## RC

<img src="http://emoji.octopus.holdings/emoji/emoji_rc.svg" width=400 height=400>

https://www.recurse.com

<div class="notes">
In lieu of an employer slide I thought I'd mention that I'm in the middle of my
batch at RC, and I love it here. If you're interested check us out :)
</div>

# Questions?
