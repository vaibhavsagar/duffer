duffer.hs
=========

This is a learning exercise and an API for `git`, in that order. Please do not
use this code in production. Instead I would recommend Vincent Hanquez's
excellent and very educational [hs-git](https://github.com/vincenthz/hs-git/).

# What this library does

- Reads and writes loose blobs, trees, commits, and tags.
- Reads full indexed packfiles.
- Reads and writes git refs.

# Some things this library does not do

- Generate a packfile index from a packfile.
- Read a streamed packfile.
- Support arbitrary backends besides file storage.
- Read .git/index
- Generate human readable diffs.
- Provide Functor, Aplicative, or Monad instances for a repository.
