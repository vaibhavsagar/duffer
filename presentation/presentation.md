% Duffer's guide to Git internals
% Vaibhav Sagar

# Introduction

## About Me

- Aspiring functional programmer
- Enjoy poking at Git for fun and profit

## You will learn

- Reading blobs, trees, tags, and commits
- Writing blobs, trees, tags, and commits

## You will not learn

- Reading and writing packfiles
- Interacting with the git index
- How to reimplement Git

# Git Objects

## Basic

- Objects start with a header denoting their type, content length, and a null byte
- Objects are zlib compressed.
- Objects are uniquely identified by their SHA1 hash.
- Objects are stored in `.git/objects/<sha1[0:2]>/<sha1[2:]>`
- Objects can be inspected with `git cat-file -p` or `git cat-file <type>`

## Intermediate

- Branches and tags are implemented as references
- Any git object can be tagged
- tags can be signed with a key (not implemented)

## Advanced (not implemented)

- objects can be packed into a packfile
- the git index is a list of all blobs in the repository with stat(2) data
- HEAD is a symbolic reference instead of a normal reference

# Blob

## Details

- \<content\> => "blob \<length\>\\NUL\<content\>"
- e.g. b75f4c9dbe3b61cacba052f23461834468832e41

# Tree

## Details

- \<content\> is a sorted list of tree entries
- \<entry\> => "\<mode\> \<name\>\\NUL\<SHA1\>"
- tree entries are either blobs or other trees
- e.g. `git cat-file tree HEAD`
- `git submodule` uses a commit as a tree entry (not implemented)

# Commit

## Details

- tree, parent(s), author, committer, message
- e.g. `git cat-file commit HEAD`
- initial commit has no parent ref
- this is where Git's DAG comes from
- commits can be GPG signed (not implemented)

# Tag

## Details

- object, type, tag, tagger, annotation
- ref is created at `refs/tags/<name>` pointing to tag object
- tags can be GPG signed (not implemented)

# Ref

## Details

- a file at `refs/heads/<name>` with commit SHA1
- a file at `refs/tags/<name>` with SHA1 (lightweight tag) or tag SHA1 (annotated tag)

# Demo

## Tasks

- Create a "hello world" blob and store it
- Create a new branch with the same history
- Add the "hello world" blob to the root tree and write it (index will not be updated)
- Create a new commit on the new branch
- View the parent of the new commit
- Watch things break horribly

# Git Annex

## Details

- Stores files under `.git/annex/[0:2]/[2:4]/[4:]`
- stores a symlink or pointer where the file should be
- Has a pre-commit hook to fix file paths if files are moved

# Further Reading

## Better Implementations

- https://github.com/jwiegley/gitlib - A type-safe interface with many backends
- https://github.com/vincenthz/hit - a pure Haskell implementation of Git objects
- http://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/

## Interesting Projects

- https://github.com/mirage/irmin - A distributed database with Git-like design

## Ideas

- A git-based project management system
- A web application to serve git objects and packfiles
- Using git as the backend of an application

# Questions?
