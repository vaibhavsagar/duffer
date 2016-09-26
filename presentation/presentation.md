% Duffer's guide to Git internals
% Vaibhav Sagar

# Introduction

## About Me

- Former web developer
- Aspiring functional programmer
- Enjoy poking at Git for fun and profit
- Attending a batch at the Recurse Center, where I've had the time and space to
  work on this

## The Recurse Center

<img src="./emoji_rc.svg" height=400 width=400
  style="border: none;background: none;box-shadow: none;"/>

## About this project

- Code is at <https://github.com/vaibhavsagar/duffer>
- Presentation is in the `gh-pages` branch or
  <http://www.vaibhavsagar.com/duffer>
- Started to give a presentation on `git` internals to the Canberra Functional
  Programming Group
- My first ever project in Haskell

# Demo

# Code

# Concepts

## Libraries

- attoparsec   => convinced me that Functor, Applicative, Monad is a good idea
- transformers => coolest package name ever
- pipes        => streaming

## Further avenues of exploration

- servant => type-safe web APIs
- lens    => a better API

## Ideas

- A git-based project management system
- Using git as the backend of an application

# My experience with Haskell

## Good

- Refactoring is a joy
- Testing is a joy
- Debugging is straightforward
- fitting small components together is more satisfying than any other language
  I've used
- Lots of support on IRC and r/haskell
- IHaskell is a godsend and everyone should use it
- types that encode side effects + syntax sugar for category theoretic concepts = magic
- My frustration dealing with deserialisation reminded me why we have types in the first place

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

# Questions?
