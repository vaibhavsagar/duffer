module Duffer.Packed.PackFile where

import qualified Data.ByteString as B

import Duffer.Loose.Objects

data PackObjectType
    = UnusedPackObjectType0
    | CommitObject
    | TreeObject
    | BlobObject
    | TagObject
    | UnusedPackObjectType5
    | OfsDeltaObject
    | RefDeltaObject
    deriving (Enum, Eq, Show)

data PackDelta
    = OfsDelta Int Delta
    | RefDelta Ref Delta
    deriving (Show)

data PackEntry
    = PackedObject PackObjectType Ref B.ByteString
    | PackedDelta  PackObjectType PackDelta
    deriving (Show)

data DeltaInstruction
    = CopyInstruction   Int Int
    | InsertInstruction B.ByteString
    deriving (Show)

data Delta = Delta Int Int [DeltaInstruction] deriving (Show)
