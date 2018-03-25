{-# LANGUAGE DeriveGeneric #-}
module Message where

import Data.Binary
import Data.List (sortOn)
import Data.Typeable (Typeable)
import GHC.Generics
import System.Clock

data M = M Double TimeSpec
  deriving (Typeable, Generic, Eq, Show)

instance Binary TimeSpec where
  put (TimeSpec s n) = put s >> put n
  get = TimeSpec <$> get <*> get

instance Binary M where
  put (M x t) = put x >> put t
  get = M <$> get <*> get

merge :: [M] -> M -> [M]
merge messages newMessage =
  let
    equal (M _ t1) (M _ t2) = toNanoSecs t1 == toNanoSecs t2
    filtered = filter (\m -> equal m newMessage) messages
  in
    case filtered of
      [] -> messages ++ [newMessage]
      _ -> messages

sort :: [M] -> [M]
sort messages =
  sortOn (\(M _ t) -> toNanoSecs t) messages

calcResult :: [M] -> (Int, Double)
calcResult messages =
  let
    indexed = zip [0..] messages
    step acc (i, (M x _)) = acc + (i * x)
  in
  ( length messages
  , foldl step 0 indexed
  )
