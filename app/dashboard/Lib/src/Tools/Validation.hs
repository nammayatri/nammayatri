{-# LANGUAGE InstanceSigs #-}

module Tools.Validation where

import Beckn.Prelude
import Beckn.Types.Predicate
import qualified Data.Text as T

-- TODO move in Beckn.Types.Predicate
newtype InList a = InList [a]

instance Eq a => Predicate a (InList a) where
  pFun :: InList a -> a -> Bool
  pFun (InList list) a = a `elem` list

instance Show a => ShowablePredicate (InList a) where
  pShow :: InList a -> Text -> Text
  pShow (InList list) name = name <> " in list " <> T.intercalate ", " (show <$> list)
