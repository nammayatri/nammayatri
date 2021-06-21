module Beckn.Types.Predicate
  ( module Beckn.Types.Predicate,
    module Kleene,
    (\/),
  )
where

import Algebra.Lattice
import qualified Data.Foldable as F
import Data.Ix (inRange)
import qualified Data.Text as T
import EulerHS.Prelude
import Kleene
import qualified Kleene.DFA as KDFA
import Kleene.Internal.Pretty (pretty)

type PredShow p = p -> Text -> Text

type PredFun p a = p -> a -> Bool

class Predicate a p where
  pFun :: PredFun p a

class ShowablePredicate p where
  pShow :: PredShow p

type Regex = RE Char

anyOf :: [Char] -> Regex
anyOf = joins . map (fromString . (: []))

instance Predicate String Regex where
  pFun = match . KDFA.fromRE

instance Predicate Text Regex where
  pFun re a = match (KDFA.fromRE re) (T.unpack a)

instance ShowablePredicate Regex where
  pShow regex var = var <> " matches regex /" <> T.pack (pretty regex) <> "/"

data And p1 p2 = And p1 p2

instance (Predicate x p1, Predicate x p2) => Predicate x (And p1 p2) where
  pFun (And p1 p2) = liftBinPredFun (&&) p1 p2

instance (ShowablePredicate p1, ShowablePredicate p2) => ShowablePredicate (And p1 p2) where
  pShow (And p1 p2) = liftBinPredShow "and" p1 p2

data Or p1 p2 = Or p1 p2

instance (Predicate x p1, Predicate x p2) => Predicate x (Or p1 p2) where
  pFun (Or p1 p2) = liftBinPredFun (||) p1 p2

instance (ShowablePredicate p1, ShowablePredicate p2) => ShowablePredicate (Or p1 p2) where
  pShow (Or p1 p2) = liftBinPredShow "or" p1 p2

newtype Not p = Not p

instance (Predicate x p) => Predicate x (Not p) where
  pFun (Not p) = not . pFun p

instance (ShowablePredicate p) => ShowablePredicate (Not p) where
  pShow (Not p) = liftPredShow "not" p

newtype ExactLength = ExactLength Int

instance ShowablePredicate ExactLength where
  pShow (ExactLength len) name = showFunction "length" name <> " == " <> show len

instance F.Foldable l => Predicate (l a) ExactLength where
  pFun (ExactLength len) l = F.length l == len

instance Predicate Text ExactLength where
  pFun (ExactLength len) l = T.length l == len

data LengthInRange = LengthInRange Int Int

instance ShowablePredicate LengthInRange where
  pShow (LengthInRange a b) name = show a <> " <= " <> showFunction "length" name <> " <= " <> show b

instance F.Foldable l => Predicate (l a) LengthInRange where
  pFun (LengthInRange a b) l = inRange (a, b) $ F.length l

instance Predicate Text LengthInRange where
  pFun (LengthInRange a b) l = inRange (a, b) $ T.length l

newtype MinLength = MinLength Int

instance ShowablePredicate MinLength where
  pShow (MinLength m) name = showFunction "length" name <> " >= " <> show m

instance F.Foldable l => Predicate (l a) MinLength where
  pFun (MinLength m) l = F.length l >= m

instance Predicate Text MinLength where
  pFun (MinLength m) l = T.length l >= m

newtype MaxLength = MaxLength Int

instance ShowablePredicate MaxLength where
  pShow (MaxLength m) name = showFunction "length" name <> " <= " <> show m

instance F.Foldable l => Predicate (l a) MaxLength where
  pFun (MaxLength m) l = F.length l <= m

instance Predicate Text MaxLength where
  pFun (MaxLength m) l = T.length l <= m

data NotEmpty = NotEmpty

instance ShowablePredicate NotEmpty where
  pShow NotEmpty name = name <> " is not empty"

instance F.Foldable l => Predicate (l a) NotEmpty where
  pFun NotEmpty l = not (F.null l)

instance Predicate Text NotEmpty where
  pFun NotEmpty l = not (T.null l)

data InRange n = InRange n n

instance Show n => ShowablePredicate (InRange n) where
  pShow (InRange a b) name = show a <> " <= " <> name <> " <= " <> show b

instance Ord n => Predicate n (InRange n) where
  pFun (InRange a b) n = a <= n && n <= b

newtype Min n = Min n

instance Show n => ShowablePredicate (Min n) where
  pShow (Min m) name = name <> " >= " <> show m

instance Ord n => Predicate n (Min n) where
  pFun (Min m) = (>= m)

newtype Max n = Max n

instance Show n => ShowablePredicate (Max n) where
  pShow (Max m) name = name <> " <= " <> show m

instance Ord n => Predicate n (Max n) where
  pFun (Max m) = (<= m)

liftPredShow :: ShowablePredicate p => Text -> p -> Text -> Text
liftPredShow fname p text = showFunction fname (pShow p text)

showFunction :: Text -> Text -> Text
showFunction fname arg = fname <> parenthesized arg

liftBinPredShow ::
  (ShowablePredicate p1, ShowablePredicate p2) =>
  Text ->
  p1 ->
  p2 ->
  Text ->
  Text
liftBinPredShow str a b text = parenthesized $ pShow a text <> " " <> str <> " " <> pShow b text

liftBinPredFun ::
  (Predicate x p1, Predicate x p2) =>
  (Bool -> Bool -> Bool) ->
  p1 ->
  p2 ->
  x ->
  Bool
liftBinPredFun op a b x = pFun a x `op` pFun b x

parenthesized :: Text -> Text
parenthesized x = "(" <> x <> ")"
