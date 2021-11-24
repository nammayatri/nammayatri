module Beckn.Prelude (module E, module Beckn.Prelude) where

import Control.Monad.Reader as E
import Data.Aeson as E (FromJSON (..), ToJSON (..))
import Data.Foldable as E
import Data.Function as E
import Data.Functor as E
import Data.Maybe as E (fromMaybe)
import Data.Proxy as E (Proxy (..))
import Data.Text as E (Text)
import qualified Data.Text as T (pack)
import GHC.Generics as E (Generic, Generic1)
import GHC.Records.Extra as E (HasField)
import Prelude as E

foldWIndex :: (Integer -> acc -> a -> acc) -> acc -> [a] -> acc
foldWIndex f acc p = snd $ foldl' (\(i, acc') c -> (i + 1, f i acc' c)) (0, acc) p

identity :: p -> p
identity a = a

everyPossibleVariant :: (Enum a, Bounded a) => [a]
everyPossibleVariant = [minBound .. maxBound]

showT :: Show a => a -> Text
showT = T.pack . show
