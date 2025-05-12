module MerchantConfig.Utils where

import Prelude
import Common.Types.App (LazyCheck(..))
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Foreign.Generic (class Decode, class Encode, decode)
import Presto.Core.Utils.Encoding (defaultEnumDecode, defaultEnumEncode)
import Debug
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Data.Show.Generic (genericShow) 
import Language.Types (STR(..))

foreign import getStringFromConfig :: forall a. STR -> (a -> Maybe a) -> (Maybe a) -> Maybe String

foreign import getMerchantId :: String -> Foreign

foreign import getStringWithVar :: String -> Array String -> String

data Merchant
  = NAMMAYATRI
  | YATRISATHI
  | YATRI
  | MOBILITY_PM
  | PASSCULTURE
  | MOBILITY_RS
  -- | FLEETX -- Todo: Shikhar add a merchant for fleetx

derive instance genericMerchant :: Generic Merchant _
instance showMerchant :: Show Merchant where show = genericShow
instance eqMerchant :: Eq Merchant where
  eq = genericEq

instance encodeMerchant :: Encode Merchant where
  encode = defaultEnumEncode

instance decodeMerchant :: Decode Merchant where
  decode = defaultEnumDecode

getMerchant :: LazyCheck -> Merchant
getMerchant lazy = case decodeMerchantId (getMerchantId "") of
  Just merchant -> merchant
  Nothing -> NAMMAYATRI

decodeMerchantId :: Foreign -> Maybe Merchant
decodeMerchantId = hush <<< runExcept <<< decode

