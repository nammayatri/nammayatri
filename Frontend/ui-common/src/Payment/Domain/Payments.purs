module Domain.Payments where

import Prelude (class Eq, class Show)
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultEnumDecode, defaultEnumEncode)

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Eq.Generic (genericEq)
import Foreign (Foreign)
import Presto.Core.Types.API (standardEncode,class StandardEncode)
import Foreign.Generic (class Decode, class Encode)
import Control.Monad.Free (Free)
import Control.Monad.Except.Trans (ExceptT)
import Presto.Core.Types.Language.Flow (FlowWrapper)
import Control.Transformers.Back.Trans (BackT)
import Data.Maybe (Maybe(..))


data PaymentStatus = Success | Pending | Failed | Scheduled

derive instance genericPaymentStatus :: Generic PaymentStatus _
instance standardEncodePaymentStatus :: StandardEncode PaymentStatus where standardEncode _ = standardEncode {}
instance showPaymentStatus :: Show PaymentStatus where show = genericShow
instance decodePaymentStatus :: Decode PaymentStatus where decode = defaultDecode
instance encodePaymentStatus  :: Encode PaymentStatus where encode = defaultEncode
instance eqPaymentStatus :: Eq PaymentStatus where eq = genericEq

data APIPaymentStatus =  NEW
                      | PENDING_VBV
                      | CHARGED
                      | AUTHENTICATION_FAILED 
                      | AUTHORIZATION_FAILED
                      | JUSPAY_DECLINED
                      | AUTHORIZING
                      | COD_INITIATED
                      | STARTED
                      | AUTO_REFUNDED

derive instance genericAPIPaymentStatus :: Generic APIPaymentStatus _
instance showAPIPaymentStatus :: Show APIPaymentStatus where show = genericShow
instance decodeAPIPaymentStatus :: Decode APIPaymentStatus where decode = defaultEnumDecode
instance encodeAPIPaymentStatus  :: Encode APIPaymentStatus where encode = defaultEnumEncode
instance eqAPIPaymentStatus :: Eq APIPaymentStatus where eq = genericEq
instance standardEncodeAPIPaymentStatus :: StandardEncode APIPaymentStatus where standardEncode _ = standardEncode {}