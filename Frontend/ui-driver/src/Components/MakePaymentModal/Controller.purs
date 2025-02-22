module Components.MakePaymentModal.Controller where

import Components.PrimaryButton as PrimaryButton
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Prelude
import Data.Maybe as MB

instance showAction :: Show Action where
  show (MakePayment) = "MakePayment"
  show (PrimaryButtonActionController var1) = "PrimaryButtonActionController_" <> show var1
  show (NoAction) = "NoAction"
  show (Cancel) = "Cancel"
  show (Info) = "Info"
  
data Action = MakePayment 
              | PrimaryButtonActionController PrimaryButton.Action
              | NoAction
              | Cancel
              | Info

type MakePaymentModalState = {
  title :: String,
  description :: String,
  feeItem :: Array FeeItem,
  ridesCount :: Int,
  description2 :: String,
  okButtontext :: String,
  cancelButtonText :: MB.Maybe String
}

type FeeItem = {
  feeType :: FeeOptions,
  title :: String,
  val :: Int
}

data FeeOptions = TOTAL_COLLECTED | EARNED_OF_THE_DAY | GST_PAYABLE
derive instance genericFeeOptions :: Generic FeeOptions _
instance eqFeeOptions :: Eq FeeOptions where eq = genericEq