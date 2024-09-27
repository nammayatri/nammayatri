module ExternalBPP.EBIX.ExternalAPI.Order where

import Domain.Types.BecknConfig
import Domain.Types.FRFSTicketBooking
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Utils.Common

getOrderId :: (MonadGuid m) => Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m Text
getOrderId _merchant _merchantOperatingCity _bapConfig (_mRiderName, _mRiderNumber) _booking = do
  -- TODO :: Need to be shared by EBIX team.
  orderId <- generateGUIDText
  return orderId
