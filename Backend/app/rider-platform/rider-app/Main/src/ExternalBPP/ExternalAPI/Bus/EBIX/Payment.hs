module ExternalBPP.ExternalAPI.Bus.EBIX.Payment where

import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.FRFSTicketBooking
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

getPaymentDetails :: (MonadFlow m) => Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m BknPaymentParams
getPaymentDetails _merchant _merchantOperatingCity _bapConfig (_mRiderName, _mRiderNumber) _booking = throwError $ InternalError "EBIX getPaymentDetails: to be implemented after specifications shared by the EBIX provider"
