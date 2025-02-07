module ExternalBPP.ExternalAPI.Bus.EBIX.Payment where

import Domain.Types
import Domain.Types.BecknConfig
import Domain.Types.FRFSTicketBooking
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude

getPaymentDetails :: Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m BknPaymentParams
getPaymentDetails _merchant _merchantOperatingCity _bapConfig (_mRiderName, _mRiderNumber) _booking = error "To be implemented, after specifications shared by the EBIX provider"
