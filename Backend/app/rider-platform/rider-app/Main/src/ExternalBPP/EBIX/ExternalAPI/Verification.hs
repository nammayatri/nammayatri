module ExternalBPP.EBIX.ExternalAPI.Verification where

import Domain.Types.BecknConfig
import Domain.Types.FRFSTicketBooking
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import Kernel.Utils.Time

getVerificationDetails :: (MonadTime m) => Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m (Text, Text, UTCTime, Text)
getVerificationDetails _merchant _merchantOperatingCity _bapConfig (_mRiderName, _mRiderNumber) _booking = error "To be implemented, after specifications shared by the EBIX provider"
