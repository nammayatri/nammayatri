{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.RideBooking.Status (getStatusGetFRFSTicketStatus) where

import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils
import Data.OpenApi (ToSchema)
import Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified ExternalBPP.CallAPI as CallBPP
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import Servant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CMOC
import qualified Storage.Queries.BecknConfig as QBC
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import Tools.Auth
import Tools.Error

getStatusGetFRFSTicketStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
getStatusGetFRFSTicketStatus _merchantShortId _opCity bookingId = do
  booking <- QTBooking.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  let merchantId = booking.merchantId
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle (Just merchantId) (show Spec.FRFS) (BecknV2.FRFS.Utils.frfsVehicleCategoryToBecknVehicleCategory booking.vehicleType) >>= fromMaybeM (InternalError "Beckn Config not found")
  merchanOperatingCity <- CMOC.findByMerchantIdAndCity merchantId _opCity >>= fromMaybeM (InternalError "Merchant City not found")
  void $ CallBPP.status merchantId merchanOperatingCity bapConfig booking
  return Kernel.Types.APISuccess.Success
