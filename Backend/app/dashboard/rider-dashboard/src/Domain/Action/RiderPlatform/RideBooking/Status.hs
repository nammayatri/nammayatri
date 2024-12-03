{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.RideBooking.Status (getStatusGetFRFSTicketStatus) where

import qualified API.Client.RiderPlatform.RideBooking
import Domain.Types.FRFSTicketBooking
import qualified "lib-dashboard" Domain.Types.Merchant
import Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getStatusGetFRFSTicketStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
getStatusGetFRFSTicketStatus merchantShortId opCity apiTokenInfo bookingId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction (do API.Client.RiderPlatform.RideBooking.callRideBookingAPI checkedMerchantId opCity (.statusDSL.getStatusGetFRFSTicketStatus) bookingId)
