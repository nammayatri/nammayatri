-- Patches Layer 1's on_init message with BPP_TERMS, ROUTE_INFO, fulfillment.type and vehicle.energy_type overrides, since Layer 1 builds it identically for every merchant unaware of the ONDC pilot.
module Beckn.OnDemand.Transformer.OndcScheduledRide.OnInit
  ( ondcScheduledRideOnInitMessageBuild,
  )
where

import qualified Beckn.OnDemand.Utils.OndcScheduledRide.Common as OSRCommon
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Storage.CachedQueries.BapMetadata as CQBapMetaData

-- | Patches Layer 1's on_init order: breakup titles, BPP_TERMS, ROUTE_INFO, fulfillment.type, vehicle.energy_type.
ondcScheduledRideOnInitMessageBuild :: (EsqDBFlow m r, CacheFlow m r, MonadFlow m) => DRB.Booking -> Text -> DBC.BecknConfig -> Spec.ConfirmReqMessage -> m Spec.ConfirmReqMessage
ondcScheduledRideOnInitMessageBuild booking bapId bppConfig msg = do
  mbBapMetadata <- CQBapMetaData.findBySubscriberIdAndDomain (Id bapId) Domain.MOBILITY
  let orderWithBreakupTitles = OSRCommon.overrideOrderBreakupTitles msg.confirmReqMessageOrder
  orderWithOverrides <- OSRCommon.applyOnInitOrderOverrides booking.transactionId mbBapMetadata bppConfig orderWithBreakupTitles
  pure msg {Spec.confirmReqMessageOrder = orderWithOverrides}
