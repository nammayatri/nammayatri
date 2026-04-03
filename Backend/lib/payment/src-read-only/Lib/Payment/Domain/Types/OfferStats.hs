{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Lib.Payment.Domain.Types.OfferStats where
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.Offer
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Tools.Beam.UtilsTH



data OfferStats
    = OfferStats {createdAt :: Kernel.Prelude.UTCTime,
                  entityId :: Kernel.Prelude.Text,
                  entityType :: Lib.Payment.Domain.Types.OfferStats.OfferStatsEntityType,
                  id :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.OfferStats.OfferStats,
                  offerAppliedCount :: Kernel.Prelude.Int,
                  offerId :: Kernel.Types.Id.Id Lib.Payment.Domain.Types.Offer.Offer,
                  updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, ( Show), ( Read), ( ToJSON), ( FromJSON))
data OfferStatsEntityType = Person | StaticPerson | Device | Offer deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''OfferStatsEntityType))

