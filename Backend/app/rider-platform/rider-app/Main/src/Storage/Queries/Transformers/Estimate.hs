{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.Estimate where

import Domain.Types.Estimate as DE
import qualified Domain.Types.Estimate
import qualified Domain.Types.TripTerms
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (HighPrecMoney, KvDbFlow, MonadFlow, Money, fromMaybeM, getCurrentTime, mkPrice, mkPriceWithDefault)
import qualified Storage.Queries.TripTerms as QTT

mkNightShiftInfo :: (Kernel.Prelude.Maybe Kernel.Types.Common.Money -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay -> Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay -> Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Domain.Types.Estimate.NightShiftInfo)
mkNightShiftInfo nightShiftCharge nightShiftChargeAmount nightShiftEnd nightShiftStart oldNightShiftCharge currency =
  ((,,) <$> nightShiftCharge <*> nightShiftStart <*> nightShiftEnd)
    <&> \(nightShiftCharge', nightShiftStart', nightShiftEnd') ->
      DE.NightShiftInfo
        { nightShiftCharge = mkPriceWithDefault nightShiftChargeAmount currency nightShiftCharge',
          oldNightShiftCharge = oldNightShiftCharge,
          nightShiftStart = nightShiftStart',
          nightShiftEnd = nightShiftEnd'
        }

mkTollChargesInfo :: (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe [Kernel.Prelude.Text] -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Domain.Types.Estimate.TollChargesInfo)
mkTollChargesInfo tollCharges tollNames currency =
  ((,) <$> tollCharges <*> tollNames)
    <&> \(tollCharges', tollNames') ->
      DE.TollChargesInfo
        { tollCharges = mkPriceWithDefault (Just tollCharges') currency (round tollCharges' :: Money),
          tollNames = tollNames'
        }

mkFareRange :: (Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Domain.Types.Estimate.FareRange)
mkFareRange currency maxTotalFare minTotalFare =
  DE.FareRange
    { minFare = mkPrice currency minTotalFare,
      maxFare = mkPrice currency maxTotalFare
    }

mKTripTerms :: KvDbFlow m r => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Kernel.Prelude.Maybe Domain.Types.TripTerms.TripTerms))
mKTripTerms tripTermsId = if isJust tripTermsId then QTT.findById'' (Kernel.Types.Id.Id (fromJust tripTermsId)) else pure Nothing

mknightShiftCharge :: Maybe NightShiftInfo -> Maybe Money
mknightShiftCharge nightShiftInfo = nightShiftInfo <&> (.nightShiftCharge.amountInt)

mknightShiftChargeAmount :: Maybe NightShiftInfo -> Maybe HighPrecMoney
mknightShiftChargeAmount nightShiftInfo = nightShiftInfo <&> (.nightShiftCharge.amount)

mkMaxTotalFare :: FareRange -> HighPrecMoney
mkMaxTotalFare = (.maxFare.amount)

mkMinTotalFare :: FareRange -> HighPrecMoney
mkMinTotalFare = (.minFare.amount)
