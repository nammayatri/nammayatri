module Storage.Queries.Transformers.Estimate where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Domain.Types.Estimate as DE
import qualified Domain.Types.Estimate
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Id (Id (Id))
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, HighPrecMoney, MonadFlow, Money, mkPrice, mkPriceWithDefault)
import qualified Storage.Queries.EstimateBreakup as QEB

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

mkBusinessDiscountInfo :: (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Double -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Domain.Types.Estimate.BusinessDiscountInfo)
mkBusinessDiscountInfo businessDiscount businessDiscountPercentage currency =
  ((,) <$> businessDiscount <*> businessDiscountPercentage)
    <&> \(businessDiscount', businessDiscountPercentage') ->
      DE.BusinessDiscountInfo
        { businessDiscount = mkPriceWithDefault (Just businessDiscount') currency (round businessDiscount' :: Money),
          businessDiscountPercentage = businessDiscountPercentage'
        }

mkPersonalDiscountInfo :: (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Double -> Kernel.Prelude.Maybe Kernel.Types.Common.Currency -> Kernel.Prelude.Maybe Domain.Types.Estimate.PersonalDiscountInfo)
mkPersonalDiscountInfo personalDiscount personalDiscountPercentage currency =
  ((,) <$> personalDiscount <*> personalDiscountPercentage)
    <&> \(personalDiscount', personalDiscountPercentage') ->
      DE.PersonalDiscountInfo
        { personalDiscount = mkPriceWithDefault (Just personalDiscount') currency (round personalDiscount' :: Money),
          personalDiscountPercentage = personalDiscountPercentage'
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

mknightShiftCharge :: Maybe NightShiftInfo -> Maybe Money
mknightShiftCharge nightShiftInfo = nightShiftInfo <&> (.nightShiftCharge.amountInt)

mknightShiftChargeAmount :: Maybe NightShiftInfo -> Maybe HighPrecMoney
mknightShiftChargeAmount nightShiftInfo = nightShiftInfo <&> (.nightShiftCharge.amount)

mkMaxTotalFare :: FareRange -> HighPrecMoney
mkMaxTotalFare = (.maxFare.amount)

mkMinTotalFare :: FareRange -> HighPrecMoney
mkMinTotalFare = (.minFare.amount)

-- | Slim shape persisted in the estimate.estimate_breakup_list_json column.
-- The full domain 'EstimateBreakup' carries id + estimateId which are
-- regenerated deterministically on read; only title and the price value
-- need to be round-tripped through JSON.
data EstimateBreakupItem = EstimateBreakupItem
  { title :: Text,
    price :: Kernel.Types.Common.Price
  }
  deriving (Generic, Show)

instance A.ToJSON EstimateBreakupItem

instance A.FromJSON EstimateBreakupItem

-- | toTType: serialise the in-memory list into the new JSON column.
-- Returns Nothing for an empty list so the column stays NULL and we don't
-- store gratuitous '[]' rows.
encodeEstimateBreakupList :: [DE.EstimateBreakup] -> Maybe A.Value
encodeEstimateBreakupList [] = Nothing
encodeEstimateBreakupList xs = Just . A.toJSON $ map toItem xs
  where
    toItem eb = EstimateBreakupItem {title = eb.title, price = eb.price.value}

-- | fromTType loader: prefer the JSON column; fall back to the legacy
-- estimate_breakup table query when the column is NULL or fails to parse
-- (so existing rows continue to read correctly during the migration window).
loadEstimateBreakupList ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Maybe A.Value ->
  Id DE.Estimate ->
  m [DE.EstimateBreakup]
loadEstimateBreakupList Nothing estId = QEB.findAllByEstimateIdT estId
loadEstimateBreakupList (Just v) estId =
  case A.fromJSON v :: A.Result [EstimateBreakupItem] of
    A.Success items -> pure $ zipWith (fromItem estId) [0 :: Int ..] items
    A.Error _ -> QEB.findAllByEstimateIdT estId
  where
    fromItem eId idx item =
      DE.EstimateBreakup
        { id = Id (eId.getId <> "-bi-" <> T.pack (show idx)),
          estimateId = eId,
          title = item.title,
          price = DE.EstimateBreakupPrice {value = item.price}
        }
