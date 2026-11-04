module SharedLogic.FareBreakdown
  ( FareBreakdown (..),
    FareBreakdownItem (..),
    buildFareBreakdown,
    getRideFareRows,
    withOfferTotals,
    mkFareBreakdown,
  )
where

import qualified Data.List as DL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Domain.SharedLogic.RideDiscount (isProjectedFareParamTag)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.OfferEntity as DOfferEntity
import qualified Domain.Types.Ride as DRide
import Kernel.Beam.Functions (runInReplica)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common
import Kernel.Utils.Common
import SharedLogic.Booking (getfareBreakups)
import qualified SharedLogic.FareBreakupInfo as SFareBreakupInfo
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.OfferEntity as QOfferEntity

data FareBreakdown = FareBreakdown
  { estimateRows :: [DFareBreakup.FareBreakup],
    finalRows :: [DFareBreakup.FareBreakup],
    items :: [FareBreakdownItem],
    estimatedTotal :: HighPrecMoney,
    finalTotal :: Maybe HighPrecMoney,
    currency :: Currency,
    estimatedDistance :: Maybe HighPrecMeters,
    finalDistance :: Maybe HighPrecMeters
  }
  deriving (Generic, Show)

data FareBreakdownItem = FareBreakdownItem
  { code :: Text,
    estimated :: Maybe HighPrecMoney,
    final :: Maybe HighPrecMoney
  }
  deriving (Generic, Show)

buildFareBreakdown ::
  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, MonadFlow m) =>
  DBooking.Booking ->
  Maybe DRide.Ride ->
  m FareBreakdown
buildFareBreakdown booking mbRide = do
  (finalFareRows, estimateFareRows) <- getfareBreakups booking mbRide
  pure $ mkFareBreakdown booking mbRide estimateFareRows finalFareRows

getRideFareRows ::
  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, MonadFlow m) =>
  FareBreakdown ->
  DRide.Ride ->
  m [DFareBreakup.FareBreakup]
getRideFareRows breakdown ride =
  if not (null breakdown.finalRows) && all ((== DFareBreakup.RIDE) . (.entityType)) breakdown.finalRows
    then pure breakdown.finalRows
    else SFareBreakupInfo.getFareBreakupsWithFallback ride.id.getId DFareBreakup.RIDE (runInReplica $ QFareBreakup.findAllByEntityIdAndEntityType ride.id.getId DFareBreakup.RIDE)

withOfferTotals ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  DBooking.Booking ->
  Maybe DRide.Ride ->
  FareBreakdown ->
  m FareBreakdown
withOfferTotals booking mbRide breakdown =
  case booking.selectedOfferId of
    Nothing -> pure breakdown
    Just _ -> do
      mbBookingOffer <- QOfferEntity.findByEntityIdAndEntityType booking.id.getId DOfferEntity.BOOKING
      mbRideOffer <- join <$> mapM (\ride -> QOfferEntity.findByEntityIdAndEntityType ride.id.getId DOfferEntity.RIDE) mbRide
      let tipAmount = maybe 0 (.amount) (mbRide >>= (.tipAmount))
      pure
        breakdown
          { estimatedTotal = fromMaybe breakdown.estimatedTotal (discountedAmount mbBookingOffer),
            finalTotal = breakdown.finalTotal <&> \rideTotal -> maybe rideTotal (+ tipAmount) (discountedAmount mbRideOffer)
          }
  where
    discountedAmount mbOffer = (.postOfferAmount) <$> (mbOffer >>= \offer -> if offer.discountAmount > 0 then Just offer else Nothing)

mkFareBreakdown :: DBooking.Booking -> Maybe DRide.Ride -> [DFareBreakup.FareBreakup] -> [DFareBreakup.FareBreakup] -> FareBreakdown
mkFareBreakdown booking mbRide estimateFareRows finalFareRows =
  FareBreakdown
    { estimateRows = estimateFareRows,
      finalRows = finalFareRows,
      items = breakdownItems,
      estimatedTotal = booking.estimatedTotalFare.amount,
      finalTotal = (.amount) <$> (mbRide >>= (.totalFare)),
      currency = booking.estimatedTotalFare.currency,
      estimatedDistance = distanceToHighPrecMeters <$> booking.estimatedDistance,
      finalDistance = distanceToHighPrecMeters <$> (mbRide >>= (.chargeableDistance))
    }
  where
    estimates = groupAmounts estimateFareRows
    finals = groupAmounts finalFareRows
    breakdownItems =
      [ FareBreakdownItem
          { code = chargeCode,
            estimated = amountIfPresent estimates chargeCode,
            final = amountIfPresent finals chargeCode
          }
        | chargeCode <- DL.sortOn chargeOrder (Map.keys (Map.union finals estimates)),
          isNonZero (Map.findWithDefault 0 chargeCode estimates) || isNonZero (Map.findWithDefault 0 chargeCode finals)
      ]
    amountIfPresent amounts chargeCode =
      if Map.null amounts then Nothing else Just (Map.findWithDefault 0 chargeCode amounts)
    isNonZero amount = (round (amount * 100) :: Integer) /= 0
    chargeOrder chargeCode = (fromMaybe (length chargePriority) (DL.elemIndex chargeCode chargePriority), chargeCode)

chargePriority :: [Text]
chargePriority = ["BASE_FARE", "DISTANCE_FARE", "RIDE_DURATION_FARE", "WAITING_OR_PICKUP_CHARGES", "TOLL_CHARGES", "RIDE_VAT", "TOLL_VAT"]

groupAmounts :: [DFareBreakup.FareBreakup] -> Map.Map Text HighPrecMoney
groupAmounts rows =
  Map.fromListWith (+) [(row.description, row.amount.amount) | row <- rows, isShownCharge row.description]
  where
    isShownCharge description =
      not (isProjectedFareParamTag description || "_TAX" `T.isSuffixOf` description || "_TAX_EXCLUSIVE" `T.isSuffixOf` description)
        && description `notElem` ["FIXED_GOVERNMENT_RATE", "RIDE_CANCELLATION_CHARGES"]
