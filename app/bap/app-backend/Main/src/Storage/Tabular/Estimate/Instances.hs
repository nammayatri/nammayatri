{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Estimate.Instances where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.Estimate as Domain
import qualified Storage.Tabular.Estimate as SEstimate
import qualified Storage.Tabular.EstimateBreakup as ESB
import qualified Storage.Tabular.TripTerms as STripTerms

type FullEstimateT = (SEstimate.EstimateT, [ESB.EstimateBreakupT], Maybe STripTerms.TripTermsT)

instance TType FullEstimateT Domain.Estimate where
  fromTType (SEstimate.EstimateT {..}, estimateBreakupListT, mbTripTermsT) = do
    pUrl <- parseBaseUrl providerUrl
    tripTerms <- forM mbTripTermsT fromTType
    estimateBreakupList <- fromTType `traverse` estimateBreakupListT
    let totalFareRange =
          Domain.FareRange
            { minFare = roundToIntegral minTotalFare,
              maxFare = roundToIntegral maxTotalFare
            }
    return $
      Domain.Estimate
        { id = Id id,
          requestId = fromKey requestId,
          providerUrl = pUrl,
          estimatedFare = roundToIntegral estimatedFare,
          discount = roundToIntegral <$> discount,
          estimatedTotalFare = roundToIntegral estimatedTotalFare,
          nightShiftRate =
            Just $
              Domain.NightShiftRate
                { ..
                },
          ..
        }
  toTType Domain.Estimate {..} = do
    let estimateId = getId id
    let estimateT =
          SEstimate.EstimateT
            { id = estimateId,
              requestId = toKey requestId,
              providerUrl = showBaseUrl providerUrl,
              tripTermsId = toKey <$> (tripTerms <&> (.id)),
              estimatedFare = realToFrac estimatedFare,
              discount = realToFrac <$> discount,
              estimatedTotalFare = realToFrac estimatedTotalFare,
              minTotalFare = realToFrac totalFareRange.minFare,
              maxTotalFare = realToFrac totalFareRange.maxFare,
              nightShiftMultiplier = nightShiftRate >>= (.nightShiftMultiplier),
              nightShiftStart = nightShiftRate >>= (.nightShiftStart),
              nightShiftEnd = nightShiftRate >>= (.nightShiftEnd),
              ..
            }
    let mbTripTermsT = toTType <$> tripTerms
    let estimateBreakupLisTType = toTType <$> estimateBreakupList
    (estimateT, estimateBreakupLisTType, mbTripTermsT)
