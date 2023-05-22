{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Estimate.Instances where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import qualified SharedLogic.Estimate as Domain
import qualified Storage.Tabular.Estimate as SEstimate
import qualified Storage.Tabular.EstimateBreakup as ESB
import qualified Storage.Tabular.TripTerms as STripTerms

type FullEstimateT = (SEstimate.EstimateT, [ESB.EstimateBreakupT], Maybe STripTerms.TripTermsT)

instance FromTType FullEstimateT Domain.Estimate where
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
          bppEstimateId = Id bppEstimateId,
          providerUrl = pUrl,
          estimatedFare = roundToIntegral estimatedFare,
          discount = roundToIntegral <$> discount,
          estimatedTotalFare = roundToIntegral estimatedTotalFare,
          driversLocation = unPostgresList driversLocation,
          nightShiftInfo =
            ((,,,) <$> nightShiftCharge <*> oldNightShiftCharge <*> nightShiftStart <*> nightShiftEnd)
              <&> \(nightShiftCharge', oldNightShiftCharge', nightShiftStart', nightShiftEnd') ->
                Domain.NightShiftInfo
                  { nightShiftCharge = nightShiftCharge',
                    oldNightShiftCharge = oldNightShiftCharge',
                    nightShiftStart = nightShiftStart',
                    nightShiftEnd = nightShiftEnd'
                  },
          waitingCharges =
            Domain.WaitingCharges
              { ..
              },
          ..
        }

instance ToTType FullEstimateT Domain.Estimate where
  toTType Domain.Estimate {..} = do
    let estimateId = getId id
    let estimateT =
          SEstimate.EstimateT
            { id = estimateId,
              requestId = toKey requestId,
              bppEstimateId = getId bppEstimateId,
              providerUrl = showBaseUrl providerUrl,
              tripTermsId = toKey <$> (tripTerms <&> (.id)),
              estimatedFare = realToFrac estimatedFare,
              discount = realToFrac <$> discount,
              estimatedTotalFare = realToFrac estimatedTotalFare,
              minTotalFare = realToFrac totalFareRange.minFare,
              maxTotalFare = realToFrac totalFareRange.maxFare,
              nightShiftCharge = nightShiftInfo <&> (.nightShiftCharge),
              oldNightShiftCharge = nightShiftInfo <&> (.oldNightShiftCharge),
              nightShiftStart = nightShiftInfo <&> (.nightShiftStart),
              nightShiftEnd = nightShiftInfo <&> (.nightShiftEnd),
              driversLocation = PostgresList driversLocation,
              waitingChargePerMin = waitingCharges.waitingChargePerMin,
              status = status,
              ..
            }
    let mbTripTermsT = toTType <$> tripTerms
    let estimateBreakupLisTType = toTType <$> estimateBreakupList
    (estimateT, estimateBreakupLisTType, mbTripTermsT)
