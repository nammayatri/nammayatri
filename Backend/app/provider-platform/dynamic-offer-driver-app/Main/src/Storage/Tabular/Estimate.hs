{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Estimate where

import Data.Coerce (coerce)
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Estimate as Domain
import qualified Domain.Types.Vehicle as Variant
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Storage.Tabular.Vehicle ()
import Tools.Error

mkPersist
  defaultSqlSettings
  [defaultQQ|
    EstimateT sql=estimate
      id Text
      transactionId Text
      vehicleVariant Variant.Variant
      minFare Money
      maxFare Money
      estimateBreakupList (PostgresList Text)
      nightShiftCharge Money Maybe
      oldNightShiftCharge Centesimal Maybe sql=night_shift_multiplier
      nightShiftStart TimeOfDay Maybe
      nightShiftEnd TimeOfDay Maybe
      waitingChargePerMin Money Maybe
      waitingOrPickupCharges Money Maybe
      createdAt UTCTime

      Primary id
      deriving Generic
    |]

instance TEntityKey EstimateT where
  type DomainKey EstimateT = Id Domain.Estimate
  fromKey (EstimateTKey _id) = Id _id
  toKey (Id id) = EstimateTKey id

instance FromTType EstimateT Domain.Estimate where
  fromTType EstimateT {..} = do
    let nightShiftInfo =
          ((,,,) <$> nightShiftCharge <*> oldNightShiftCharge <*> nightShiftStart <*> nightShiftEnd)
            <&> \(nightShiftCharge', oldNightShiftCharge', nightShiftStart', nightShiftEnd') ->
              Domain.NightShiftInfo
                { nightShiftCharge = nightShiftCharge',
                  oldNightShiftCharge = oldNightShiftCharge', -- TODO: To be removed
                  nightShiftStart = nightShiftStart',
                  nightShiftEnd = nightShiftEnd'
                }
        waitingCharges = Domain.WaitingCharges {..}
    estimateBreakupListDec <- (decodeFromText `mapM` unPostgresList estimateBreakupList) & fromMaybeM (InternalError "Unable to decode EstimateBreakup")
    return $
      Domain.Estimate
        { id = Id id,
          estimateBreakupList = coerce @[Domain.EstimateBreakupD 'Unsafe] @[Domain.EstimateBreakup] $ estimateBreakupListDec,
          ..
        }

instance ToTType EstimateT Domain.Estimate where
  toTType Domain.Estimate {..} = do
    let Domain.WaitingCharges {..} = waitingCharges
        unsafeEstimateBreakupList = coerce @[Domain.EstimateBreakup] @[Domain.EstimateBreakupD 'Unsafe] $ estimateBreakupList
    EstimateT
      { id = getId id,
        estimateBreakupList = PostgresList $ encodeToText <$> unsafeEstimateBreakupList,
        nightShiftCharge = nightShiftInfo <&> (.nightShiftCharge),
        oldNightShiftCharge = nightShiftInfo <&> (.oldNightShiftCharge),
        nightShiftStart = nightShiftInfo <&> (.nightShiftStart),
        nightShiftEnd = nightShiftInfo <&> (.nightShiftEnd),
        ..
      }
