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

module Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails where

import qualified Domain.Types.FarePolicy as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection (FarePolicyProgressiveDetailsPerExtraKmRateSectionT, FullFarePolicyProgressiveDetailsPerExtraKmRateSection)
import Storage.Tabular.FarePolicy.Table (FarePolicyTId)
import Tools.Error

mkPersist
  defaultSqlSettings
  [defaultQQ|
    FarePolicyProgressiveDetailsT sql=fare_policy_progressive_details
      farePolicyId FarePolicyTId
      baseDistance Meters
      baseFare HighPrecMoney
      deadKmFare HighPrecMoney
      waitingCharge Domain.WaitingCharge Maybe
      freeWatingTime Minutes Maybe
      nightShiftCharge Domain.NightShiftCharge Maybe

      Primary farePolicyId
      deriving Generic
    |]

instance TEntityKey FarePolicyProgressiveDetailsT where
  type DomainKey FarePolicyProgressiveDetailsT = Id Domain.FarePolicy
  fromKey (FarePolicyProgressiveDetailsTKey _id) = fromKey _id
  toKey id = FarePolicyProgressiveDetailsTKey $ toKey id

type FullFarePolicyProgressiveDetails = (Id Domain.FarePolicy, Domain.FPProgressiveDetails)

type FullFarePolicyProgressiveDetailsT = (FarePolicyProgressiveDetailsT, [FarePolicyProgressiveDetailsPerExtraKmRateSectionT])

instance FromTType FullFarePolicyProgressiveDetailsT FullFarePolicyProgressiveDetails where
  fromTType (FarePolicyProgressiveDetailsT {..}, perExtraKmRateSectionsT) = do
    nonEmptyPerExtraKmFareSectionsT <- case perExtraKmRateSectionsT of
      [] -> throwError (InternalError "Unable to decode progressive FarePolicy details. PerExtraKmFareSections list is emtpy.")
      a : xs -> return $ a :| xs
    perExtraKmRateSectionsFullDTypes <- fromTType @_ @FullFarePolicyProgressiveDetailsPerExtraKmRateSection `mapM` nonEmptyPerExtraKmFareSectionsT
    unless (all (\(farePolicyId', _) -> fromKey farePolicyId == farePolicyId') perExtraKmRateSectionsFullDTypes) $
      throwError (InternalError "Unable to decode progressive FarePolicy details. Fare policy ids are not the same.")
    unless (any (\(_, farePolicySlab) -> farePolicySlab.startDistance <= 0) perExtraKmRateSectionsFullDTypes) $
      throwError (InternalError "Unable to decode progressive FarePolicy details. At least one slab must have startDistance <= 0")

    let waitingChargeInfo =
          ((,) <$> waitingCharge <*> freeWatingTime) <&> \(waitingCharge', freeWaitingTime') ->
            Domain.WaitingChargeInfo
              { waitingCharge = waitingCharge',
                freeWaitingTime = freeWaitingTime'
              }
    return
      ( fromKey farePolicyId,
        Domain.FPProgressiveDetails
          { perExtraKmRateSections = perExtraKmRateSectionsFullDTypes <&> (._2),
            ..
          }
      )

instance ToTType FullFarePolicyProgressiveDetailsT FullFarePolicyProgressiveDetails where
  toTType (farePolicyId, Domain.FPProgressiveDetails {..}) = do
    ( FarePolicyProgressiveDetailsT
        { farePolicyId = toKey farePolicyId,
          waitingCharge = waitingChargeInfo <&> (.waitingCharge),
          freeWatingTime = waitingChargeInfo <&> (.freeWaitingTime),
          ..
        },
      toTType . (farePolicyId,) <$> toList perExtraKmRateSections
      )
