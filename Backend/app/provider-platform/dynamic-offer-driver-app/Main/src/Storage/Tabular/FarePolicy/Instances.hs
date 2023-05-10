{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE.See the GNU Affero General Public License for more details.You should have received a copy of

 the GNU Affero General Public License along with this program.If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.FarePolicy.Instances where

import qualified Domain.Types.FarePolicy as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.FarePolicy.CustomerExtraFeeBounds (CustomerExtraFeeBoundsT, FullCustomerExtraFeeBounds)
import Storage.Tabular.FarePolicy.DriverExtraFeeBounds (DriverExtraFeeBoundsT, FullDriverExtraFeeBounds)
import Storage.Tabular.FarePolicy.FarePolicyProgressiveDetails
import Storage.Tabular.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab
import Storage.Tabular.FarePolicy.Table
import Storage.Tabular.Vehicle ()
import Tools.Error

type FullFarePolicyT = (FarePolicyT, [CustomerExtraFeeBoundsT], [DriverExtraFeeBoundsT], FarePolicyDetailsT)

data FarePolicyDetailsT = ProgressiveDetailsT FullFarePolicyProgressiveDetailsT | SlabsDetailsT [FarePolicySlabsDetailsSlabT]

instance FromTType FullFarePolicyT Domain.FarePolicy where
  fromTType (FarePolicyT {..}, customerExtraFareBoundsTList, driverExtraFareBoundsTList, farePolicyDetails) = do
    customerExtraFeeBounds <- do
      customerExtraFeeBoundsList <- fromTType @_ @FullCustomerExtraFeeBounds `mapM` customerExtraFareBoundsTList
      case customerExtraFeeBoundsList of
        [] -> return Nothing
        a : xs -> do
          unless (all (\(farePolicyId, _) -> Id id == farePolicyId) customerExtraFeeBoundsList) $
            throwError (InternalError "Unable to decode FullDriverExtraFeeBounds of FarePolicy. Fare policy ids are not the same.")
          unless (any (\(_, customerExtraFareBounds) -> customerExtraFareBounds.startDistance <= 0) customerExtraFeeBoundsList) $
            throwError (InternalError "Unable to decode FullCustomerExtraFeeBounds of  FarePolicy. At least one slab must have startDistance <= 0")
          return . Just . fmap (._2) $ a :| xs
    driverExtraFeeBounds <- do
      driverExtraFeeBoundsList <- fromTType @_ @FullDriverExtraFeeBounds `mapM` driverExtraFareBoundsTList
      case driverExtraFeeBoundsList of
        [] -> return Nothing
        a : xs -> do
          unless (all (\(farePolicyId, _) -> Id id == farePolicyId) driverExtraFeeBoundsList) $
            throwError (InternalError "Unable to decode FullDriverExtraFeeBounds of FarePolicy. Fare policy ids are not the same.")
          unless (any (\(_, driverExtraFareBounds) -> driverExtraFareBounds.startDistance <= 0) driverExtraFeeBoundsList) $
            throwError (InternalError "Unable to decode FullDriverExtraFeeBounds of  FarePolicy. At least one slab must have startDistance <= 0")
          return . Just . fmap (._2) $ a :| xs
    det <- case farePolicyDetails of
      ProgressiveDetailsT detT -> do
        (farePolicyId, det) <- fromTType @_ @FullFarePolicyProgressiveDetails detT
        unless (farePolicyId == Id id) $ throwError (InternalError "Unable to decode progressive FarePolicy. Fare policy ids are not the same.")
        return $ Domain.ProgressiveDetails det
      SlabsDetailsT det -> do
        nonEmptySlabsT <- case det of
          [] -> throwError (InternalError "Unable to decode slab FarePolicy. Slabs list is emtpy.")
          a : xs -> return $ a :| xs
        slabsFullDTypes :: NonEmpty FullFarePolicySlabsDetailsSlab <- fromTType @_ @FullFarePolicySlabsDetailsSlab `mapM` nonEmptySlabsT
        unless (all (\(farePolicyId, _) -> Id id == farePolicyId) slabsFullDTypes) $
          throwError (InternalError "Unable to decode slab FarePolicy. Fare policy ids are not the same.")
        unless (any (\(_, farePolicySlab) -> farePolicySlab.startDistance <= 0) slabsFullDTypes) $
          throwError (InternalError "Unable to decode slab FarePolicy. At least one slab must have startDistance <= 0")
        return . Domain.SlabsDetails . Domain.FPSlabsDetails $ slabsFullDTypes <&> (._2)
    let nightShiftBounds =
          ((,) <$> nightShiftStart <*> nightShiftEnd) <&> \(nightShiftStart', nightShiftEnd') ->
            Domain.NightShiftBounds
              { nightShiftStart = nightShiftStart',
                nightShiftEnd = nightShiftEnd'
              }
        allowedTripDistanceBounds =
          ((,) <$> minAllowedTripDistance <*> maxAllowedTripDistance) <&> \(minAllowedTripDistance', maxAllowedTripDistance') ->
            Domain.AllowedTripDistanceBounds
              { minAllowedTripDistance = minAllowedTripDistance',
                maxAllowedTripDistance = maxAllowedTripDistance'
              }
    return $
      Domain.FarePolicy
        { id = Id id,
          merchantId = fromKey merchantId,
          farePolicyDetails = det,
          ..
        }

instance ToTType FullFarePolicyT Domain.FarePolicy where
  toTType farePolicy@Domain.FarePolicy {..} = do
    let detT = case farePolicyDetails of
          Domain.ProgressiveDetails det -> ProgressiveDetailsT $ toTType (id, det)
          Domain.SlabsDetails det -> SlabsDetailsT $ toTType . (id,) <$> toList det.slabs
    ( FarePolicyT
        { id = getId id,
          merchantId = toKey merchantId,
          farePolicyType = Domain.getFarePolicyType farePolicy,
          nightShiftStart = nightShiftBounds <&> (.nightShiftStart),
          nightShiftEnd = nightShiftBounds <&> (.nightShiftEnd),
          maxAllowedTripDistance = allowedTripDistanceBounds <&> (.maxAllowedTripDistance),
          minAllowedTripDistance = allowedTripDistanceBounds <&> (.minAllowedTripDistance),
          ..
        },
      maybe [] (fmap (toTType . (id,)) . toList) customerExtraFeeBounds,
      maybe [] (fmap (toTType . (id,)) . toList) driverExtraFeeBounds,
      detT
      )
