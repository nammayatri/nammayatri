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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.PlanDetails where

import Control.Monad
import qualified Domain.Types.PlanDetails as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (Money)
import Kernel.Types.Id
import Kernel.Utils.Common (decodeFromText, encodeToText)
import Kernel.Utils.Error
import Storage.Tabular.Merchant (MerchantTId)
import Tools.Error

derivePersistField "Domain.PaymentType"
derivePersistField "Domain.PlanType"
derivePersistField "Domain.Frequency"
derivePersistField "Domain.PlanStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PlanDetailsT sql=plan_details
      id Text
      paymentType Domain.PaymentType
      merchantId MerchantTId
      name Text
      description Text
      maxAmount Money
      isOfferApplicable Bool
      maxCreditLimit Money
      rideCountBasedFeePolicyJSON Text sql=ride_count_based_fee_policy_json
   -- distanceBasedFeePolicy :: , -- todo
      freeRideCount Int
      frequency Domain.Frequency
      planType Domain.PlanType
      Primary id paymentType
      deriving Generic
    |]

instance TEntityKey PlanDetailsT where
  type DomainKey PlanDetailsT = (Id Domain.PlanDetails, Domain.PaymentType)
  fromKey (PlanDetailsTKey _id paymentType) = (Id _id, paymentType)
  toKey (Id id, paymentType) = PlanDetailsTKey id paymentType

instance FromTType PlanDetailsT Domain.PlanDetails where
  fromTType PlanDetailsT {..} = do
    rideCountBasedFeePolicy <-
      maybe (throwError $ InternalError "Unable to decode PlanDetailsT.planCriteriaConfig") (return . Domain.RideCountBasedFeePolicyConfig) (decodeFromText rideCountBasedFeePolicyJSON)
    return $
      Domain.PlanDetails
        { id = Id id,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType PlanDetailsT Domain.PlanDetails where
  toTType Domain.PlanDetails {..} = do
    let rideCountBasedFeePolicyJSON = getConfigJSON rideCountBasedFeePolicy
    PlanDetailsT
      { id = getId id,
        merchantId = toKey merchantId,
        ..
      }

getConfigJSON :: Domain.RideCountBasedFeePolicyConfig -> Text
getConfigJSON = \case
  Domain.RideCountBasedFeePolicyConfig cfg -> encodeToText cfg
