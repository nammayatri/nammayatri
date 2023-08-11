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

module Storage.Tabular.Plan where

import Control.Monad
import qualified Domain.Types.Plan as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common hiding (id)
import Storage.Tabular.Merchant (MerchantTId)

derivePersistField "Domain.PaymentMode"
derivePersistField "Domain.Frequency"
derivePersistField "Domain.PlanType"
derivePersistField "Domain.PlanBaseAmount"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    PlanT sql=plan
      id Text
      paymentMode Domain.PaymentMode
      merchantId MerchantTId
      name Text
      description Text
      maxAmount HighPrecMoney
      registrationAmount HighPrecMoney
      isOfferApplicable Bool
      maxCreditLimit HighPrecMoney
      planBaseAmount Domain.PlanBaseAmount
   -- rideCountBasedFeePolicyJSON Text sql=ride_count_based_fee_policy_json
   -- distanceBasedFeePolicy :: , -- todo
      freeRideCount Int
      frequency Domain.Frequency
      planType Domain.PlanType
      Primary id
      deriving Generic
    |]

instance TEntityKey PlanT where
  type DomainKey PlanT = Id Domain.Plan
  fromKey (PlanTKey _id) = Id _id
  toKey (Id id) = PlanTKey id

instance FromTType PlanT Domain.Plan where
  fromTType PlanT {..} = do
    -- rideCountBasedFeePolicy <-
    --   maybe (throwError $ InternalError "Unable to decode PlanT.planCriteriaConfig") (return . Domain.RideCountBasedFeePolicyConfig) (decodeFromText rideCountBasedFeePolicyJSON)
    return $
      Domain.Plan
        { id = Id id,
          merchantId = fromKey merchantId,
          ..
        }

instance ToTType PlanT Domain.Plan where
  toTType Domain.Plan {..} = do
    -- let rideCountBasedFeePolicyJSON = getConfigJSON rideCountBasedFeePolicy
    PlanT
      { id = getId id,
        merchantId = toKey merchantId,
        ..
      }

getConfigJSON :: Domain.RideCountBasedFeePolicyConfig -> Text
getConfigJSON = \case
  Domain.RideCountBasedFeePolicyConfig cfg -> encodeToText cfg
