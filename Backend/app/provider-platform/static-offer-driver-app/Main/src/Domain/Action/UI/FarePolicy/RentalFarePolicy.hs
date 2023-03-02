{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.FarePolicy.RentalFarePolicy
  ( ListRentalFarePoliciesRes (..),
    CreateRentalFarePolicyReq (..),
    CreateRentalFarePolicyItem (..),
    createRentalFarePolicy,
    listRentalFarePolicies,
  )
where

import Domain.Types.FarePolicy.RentalFarePolicy as Domain
import Domain.Types.Merchant
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Id (Id (..))
import Kernel.Types.Predicate
import Kernel.Utils.Validation
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.RentalFarePolicy as SRentalFarePolicy

newtype ListRentalFarePoliciesRes = ListRentalFarePoliciesRes
  { rentalFarePolicies :: [RentalFarePolicyAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype CreateRentalFarePolicyReq = CreateRentalFarePolicyReq
  { createList :: NonEmpty CreateRentalFarePolicyItem
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CreateRentalFarePolicyItem = CreateRentalFarePolicyItem
  { vehicleVariant :: Vehicle.Variant,
    baseFare :: Money,
    baseDistance :: Kilometers, -- Distance
    baseDuration :: Hours,
    extraKmFare :: HighPrecMoney,
    extraMinuteFare :: HighPrecMoney,
    driverAllowanceForDay :: Maybe Money
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

validateCreateRentalsFarePolicyRequest :: Validate CreateRentalFarePolicyItem
validateCreateRentalsFarePolicyRequest CreateRentalFarePolicyItem {..} =
  sequenceA_
    [ validateField "baseFare" baseFare $ Min @Money 0,
      validateField "baseDistance" baseDistance $ Min @Kilometers 0,
      validateField "baseDuration" baseDuration $ Min @Hours 0,
      validateField "extraKmFare" extraKmFare $ Min @HighPrecMoney 0,
      validateField "extraMinuteFare" extraMinuteFare $ Min @HighPrecMoney 0,
      validateField "driverAllowanceForDay" driverAllowanceForDay $ InMaybe $ Min @Money 0
    ]

createRentalFarePolicy :: forall m r. (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) => SP.Person -> CreateRentalFarePolicyReq -> m APISuccess
createRentalFarePolicy admin req = do
  let merchantId = admin.merchantId
  mapM_ (runRequestValidation validateCreateRentalsFarePolicyRequest) req.createList
  newRentalFarePolicyItems <- forM req.createList $ \createItemReq -> do
    guid <- Id <$> generateGUID
    pure $ toDomainType merchantId guid createItemReq
  Esq.runTransaction $ do
    SRentalFarePolicy.markAllAsDeleted @m merchantId
    forM_ newRentalFarePolicyItems SRentalFarePolicy.create
  SRentalFarePolicy.clearAllCacheByMerchantId merchantId
  pure Success
  where
    toDomainType :: Id Merchant -> Id RentalFarePolicy -> CreateRentalFarePolicyItem -> RentalFarePolicy
    toDomainType merchantId guid CreateRentalFarePolicyItem {..} = do
      let driverAllowanceForDay' = driverAllowanceForDay
      RentalFarePolicy
        { id = guid,
          merchantId = merchantId,
          baseFare = baseFare,
          driverAllowanceForDay = driverAllowanceForDay',
          descriptions = mkDescriptions extraKmFare extraMinuteFare driverAllowanceForDay',
          ..
        }

listRentalFarePolicies :: (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) => SP.Person -> m ListRentalFarePoliciesRes
listRentalFarePolicies person = do
  rentalFarePolicies <- SRentalFarePolicy.findAllByMerchantId person.merchantId
  pure $
    ListRentalFarePoliciesRes
      { rentalFarePolicies = map makeRentalFarePolicyAPIEntity rentalFarePolicies
      }
