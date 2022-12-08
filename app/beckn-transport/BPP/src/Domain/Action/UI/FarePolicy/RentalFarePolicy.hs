{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.FarePolicy.RentalFarePolicy
  ( ListRentalFarePoliciesRes (..),
    CreateRentalFarePolicyReq (..),
    CreateRentalFarePolicyItem (..),
    createRentalFarePolicy,
    listRentalFarePolicies,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.APISuccess
import Beckn.Types.Common
import Beckn.Types.Id (Id (..))
import Beckn.Types.Predicate
import Beckn.Utils.Validation
import Domain.Types.FarePolicy.RentalFarePolicy as Domain
import Domain.Types.Merchant
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as Vehicle
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

createRentalFarePolicy :: (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) => SP.Person -> CreateRentalFarePolicyReq -> m APISuccess
createRentalFarePolicy admin req = do
  let merchantId = admin.merchantId
  mapM_ (runRequestValidation validateCreateRentalsFarePolicyRequest) req.createList
  newRentalFarePolicyItems <- forM req.createList $ \createItemReq -> do
    guid <- Id <$> generateGUID
    pure $ toDomainType merchantId guid createItemReq
  Esq.runTransaction $ do
    SRentalFarePolicy.markAllAsDeleted merchantId
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
