{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.FleetOwnerList (getFleetOwnerList) where

import qualified API.Types.UI.FleetOwnerList as API
import Control.Monad.Extra (mapMaybeM)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Domain.Types.FleetOwnerInformation as FOI
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id as Id
import Servant
import qualified Storage.Queries.FleetOwnerInformation as QFleetOwnerInfo
import qualified Storage.Queries.PersonExtra as QPerson
import Tools.Auth

getFleetOwnerList ::
  ( ( Kernel.Prelude.Maybe (Id.Id Person.Person),
      Id.Id Domain.Types.Merchant.Merchant,
      Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe FOI.FleetType ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.Flow [API.FleetOwnerListItem]
  )
getFleetOwnerList (_mbPersonId, _, defaultOpCityId) mbBlocked mbFleetType mbLimit mbOffset mbOnlyEnabled = do
  let normalizedLimit = Just $ clampLimit $ fromMaybe 10 mbLimit
      normalizedOffset = Just $ max 0 $ fromMaybe 0 mbOffset
      targetOpCity = defaultOpCityId
  fleetOwners <- QFleetOwnerInfo.findFleetOwners targetOpCity mbFleetType mbOnlyEnabled mbBlocked normalizedLimit normalizedOffset
  persons <- QPerson.findAllByPersonIds (Id.getId . (.fleetOwnerPersonId) <$> fleetOwners)
  let personMap = HM.fromList $ (\p -> (p.id, p)) <$> persons
  mapMaybeM (toApiItem personMap targetOpCity) fleetOwners
  where
    clampLimit :: Int -> Int
    clampLimit = max 1 . min 10
    toApiItem personMap fallbackOpCityId fleetOwnerInfo = do
      case HM.lookup fleetOwnerInfo.fleetOwnerPersonId personMap of
        Nothing -> pure Nothing
        Just person -> do
          mobileNumber <- mapM decrypt person.mobileNumber
          let merchantOpCityId = fromMaybe fallbackOpCityId fleetOwnerInfo.merchantOperatingCityId
          pure $
            Just $
              API.FleetOwnerListItem
                { fleetOwnerId = fleetOwnerInfo.fleetOwnerPersonId,
                  fleetOwnerName = personName person,
                  merchantId = fleetOwnerInfo.merchantId,
                  merchantOperatingCityId = merchantOpCityId,
                  fleetType = fleetOwnerInfo.fleetType,
                  enabled = fleetOwnerInfo.enabled,
                  verified = fleetOwnerInfo.verified,
                  blocked = fleetOwnerInfo.blocked,
                  isEligibleForSubscription = fleetOwnerInfo.isEligibleForSubscription,
                  address = fleetOwnerInfo.stripeAddress,
                  fleetName = fleetOwnerInfo.fleetName,
                  mobileNumber = mobileNumber
                }

    personName person@Person.Person {..} =
      let parts = filter (not . T.null) $ catMaybes [Just firstName, middleName, lastName]
       in if null parts then Id.getId person.id else T.intercalate " " parts
