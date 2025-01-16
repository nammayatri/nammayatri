module Domain.Action.UI.SpecialLocationWarrior
  ( getSpecialLocationListCategory,
    getGetInfoSpecialLocWarrior,
    postUpdateInfoSpecialLocWarrior,
  )
where

import API.Types.UI.SpecialLocationWarrior
import Data.List.Extra (notNull)
import qualified Data.Text
import qualified Data.Text as T
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (elem, filter, id, map, whenJust)
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, logDebug, throwError)
import qualified Lib.Queries.SpecialLocation
import qualified Lib.Queries.SpecialLocation as SpecialLocation
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Transformers.DriverInformation as TDI
import Tools.Error

getSpecialLocationListCategory ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.Flow [Lib.Queries.SpecialLocation.SpecialLocationWarrior]
  )
getSpecialLocationListCategory (_, _, merchanOperatingCityId) category = do
  specialLocations <- SpecialLocation.findSpecialLocationsWarriorByMerchantOperatingCityId merchanOperatingCityId.getId category
  mapM SpecialLocation.specialLocToSpecialLocWarrior (filter (notNull . (.gates)) specialLocations)

getGetInfoSpecialLocWarrior ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Person.Person ->
    Environment.Flow API.Types.UI.SpecialLocationWarrior.SpecialLocWarriorInfoRes
  )
getGetInfoSpecialLocWarrior (_, _, _merchantOperatingCityId) personId = do
  DI.DriverInformation {..} <- QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  preferredPrimarySpecialLoc <- maybe (return Nothing) (\locId -> TDI.getPreferredPrimarySpecialLoc (Just locId.getId)) preferredPrimarySpecialLocId
  return $
    SpecialLocWarriorInfoRes
      { ..
      }

postUpdateInfoSpecialLocWarrior ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Types.Id.Id Domain.Types.Person.Person ->
    API.Types.UI.SpecialLocationWarrior.SpecialLocWarriorInfoReq ->
    Environment.Flow API.Types.UI.SpecialLocationWarrior.SpecialLocWarriorInfoRes
  )
postUpdateInfoSpecialLocWarrior (_, _, _merchantOperatingCityId) personId SpecialLocWarriorInfoReq {..} = do
  driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  logDebug $ "Driver Tag driver:preferredPrimarySpecialLocId ----------" <> personId.getId <> "  " <> show preferredPrimarySpecialLocId
  driverInfo <- QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  let preferredPrimarySpecialLocationId = preferredPrimarySpecialLocId <|> driverInfo.preferredPrimarySpecialLocId
  preferredPrimarySpecialLocation <- maybe (return Nothing) (\locId -> TDI.getPreferredPrimarySpecialLoc (Just locId.getId)) preferredPrimarySpecialLocationId
  when (isSpecialLocWarrior && isNothing preferredPrimarySpecialLocation) $ throwError (InvalidRequest "preferredPrimarySpecialLoc is required when isSpecialLocWarrior is true")
  mbOlderDriverTag <- runMaybeT $ do
    preferredPrimarySpecialLocationId' <- MaybeT $ pure driverInfo.preferredPrimarySpecialLocId
    getDriverTag preferredPrimarySpecialLocationId' driverInfo.preferredSecondarySpecialLocIds
  now <- getCurrentTime
  let enabledAt = if isSpecialLocWarrior then Just now else Nothing
  QDI.updateSpecialLocWarriorInfo isSpecialLocWarrior preferredPrimarySpecialLocationId preferredSecondarySpecialLocIds enabledAt personId

  mbDriverTag <- runMaybeT $ do
    preferredPrimarySpecialLocationId' <- MaybeT $ pure preferredPrimarySpecialLocationId
    getDriverTag preferredPrimarySpecialLocationId' preferredSecondarySpecialLocIds
  whenJust mbDriverTag $ \driverTag -> do
    if isSpecialLocWarrior
      then unless (maybe False (elem driverTag) driver.driverTag) $ do
        logDebug $ "Driver Tag driverTag: ----------" <> personId.getId <> "  " <> show driverTag
        whenJust mbOlderDriverTag $ \olderDriverTag -> do
          let updatedTag = addDriverTag (Just $ removeDriverTag driver.driverTag olderDriverTag) driverTag
          logDebug $ "Driver Tag olderDriverTag: ----------" <> personId.getId <> "  " <> show olderDriverTag
          logDebug $ "Driver Tag removeDriverTag: ----------" <> personId.getId <> "  " <> show (removeDriverTag driver.driverTag olderDriverTag)
          QPerson.updateDriverTag (Just updatedTag) personId
        whenNothing_ mbOlderDriverTag $ QPerson.updateDriverTag (Just $ addDriverTag driver.driverTag driverTag) personId
        logDebug $ "Driver Tag addDriverTag: ----------" <> personId.getId <> "  " <> show (addDriverTag driver.driverTag driverTag)
      else when (maybe False (elem driverTag) driver.driverTag) $ QPerson.updateDriverTag (Just $ removeDriverTag driver.driverTag driverTag) personId
  return $
    SpecialLocWarriorInfoRes
      { isSpecialLocWarrior = isSpecialLocWarrior,
        preferredPrimarySpecialLoc = preferredPrimarySpecialLocation,
        preferredSecondarySpecialLocIds = preferredSecondarySpecialLocIds
      }
  where
    getDriverTag prefPrimarySpecialLocationId prefSecondarySpecialLocIds =
      if notNull prefSecondarySpecialLocIds
        then do
          let secondaryLocTag = makeDriverTag (map (.getId) prefSecondarySpecialLocIds)
          pure $ "MetroWarrior#" <> prefPrimarySpecialLocationId.getId <> "&" <> secondaryLocTag
        else pure $ "MetroWarrior#" <> prefPrimarySpecialLocationId.getId

makeDriverTag :: [Text] -> Text
makeDriverTag = T.intercalate "&"

addDriverTag :: Maybe [Text] -> Text -> [Text]
addDriverTag Nothing tag = [tag]
addDriverTag (Just tags) tag = tags ++ [tag]

removeDriverTag :: Maybe [Text] -> Text -> [Text]
removeDriverTag Nothing _ = []
removeDriverTag (Just tags) tag = filter (/= tag) tags
