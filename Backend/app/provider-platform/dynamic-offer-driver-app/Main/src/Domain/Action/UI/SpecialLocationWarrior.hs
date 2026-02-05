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
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, logDebug, throwError)
import qualified Lib.Queries.SpecialLocation
import qualified Lib.Queries.SpecialLocation as SpecialLocation
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Transformers.DriverInformation as TDI
import Tools.Error

getSpecialLocationListCategory ::
  ( ( Kernel.Prelude.Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Text.Text ->
    Environment.Flow [Lib.Queries.SpecialLocation.SpecialLocationWarrior]
  )
getSpecialLocationListCategory (_, _, merchanOperatingCityId) category = do
  specialLocations <- SpecialLocation.findSpecialLocationsWarriorByMerchantOperatingCityId merchanOperatingCityId.getId category
  mapM SpecialLocation.specialLocToSpecialLocWarrior (filter (notNull . (.gates)) specialLocations)

getGetInfoSpecialLocWarrior ::
  ( ( Kernel.Prelude.Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Id Domain.Types.Person.Person ->
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
  ( ( Kernel.Prelude.Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Id Domain.Types.Person.Person ->
    API.Types.UI.SpecialLocationWarrior.SpecialLocWarriorInfoReq ->
    Environment.Flow API.Types.UI.SpecialLocationWarrior.SpecialLocWarriorInfoRes
  )
postUpdateInfoSpecialLocWarrior (_, _, merchantOperatingCityId) personId SpecialLocWarriorInfoReq {..} = do
  driver <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  logDebug $ "Driver Tag driver:preferredPrimarySpecialLocId ----------" <> personId.getId <> "  " <> show preferredPrimarySpecialLocId
  driverInfo <- QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  let preferredPrimarySpecialLocationId = preferredPrimarySpecialLocId <|> driverInfo.preferredPrimarySpecialLocId
  preferredPrimarySpecialLocation <- maybe (return Nothing) (\locId -> TDI.getPreferredPrimarySpecialLoc (Just locId.getId)) preferredPrimarySpecialLocationId
  when (isSpecialLocWarrior && isNothing preferredPrimarySpecialLocation) $ throwError (InvalidRequest "preferredPrimarySpecialLoc is required when isSpecialLocWarrior is true")
  metroWarriorTagValidity <- Yudhishthira.fetchNammaTagValidity (cast merchantOperatingCityId) $ LYT.TagName "MetroWarrior"
  now <- getCurrentTime
  mbOlderDriverTag <- runMaybeT $ do
    preferredPrimarySpecialLocationId' <- MaybeT $ pure driverInfo.preferredPrimarySpecialLocId
    pure $ getDriverTag preferredPrimarySpecialLocationId' driverInfo.preferredSecondarySpecialLocIds metroWarriorTagValidity now
  let enabledAt = if isSpecialLocWarrior then Just now else Nothing
  QDI.updateSpecialLocWarriorInfo isSpecialLocWarrior preferredPrimarySpecialLocationId preferredSecondarySpecialLocIds enabledAt personId

  mbDriverTag <- runMaybeT $ do
    preferredPrimarySpecialLocationId' <- MaybeT $ pure preferredPrimarySpecialLocationId
    pure $ getDriverTag preferredPrimarySpecialLocationId' preferredSecondarySpecialLocIds metroWarriorTagValidity now
  whenJust mbDriverTag $ \driverTag -> do
    if isSpecialLocWarrior
      then unless (maybe False (Yudhishthira.elemTagNameValue driverTag) driver.driverTag) $ do
        logDebug $ "Driver Tag driverTag: ----------" <> personId.getId <> "  " <> show driverTag
        whenJust mbOlderDriverTag $ \olderDriverTag -> do
          logDebug $ "Driver Tag olderDriverTag: ----------" <> personId.getId <> "  " <> show olderDriverTag
          logDebug $ "Driver Tag removeDriverTag: ----------" <> personId.getId <> "  " <> show (Yudhishthira.removeTagName driver.driverTag olderDriverTag)
        let updatedTag = Yudhishthira.replaceTagNameValue driver.driverTag driverTag
        QPerson.updateDriverTag (Just updatedTag) personId
        logDebug $ "Driver Tag addDriverTag: ----------" <> personId.getId <> "  " <> show (Yudhishthira.replaceTagNameValue driver.driverTag driverTag)
      else when (maybe False (Yudhishthira.elemTagName driverTag) driver.driverTag) $ QPerson.updateDriverTag (Just $ Yudhishthira.removeTagName driver.driverTag driverTag) personId
  return $
    SpecialLocWarriorInfoRes
      { isSpecialLocWarrior = isSpecialLocWarrior,
        preferredPrimarySpecialLoc = preferredPrimarySpecialLocation,
        preferredSecondarySpecialLocIds = preferredSecondarySpecialLocIds
      }
  where
    getDriverTag prefPrimarySpecialLocationId prefSecondarySpecialLocIds metroWarriorTagValidity now = do
      let tagNameValue =
            if notNull prefSecondarySpecialLocIds
              then do
                let secondaryLocTag = makeDriverTag (map (.getId) prefSecondarySpecialLocIds)
                LYT.TagNameValue $ "MetroWarrior#" <> prefPrimarySpecialLocationId.getId <> "&" <> secondaryLocTag
              else LYT.TagNameValue $ "MetroWarrior#" <> prefPrimarySpecialLocationId.getId
      Yudhishthira.addTagExpiry tagNameValue metroWarriorTagValidity now

makeDriverTag :: [Text] -> Text
makeDriverTag = T.intercalate "&"
