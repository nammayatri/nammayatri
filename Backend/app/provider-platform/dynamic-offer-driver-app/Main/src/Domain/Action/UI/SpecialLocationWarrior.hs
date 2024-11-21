{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.SpecialLocationWarrior
  ( getSpecialLocationListCategory,
    getGetInfoSpecialLocWarrior,
    postUpdateInfoSpecialLocWarrior,
  )
where

import API.Types.UI.SpecialLocationWarrior
import Data.List.Extra (notNull)
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Data.Text as T
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (elem, filter, id, map, whenJust)
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, logDebug, throwError)
import qualified Lib.Queries.SpecialLocation
import qualified Lib.Queries.SpecialLocation as SpecialLocation
import Servant hiding (throwError)
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Transformers.DriverInformation as TDI
import Tools.Auth
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
  logDebug $ "Driver Tag driverInfo: ----------" <> personId.getId <> "  " <> show driverInfo
  logDebug $ "Driver Tag driver:driverTag ----------" <> personId.getId <> "  " <> show driver.driverTag
  primarySpecialLocation <-
    maybe
      (pure Nothing)
      ( \locId -> do
          loc <- TDI.getPreferredPrimarySpecialLoc (Just locId.getId)
          logDebug $ "Driver Tag TDI.getPreferredPrimarySpecialLoc': ----------" <> personId.getId <> "  " <> show loc
          return loc
      )
      preferredPrimarySpecialLocId
  -- primarySpecialLocation <- runMaybeT $ do
  --   preferredPrimarySpecialLocId' <- MaybeT $ pure preferredPrimarySpecialLocId
  --   logDebug $ "Driver Tag preferredPrimarySpecialLocId': ----------" <> personId.getId <> "  " <> show preferredPrimarySpecialLocId'
  --   loc <- MaybeT $ TDI.getPreferredPrimarySpecialLoc (Just preferredPrimarySpecialLocId'.getId) -- >>= fromMaybeM (InvalidRequest $ "SpecialLocWarrior Loc not found with id : " <> preferredPrimarySpecialLocId'.getId)
  --   logDebug $ "Driver Tag TDI.getPreferredPrimarySpecialLoc': ----------" <> personId.getId <> "  " <> show loc
  --   pure loc
  logDebug $ "Driver Tag primarySpecialLocation': ----------" <> personId.getId <> "  " <> show primarySpecialLocation
  let preferredPrimarySpecialLocation = primarySpecialLocation <|> driverInfo.preferredPrimarySpecialLoc
  logDebug $ "Driver Tag primarySpecialLocation': ----------" <> personId.getId <> "  " <> show primarySpecialLocation
  logDebug $ "Driver Tag primarySpecialLocationsssssss': ----------" <> personId.getId <> "  " <> show primarySpecialLocation

  when (isSpecialLocWarrior && isNothing preferredPrimarySpecialLocation) $ throwError (InvalidRequest "preferredPrimarySpecialLoc is required when isSpecialLocWarrior is true")
  mbOlderDriverTag <- runMaybeT $ do
    preferredPrimarySpecialLocation' <- MaybeT $ pure driverInfo.preferredPrimarySpecialLoc
    getDriverTag preferredPrimarySpecialLocation' driverInfo.preferredSecondarySpecialLocIds
  logDebug $ "Driver Tag mbOlderDriverTag: ----------" <> personId.getId <> "  " <> show mbOlderDriverTag
  logDebug $ "Driver Tag mbOlderDriverTagssssssssssssss: ----------" <> personId.getId <> "  " <> show mbOlderDriverTag
  QDI.updateSpecialLocWarriorInfo isSpecialLocWarrior preferredPrimarySpecialLocation preferredSecondarySpecialLocIds personId

  mbDriverTag <- runMaybeT $ do
    preferredPrimarySpecialLocation' <- MaybeT $ pure preferredPrimarySpecialLocation
    getDriverTag preferredPrimarySpecialLocation' preferredSecondarySpecialLocIds
  logDebug $ "Driver Tag mbDriverTag: ----------" <> personId.getId <> "  " <> show mbDriverTag
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
    getDriverTag prefPrimarySpecialLocation prefSecondarySpecialLocIds =
      if notNull prefSecondarySpecialLocIds
        then do
          let secondaryLocTag = makeDriverTag (map (.getId) prefSecondarySpecialLocIds)
          pure $ "MetroWarrior#" <> prefPrimarySpecialLocation.id.getId <> "&" <> secondaryLocTag
        else pure $ "MetroWarrior#" <> prefPrimarySpecialLocation.id.getId

makeDriverTag :: [Text] -> Text
makeDriverTag = T.intercalate "&"

addDriverTag :: Maybe [Text] -> Text -> [Text]
addDriverTag Nothing tag = [tag]
addDriverTag (Just tags) tag = tags ++ [tag]

removeDriverTag :: Maybe [Text] -> Text -> [Text]
removeDriverTag Nothing _ = []
removeDriverTag (Just tags) tag = filter (/= tag) tags
