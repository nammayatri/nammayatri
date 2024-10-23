{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.SpecialLocationWarrior
  ( getSpecialLocationListCategory,
    getGetInfoSpecialLocWarrior,
    postUpdateInfoSpecialLocWarrior,
  )
where

import API.Types.UI.SpecialLocationWarrior
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (filter, id)
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, throwError)
import qualified Lib.Queries.SpecialLocation
import qualified Lib.Queries.SpecialLocation as SpecialLocation
import Servant hiding (throwError)
import qualified Storage.Queries.DriverInformation as QDI
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
  mapM SpecialLocation.specialLocToSpecialLocWarrior specialLocations

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
  driverInfo <- QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  primarySpecialLocation <- runMaybeT $ do
    preferredPrimarySpecialLocId' <- MaybeT $ pure preferredPrimarySpecialLocId
    MaybeT $ TDI.getPreferredPrimarySpecialLoc (Just preferredPrimarySpecialLocId'.getId) -- >>= fromMaybeM (InvalidRequest $ "SpecialLocWarrior Loc not found with id : " <> preferredPrimarySpecialLocId'.getId)
  let preferredPrimarySpecialLocation = primarySpecialLocation <|> driverInfo.preferredPrimarySpecialLoc
  when (isSpecialLocWarrior == True && isNothing preferredPrimarySpecialLocation) $ throwError (InvalidRequest "preferredPrimarySpecialLoc is required when isSpecialLocWarrior is true")
  QDI.updateSpecialLocWarriorInfo isSpecialLocWarrior preferredPrimarySpecialLocation preferredSecondarySpecialLocIds personId
  return $
    SpecialLocWarriorInfoRes
      { isSpecialLocWarrior = isSpecialLocWarrior,
        preferredPrimarySpecialLoc = preferredPrimarySpecialLocation,
        preferredSecondarySpecialLocIds = preferredSecondarySpecialLocIds
      }
