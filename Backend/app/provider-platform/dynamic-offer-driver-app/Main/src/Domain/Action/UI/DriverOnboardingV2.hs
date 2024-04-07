{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.DriverOnboardingV2 where

import qualified API.Types.UI.DriverOnboardingV2
import Data.OpenApi (ToSchema)
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import Domain.Types.ServiceTierType
import qualified Domain.Types.Vehicle as DTV
import Domain.Types.VehicleServiceTier
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Types (Language (..))
import qualified Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import SharedLogic.DriverOnboarding
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as PersonQuery
import qualified Storage.Queries.Translations as MTQuery
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Auth
import Tools.Error

mkDocumentVerificationConfigAPIEntity :: Language -> Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig -> Environment.Flow API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity
mkDocumentVerificationConfigAPIEntity language Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig {..} = do
  mbTitle <- MTQuery.findByErrorAndLanguage ((show documentType) <> "_Title") language
  mbDescription <- MTQuery.findByErrorAndLanguage ((show documentType) <> "_Description") language
  return $
    API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigAPIEntity
      { title = maybe title (.message) mbTitle,
        description = maybe description (Just . (.message)) mbDescription,
        ..
      }

getOnboardingConfigs ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.Flow API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
  )
getOnboardingConfigs (mbPersonId, _, merchanOperatingCityId) mbOnlyVehicle = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound "No person found")
  let personLangauge = fromMaybe ENGLISH person.language
  cabConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchanOperatingCityId DTV.CAR
  autoConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchanOperatingCityId DTV.AUTO_CATEGORY
  bikeConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchanOperatingCityId DTV.MOTORCYCLE

  cabConfigs <- mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments cabConfigsRaw)
  autoConfigs <- mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments autoConfigsRaw)
  bikeConfigs <- mapM (mkDocumentVerificationConfigAPIEntity personLangauge) (filterVehicleDocuments bikeConfigsRaw)

  return $
    API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
      { cabs = toMaybe cabConfigs,
        autos = toMaybe autoConfigs,
        bikes = toMaybe bikeConfigs
      }
  where
    toMaybe :: [a] -> Kernel.Prelude.Maybe [a]
    toMaybe [] = Kernel.Prelude.Nothing
    toMaybe xs = Kernel.Prelude.Just xs

    filterVehicleDocuments :: [Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig] -> [Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig]
    filterVehicleDocuments docs =
      if mbOnlyVehicle == Just True
        then filter (\Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig {..} -> documentType `elem` vehicleDocumentTypes) docs
        else docs

getDriverVehicleServiceTiers ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
  )
getDriverVehicleServiceTiers (mbPersonId, _, merchanOperatingCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound "No person found")
  driverInfo <- runInReplica $ QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- runInReplica $ QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchanOperatingCityId

  let driverVehicleServiceTierTypes = selectVehicleTierForDriver person driverInfo vehicle cityVehicleServiceTiers

  let tierOptions =
        driverVehicleServiceTierTypes <&> \VehicleServiceTier {..} -> do
          API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTier
            { isSelected = serviceTierType `elem` vehicle.selectedServiceTiers,
              isDefault = vehicle.variant `elem` defaultForVehicleVariant,
              ..
            }
  return $
    API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers
      { tiers = tierOptions
      }

postDriverUpdateServiceTiers ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers ->
    Environment.Flow APISuccess
  )
postDriverUpdateServiceTiers (mbPersonId, _, merchanOperatingCityId) API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTiers {..} = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound "No person found")
  driverInfo <- runInReplica $ QDI.findById personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- runInReplica $ QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)
  cityVehicleServiceTiers <- CQVST.findAllByMerchantOpCityId merchanOperatingCityId

  let driverVehicleServiceTierTypes = selectVehicleTierForDriver person driverInfo vehicle cityVehicleServiceTiers
  let defaultServiceTierForDriver = (.serviceTierType) <$> (find (\vst -> vehicle.variant `elem` vst.defaultForVehicleVariant) driverVehicleServiceTierTypes)

  mbSelectedServiceTiers <-
    tiers `forM` \API.Types.UI.DriverOnboardingV2.DriverVehicleServiceTier {..} -> do
      when (isSelected && serviceTierType `notElem` (map (.serviceTierType) driverVehicleServiceTierTypes)) $
        throwError $ InvalidRequest "Driver can't select a service tier that is not available"
      if isSelected || defaultServiceTierForDriver == Just serviceTierType
        then return $ Just serviceTierType
        else return Nothing

  when (any (\tier -> tier.serviceTierType `elem` (map (.serviceTierType) driverVehicleServiceTierTypes)) tiers) $
    throwError $ InvalidRequest "Drive can't select a service tier that is not available"

  QVehicle.updateSelectedServiceTiers (catMaybes mbSelectedServiceTiers) personId

  return Success
