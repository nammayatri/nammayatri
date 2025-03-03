{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Fleet.Onboarding (getOnboardingDocumentConfigs) where

import qualified API.Types.ProviderPlatform.Fleet.Onboarding
import API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration
import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Domain.Types.DocumentVerificationConfig
import Domain.Types.FleetOwnerInformation
import qualified Domain.Types.Merchant
import Domain.Types.Person
import qualified Domain.Types.VehicleCategory as DVC
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Types (Language (ENGLISH))
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import Servant
import SharedLogic.DriverOnboarding
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as FODVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.FleetOwnerInformation as FOIQuery
import qualified Storage.Queries.Person as PersonQuery
import qualified Storage.Queries.Translations as MTQuery
import Tools.Auth

getOnboardingDocumentConfigs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Onboarding.Role -> Environment.Flow API.Types.ProviderPlatform.Fleet.Onboarding.DocumentVerificationConfigList)
getOnboardingDocumentConfigs merchantShortId opCity fleetOwnerId makeSelfieAadhaarPanMandatory onlyVehicle role = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  mbPerson <- runInReplica $ PersonQuery.findById (Id fleetOwnerId)
  let personLanguage = maybe ENGLISH (fromMaybe ENGLISH . language) mbPerson
  cabConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.CAR
  autoConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.AUTO_CATEGORY
  bikeConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.MOTORCYCLE
  ambulanceConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.AMBULANCE
  truckConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.TRUCK
  busConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchantOpCityId DVC.BUS
  fleetConfigsRaw <- case role of
    Just API.Types.ProviderPlatform.Fleet.Onboarding.NORMAL_FLEET -> FODVC.findAllByMerchantOpCityIdAndRole merchantOpCityId FLEET_OWNER
    Just API.Types.ProviderPlatform.Fleet.Onboarding.BUSINESS_FLEET -> FODVC.findAllByMerchantOpCityIdAndRole merchantOpCityId FLEET_BUSINESS
    _ -> pure []
  cabConfigs <- (filterInCompatibleFlows makeSelfieAadhaarPanMandatory) <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (filterVehicleDocuments cabConfigsRaw onlyVehicle)
  autoConfigs <- (filterInCompatibleFlows makeSelfieAadhaarPanMandatory) <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (filterVehicleDocuments autoConfigsRaw onlyVehicle)
  bikeConfigs <- (filterInCompatibleFlows makeSelfieAadhaarPanMandatory) <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (filterVehicleDocuments bikeConfigsRaw onlyVehicle)
  ambulanceConfigs <- (filterInCompatibleFlows makeSelfieAadhaarPanMandatory) <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (filterVehicleDocuments ambulanceConfigsRaw onlyVehicle)
  truckConfigs <- (filterInCompatibleFlows makeSelfieAadhaarPanMandatory) <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (filterVehicleDocuments truckConfigsRaw onlyVehicle)
  busConfigs <- (filterInCompatibleFlows makeSelfieAadhaarPanMandatory) <$> mapM (mkDocumentVerificationConfigAPIEntity personLanguage) (filterVehicleDocuments busConfigsRaw onlyVehicle)
  fleetConfigs <- (filterInCompatibleFlows makeSelfieAadhaarPanMandatory) <$> mapM (mkFleetOwnerDocumentVerificationConfigAPIEntity personLanguage) fleetConfigsRaw
  return $
    API.Types.ProviderPlatform.Fleet.Onboarding.DocumentVerificationConfigList
      { cabs = toMaybe cabConfigs,
        autos = toMaybe autoConfigs,
        bikes = toMaybe bikeConfigs,
        ambulances = toMaybe ambulanceConfigs,
        trucks = toMaybe truckConfigs,
        bus = toMaybe busConfigs,
        fleet = toMaybe fleetConfigs
      }
