{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.DriverOnboardingV2 where

import qualified API.Types.UI.DriverOnboardingV2
import Data.OpenApi (ToSchema)
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Vehicle as DTV
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Types (Language (..))
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.Queries.Person as PersonQuery
import qualified Storage.Queries.Translations as MTQuery
import Tools.Auth

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
    Environment.Flow API.Types.UI.DriverOnboardingV2.DocumentVerificationConfigList
  )
getOnboardingConfigs (mbPersonId, _, merchanOperatingCityId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ PersonQuery.findById personId >>= fromMaybeM (PersonNotFound "No person found")
  let personLangauge = fromMaybe ENGLISH person.language
  cabConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchanOperatingCityId DTV.CAR
  autoConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchanOperatingCityId DTV.AUTO_CATEGORY
  bikeConfigsRaw <- CQDVC.findByMerchantOpCityIdAndCategory merchanOperatingCityId DTV.MOTORCYCLE

  cabConfigs <- mapM (mkDocumentVerificationConfigAPIEntity personLangauge) cabConfigsRaw
  autoConfigs <- mapM (mkDocumentVerificationConfigAPIEntity personLangauge) autoConfigsRaw
  bikeConfigs <- mapM (mkDocumentVerificationConfigAPIEntity personLangauge) bikeConfigsRaw

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
