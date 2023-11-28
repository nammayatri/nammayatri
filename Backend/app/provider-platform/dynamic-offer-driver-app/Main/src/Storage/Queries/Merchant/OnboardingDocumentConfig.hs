{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.OnboardingDocumentConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Data.Aeson (fromJSON)
import qualified Data.Aeson as A
import qualified Data.List
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.OnboardingDocumentConfig
import qualified Domain.Types.Merchant.OnboardingDocumentConfig as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.OnboardingDocumentConfig as BeamODC

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => OnboardingDocumentConfig -> m ()
create = createWithKV

findAllByMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> m [OnboardingDocumentConfig]
findAllByMerchantOpCityId (Id merchantOperatingCityId) = findAllWithKV [Se.Is BeamODC.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

findByConfigMapId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id ConfigMapping -> m [OnboardingDocumentConfig]
findByConfigMapId (Id configMapId) = findAllWithKV [Se.Is BeamODC.configMapId $ Se.Eq configMapId]

update :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => OnboardingDocumentConfig -> m ()
update config = do
  let supportedClassJson = BeamODC.getConfigJSON config.supportedVehicleClasses
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamODC.checkExtraction (config.checkExtraction),
      Se.Set BeamODC.checkExpiry (config.checkExpiry),
      Se.Set BeamODC.supportedVehicleClassesJSON $ toJSON supportedClassJson,
      Se.Set BeamODC.vehicleClassCheckType (config.vehicleClassCheckType),
      Se.Set BeamODC.rcNumberPrefix (config.rcNumberPrefix),
      Se.Set BeamODC.rcNumberPrefixList (config.rcNumberPrefixList),
      Se.Set BeamODC.updatedAt now
    ]
    [Se.Is BeamODC.merchantOperatingCityId $ Se.Eq $ getId config.merchantOperatingCityId, Se.Is BeamODC.documentType $ Se.Eq config.documentType]

instance FromTType' BeamODC.OnboardingDocumentConfig OnboardingDocumentConfig where
  fromTType' BeamODC.OnboardingDocumentConfigT {..} = do
    supportedVehicleClasses' <- maybe (throwError $ InternalError "Unable to decode OnboardingDocumentConfigT.supportedVehicleClasses") return $ case documentType of
      Domain.DL -> Domain.DLValidClasses <$> valueToMaybe supportedVehicleClassesJSON
      Domain.RC -> Domain.RCValidClasses . sortOnCapcity <$> valueToVehicleClassMap supportedVehicleClassesJSON
      _ -> Just $ Domain.RCValidClasses []
    pure $
      Just
        OnboardingDocumentConfig
          { merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            documentType = documentType,
            checkExtraction = checkExtraction,
            checkExpiry = checkExpiry,
            supportedVehicleClasses = supportedVehicleClasses',
            vehicleClassCheckType = vehicleClassCheckType,
            rcNumberPrefix = rcNumberPrefix,
            rcNumberPrefixList = rcNumberPrefixList,
            createdAt = createdAt,
            updatedAt = updatedAt
          }
    where
      sortOnCapcity = Data.List.sortBy (\a b -> compare b.vehicleCapacity a.vehicleCapacity)
      valueToMaybe :: A.Value -> Maybe [Text]
      valueToMaybe value = case fromJSON value of
        A.Error _ -> Nothing
        A.Success a -> Just a

      valueToVehicleClassMap :: A.Value -> Maybe [VehicleClassVariantMap]
      valueToVehicleClassMap value = case fromJSON value of
        A.Error _ -> Nothing
        A.Success a -> Just a

instance ToTType' BeamODC.OnboardingDocumentConfig OnboardingDocumentConfig where
  toTType' OnboardingDocumentConfig {..} = do
    BeamODC.OnboardingDocumentConfigT
      { BeamODC.merchantId = getId merchantId,
        BeamODC.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamODC.documentType = documentType,
        BeamODC.checkExtraction = checkExtraction,
        BeamODC.checkExpiry = checkExpiry,
        BeamODC.supportedVehicleClassesJSON = toJSON supportedVehicleClasses,
        BeamODC.vehicleClassCheckType = vehicleClassCheckType,
        BeamODC.rcNumberPrefix = rcNumberPrefix,
        BeamODC.rcNumberPrefixList = rcNumberPrefixList,
        BeamODC.createdAt = createdAt,
        BeamODC.updatedAt = updatedAt
      }
