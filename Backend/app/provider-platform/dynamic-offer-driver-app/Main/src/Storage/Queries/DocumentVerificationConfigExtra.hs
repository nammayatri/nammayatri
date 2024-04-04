{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DocumentVerificationConfigExtra where

import Domain.Types.DocumentVerificationConfig
import Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import Sequelize as Se
import qualified Storage.Beam.DocumentVerificationConfig as BeamODC
import Storage.Queries.OrphanInstances.DocumentVerificationConfig
import Storage.Queries.Transformers.DocumentVerificationConfig

-- Extra code goes here --
update :: KvDbFlow m r => DocumentVerificationConfig -> m ()
update config = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamODC.checkExtraction (config.checkExtraction),
      Se.Set BeamODC.checkExpiry (config.checkExpiry),
      Se.Set BeamODC.supportedVehicleClassesJSON $ getConfigJSON config.supportedVehicleClasses,
      Se.Set BeamODC.vehicleClassCheckType (config.vehicleClassCheckType),
      Se.Set BeamODC.rcNumberPrefixList (config.rcNumberPrefixList),
      Se.Set BeamODC.maxRetryCount (config.maxRetryCount),
      Se.Set BeamODC.updatedAt now
    ]
    [Se.Is BeamODC.merchantOperatingCityId $ Se.Eq $ getId config.merchantOperatingCityId, Se.Is BeamODC.documentType $ Se.Eq config.documentType]

updateSupportedVehicleClassesJSON :: KvDbFlow m r => Id MerchantOperatingCity -> SupportedVehicleClasses -> m ()
updateSupportedVehicleClassesJSON merchantOperatingCityId supportedVehicleClasses = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamODC.supportedVehicleClassesJSON $ getConfigJSON supportedVehicleClasses,
      Se.Set BeamODC.updatedAt now
    ]
    [ Se.Is BeamODC.merchantOperatingCityId $ Se.Eq $ getId merchantOperatingCityId,
      Se.Is BeamODC.documentType $ Se.Eq VehicleRegistrationCertificate
    ]
