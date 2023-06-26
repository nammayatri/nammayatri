{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Merchant.OnboardingDocumentConfig where

import Data.List (sortBy)
import qualified Domain.Types.Merchant as Domain
import qualified Domain.Types.Merchant.OnboardingDocumentConfig as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common (decodeFromText, encodeToText)
import Kernel.Utils.Error
import Storage.Tabular.Merchant (MerchantTId)
import Tools.Error

derivePersistField "Domain.VehicleClassCheckType"
derivePersistField "Domain.DocumentType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    OnboardingDocumentConfigT sql=onboarding_document_configs
      merchantId MerchantTId
      documentType Domain.DocumentType
      checkExtraction Bool
      checkExpiry Bool
      supportedVehicleClassesJSON Text sql=supported_vehicle_classes_json
      vehicleClassCheckType Domain.VehicleClassCheckType
      createdAt UTCTime
      updatedAt UTCTime
      Primary merchantId documentType
      deriving Generic
    |]

instance TEntityKey OnboardingDocumentConfigT where
  type DomainKey OnboardingDocumentConfigT = (Id Domain.Merchant, Domain.DocumentType)
  fromKey (OnboardingDocumentConfigTKey _id _documentType) = (fromKey _id, _documentType)
  toKey (id, documentType) = OnboardingDocumentConfigTKey (toKey id) documentType

instance FromTType OnboardingDocumentConfigT Domain.OnboardingDocumentConfig where
  fromTType OnboardingDocumentConfigT {..} = do
    supportedVehicleClasses <- maybe (throwError $ InternalError "Unable to decode OnboardingDocumentConfigT.supportedVehicleClasses") return $ case documentType of
      Domain.DL -> Domain.DLValidClasses <$> decodeFromText supportedVehicleClassesJSON
      Domain.RC -> Domain.RCValidClasses . sortOnCapcity <$> decodeFromText supportedVehicleClassesJSON
      _ -> Just $ Domain.RCValidClasses []
    return $
      Domain.OnboardingDocumentConfig
        { merchantId = fromKey merchantId,
          ..
        }
    where
      sortOnCapcity = sortBy (\a b -> compare b.vehicleCapacity a.vehicleCapacity)

instance ToTType OnboardingDocumentConfigT Domain.OnboardingDocumentConfig where
  toTType Domain.OnboardingDocumentConfig {..} = do
    let supportedVehicleClassesJSON = getConfigJSON supportedVehicleClasses
    OnboardingDocumentConfigT
      { merchantId = toKey merchantId,
        ..
      }

getConfigJSON :: Domain.SupportedVehicleClasses -> Text
getConfigJSON = \case
  Domain.DLValidClasses cfg -> encodeToText cfg
  Domain.RCValidClasses cfg -> encodeToText cfg
