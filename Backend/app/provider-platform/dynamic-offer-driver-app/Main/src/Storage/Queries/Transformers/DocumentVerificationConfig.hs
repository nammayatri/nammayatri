module Storage.Queries.Transformers.DocumentVerificationConfig where

import qualified Data.Aeson
import qualified Domain.Types.DocumentVerificationConfig
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (MonadFlow, throwError)

getConfigJSON :: Domain.Types.DocumentVerificationConfig.SupportedVehicleClasses -> Data.Aeson.Value
getConfigJSON = \case
  Domain.Types.DocumentVerificationConfig.DLValidClasses cfg -> toJSON cfg
  Domain.Types.DocumentVerificationConfig.RCValidClasses cfg -> toJSON cfg

mkDocumentFieldsJSON :: Maybe [Domain.Types.DocumentVerificationConfig.FieldInfo] -> Maybe Data.Aeson.Value
mkDocumentFieldsJSON = fmap Data.Aeson.toJSON

getDocumentFieldsFromJSON :: Maybe Data.Aeson.Value -> Maybe [Domain.Types.DocumentVerificationConfig.FieldInfo]
getDocumentFieldsFromJSON mDocumentFieldsJSON =
  mDocumentFieldsJSON >>= \val -> case Data.Aeson.fromJSON val of
    Data.Aeson.Success x -> Just x
    Data.Aeson.Error _ -> Nothing

getConfigFromJSON :: MonadFlow m => Domain.Types.DocumentVerificationConfig.DocumentType -> Data.Aeson.Value -> m Domain.Types.DocumentVerificationConfig.SupportedVehicleClasses
getConfigFromJSON documentType _supportedVehicleClassesJSON =
  case documentType of
    Domain.Types.DocumentVerificationConfig.DriverLicense -> Domain.Types.DocumentVerificationConfig.DLValidClasses <$> valueToVehicleClassMap _supportedVehicleClassesJSON
    Domain.Types.DocumentVerificationConfig.VehicleRegistrationCertificate -> Domain.Types.DocumentVerificationConfig.RCValidClasses <$> valueToVehicleClassMap _supportedVehicleClassesJSON
    _ -> Domain.Types.DocumentVerificationConfig.DLValidClasses <$> valueToVehicleClassMap _supportedVehicleClassesJSON
  where
    valueToVehicleClassMap value = case Data.Aeson.fromJSON value of
      Data.Aeson.Error err -> throwError $ InternalError $ "Unable to decode DocumentVerificationConfigT.supportedVehicleClassesJSON: " <> show err
      Data.Aeson.Success a -> pure a
