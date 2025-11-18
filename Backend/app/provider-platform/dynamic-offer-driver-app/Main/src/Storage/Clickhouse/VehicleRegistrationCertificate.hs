module Storage.Clickhouse.VehicleRegistrationCertificate where

import qualified Domain.Types.VehicleRegistrationCertificate as DRC
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data VehicleRegistrationCertificateT f = VehicleRegistrationCertificateT
  { id :: C f (Id DRC.VehicleRegistrationCertificate),
    unencryptedCertificateNumber :: C f (Maybe Text),
    vehicleModel :: C f (Maybe Text)
  }
  deriving (Generic)

deriving instance Show VehicleRegistrationCertificate

vehicleRegistrationCertificateTTable :: VehicleRegistrationCertificateT (FieldModification VehicleRegistrationCertificateT)
vehicleRegistrationCertificateTTable =
  VehicleRegistrationCertificateT
    { id = "id",
      unencryptedCertificateNumber = "unencrypted_certificate_number",
      vehicleModel = "vehicle_model"
    }

type VehicleRegistrationCertificate = VehicleRegistrationCertificateT Identity

$(TH.mkClickhouseInstances ''VehicleRegistrationCertificateT 'SELECT_FINAL_MODIFIER)

findByIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DRC.VehicleRegistrationCertificate] ->
  m [VehicleRegistrationCertificate]
findByIds ids = do
  CH.findAll $
    CH.select $
      CH.filter_ (\rc -> rc.id `CH.in_` ids) (CH.all_ @CH.APP_SERVICE_CLICKHOUSE vehicleRegistrationCertificateTTable)
