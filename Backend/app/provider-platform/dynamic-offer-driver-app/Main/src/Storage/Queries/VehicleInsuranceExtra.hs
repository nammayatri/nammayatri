module Storage.Queries.VehicleInsuranceExtra where

import Domain.Types.Image
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Documents
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleInsurance as BeamVI
import Storage.Queries.OrphanInstances.VehicleInsurance ()

-- Extra code goes here --

updateVerificationStatusAndRejectReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Documents.VerificationStatus -> Text -> Kernel.Types.Id.Id Domain.Types.Image.Image -> m ())
updateVerificationStatusAndRejectReason verificationStatus rejectReason (Kernel.Types.Id.Id imageId) = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set BeamVI.verificationStatus verificationStatus, Se.Set BeamVI.rejectReason (Just rejectReason), Se.Set BeamVI.updatedAt _now] [Se.Is BeamVI.documentImageId $ Se.Eq imageId]
