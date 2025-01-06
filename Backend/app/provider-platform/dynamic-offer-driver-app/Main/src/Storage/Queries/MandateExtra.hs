module Storage.Queries.MandateExtra where

import Domain.Types.Mandate as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.Mandate as BeamM
import Storage.Queries.OrphanInstances.Mandate ()

-- Extra code goes here --

updateMandateDetails :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Domain.Mandate -> MandateStatus -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m ()
updateMandateDetails (Id mandateId) status payerVpa payerApp payerAppName mandatePaymentFlow = do
  now <- getCurrentTime
  updateOneWithKV
    ( [Se.Set BeamM.status status, Se.Set BeamM.updatedAt now]
        <> [Se.Set BeamM.payerVpa payerVpa | isJust payerVpa]
        <> [Se.Set BeamM.payerApp payerApp | isJust payerApp]
        <> [Se.Set BeamM.payerAppName payerAppName | isJust payerAppName]
        <> [Se.Set BeamM.mandatePaymentFlow mandatePaymentFlow | isJust mandatePaymentFlow]
    )
    [Se.Is BeamM.id (Se.Eq mandateId)]
