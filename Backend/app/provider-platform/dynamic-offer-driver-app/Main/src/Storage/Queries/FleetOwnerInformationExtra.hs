module Storage.Queries.FleetOwnerInformationExtra where

import Domain.Types.FleetOwnerInformation (FleetOwnerInformation)
import Kernel.Beam.Functions
--import Kernel.External.Encryption
import Kernel.Prelude
--import qualified Kernel.Types.Common
--import Kernel.Types.Error
--import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOwnerInformation as Beam
import Storage.Queries.FleetOwnerInformation ()

-- findAllByReferredByOperatorId ::
--   (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
--   Kernel.Prelude.Maybe Kernel.Prelude.Text ->
--   m [Domain.Types.FleetOwnerInformation.FleetOwnerInformation]
-- findAllByReferredByOperatorId referredByOperatorId = findAllWithKV [Se.Is Beam.referredByOperatorId $ Se.Eq referredByOperatorId]

findAllByReferredByOperatorIdAndEnabledAndVerifiedWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Bool ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  m [FleetOwnerInformation]
findAllByReferredByOperatorIdAndEnabledAndVerifiedWithLimitOffset mbEnabled mbVerified mbLimit mbOffset mbReferredByOperatorId = do
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.referredByOperatorId $ Se.Eq mbReferredByOperatorId]
          <> [Se.Is Beam.enabled $ Se.Eq (fromJust mbEnabled) | isJust mbEnabled]
          <> [Se.Is Beam.verified $ Se.Eq (fromJust mbVerified) | isJust mbVerified]
    ]
    (Se.Asc Beam.enabled)
    (Just . min 10 . fromMaybe 5 $ mbLimit)
    (Just $ fromMaybe 0 mbOffset)
