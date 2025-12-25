{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Location (module Storage.Queries.Location, module ReExport) where

import qualified Domain.Types.Location
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Location as Beam
import Storage.Queries.LocationExtra as ReExport
import Storage.Queries.Transformers.Location

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Location.Location -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Location.Location] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Location.Location -> m (Maybe Domain.Types.Location.Location))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateInstructionsAndExtrasById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Location.Location -> m ())
updateInstructionsAndExtrasById instructionsBeam extrasBeam id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.instructions instructionsBeam, Se.Set Beam.extras extrasBeam, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
