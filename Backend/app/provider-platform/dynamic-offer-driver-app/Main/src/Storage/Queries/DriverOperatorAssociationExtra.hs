module Storage.Queries.DriverOperatorAssociationExtra where

import qualified Domain.Types.DriverOperatorAssociation
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOperatorAssociation as Beam
import Storage.Queries.DriverOperatorAssociation ()

findAllByOperatorIdWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  m [Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation]
findAllByOperatorIdWithLimitOffset operatorId mbIsActive mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is Beam.operatorId $ Se.Eq operatorId]
          <> [Se.Is Beam.isActive $ Se.Eq (fromJust mbIsActive) | isJust mbIsActive]
    ]
    (Se.Asc Beam.associatedOn)
    (Just . min 10 . fromMaybe 5 $ mbLimit)
    (Just $ fromMaybe 0 mbOffset)
