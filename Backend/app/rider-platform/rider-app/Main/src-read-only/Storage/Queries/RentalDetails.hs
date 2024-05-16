{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RentalDetails (module Storage.Queries.RentalDetails, module ReExport) where

import qualified Domain.Types.RentalDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RentalDetails as Beam
import Storage.Queries.RentalDetailsExtra as ReExport

findById :: KvDbFlow m r => (Kernel.Types.Id.Id Domain.Types.RentalDetails.RentalDetails -> m (Maybe Domain.Types.RentalDetails.RentalDetails))
findById (Kernel.Types.Id.Id id) = do findOneWithKV [Se.Is Beam.id $ Se.Eq id]
