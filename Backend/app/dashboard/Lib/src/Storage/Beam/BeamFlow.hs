{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.BeamFlow where

import Kernel.Beam.Lib.UtilsTH as Reexport
import Kernel.Types.Common as Reexport hiding (id)
import Kernel.Utils.Common
import qualified Storage.Beam.AccessMatrix as BeamAM
import qualified Storage.Beam.Merchant as BeamM
import qualified Storage.Beam.MerchantAccess as BeamMA
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.RegistrationToken as BeamRT
import qualified Storage.Beam.Role as BeamR
import qualified Storage.Beam.Transaction as BeamT

type BeamFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow'
  )

type BeamFlow' =
  ( HasSchemaName BeamAM.AccessMatrixT,
    HasSchemaName BeamM.MerchantT,
    HasSchemaName BeamMA.MerchantAccessT,
    HasSchemaName BeamP.PersonT,
    HasSchemaName BeamRT.RegistrationTokenT,
    HasSchemaName BeamR.RoleT,
    HasSchemaName BeamT.TransactionT
  )
