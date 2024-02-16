{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.CommonInstances where

import Data.Text as T
import Kernel.Beam.Lib.UtilsTH as Reexport
import qualified "lib-dashboard" Storage.Beam.AccessMatrix as BeamAM
import qualified "lib-dashboard" Storage.Beam.Merchant as BeamM
import qualified "lib-dashboard" Storage.Beam.MerchantAccess as BeamMA
import qualified "lib-dashboard" Storage.Beam.Person as BeamP
import qualified "lib-dashboard" Storage.Beam.RegistrationToken as BeamRT
import qualified "lib-dashboard" Storage.Beam.Role as BeamR
import qualified "lib-dashboard" Storage.Beam.Transaction as BeamT

instance HasSchemaName BeamAM.AccessMatrixT where
  schemaName _ = T.pack "atlas_bpp_dashboard"

instance HasSchemaName BeamM.MerchantT where
  schemaName _ = T.pack "atlas_bpp_dashboard"

instance HasSchemaName BeamMA.MerchantAccessT where
  schemaName _ = T.pack "atlas_bpp_dashboard"

instance HasSchemaName BeamP.PersonT where
  schemaName _ = T.pack "atlas_bpp_dashboard"

instance HasSchemaName BeamRT.RegistrationTokenT where
  schemaName _ = T.pack "atlas_bpp_dashboard"

instance HasSchemaName BeamR.RoleT where
  schemaName _ = T.pack "atlas_bpp_dashboard"

instance HasSchemaName BeamT.TransactionT where
  schemaName _ = T.pack "atlas_bpp_dashboard"
