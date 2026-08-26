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

import qualified Data.Aeson as A
import Data.Text as T
import Kernel.Beam.Lib.UtilsTH as Reexport
import qualified Kernel.Prelude
import qualified Kernel.Storage.Beam.MerchantOperatingCity as BeamMOC
import Kernel.Types.HideSecrets (HideSecrets (..))
import qualified "lib-dashboard" Storage.Beam.AccessAudit as BeamAA
import qualified "lib-dashboard" Storage.Beam.AccessMatrix as BeamAM
import qualified "lib-dashboard" Storage.Beam.Capability as BeamC
import qualified "lib-dashboard" Storage.Beam.CapabilityEndpoint as BeamCE
import qualified "lib-dashboard" Storage.Beam.DeletedUser as BeamDU
import qualified "lib-dashboard" Storage.Beam.Entity as BeamE
import qualified "lib-dashboard" Storage.Beam.Merchant as BeamM
import qualified "lib-dashboard" Storage.Beam.MerchantAccess as BeamMA
import qualified "lib-dashboard" Storage.Beam.MerchantPair as BeamMPair
import qualified "lib-dashboard" Storage.Beam.Person as BeamP
import qualified "lib-dashboard" Storage.Beam.PersonCapability as BeamPC
import qualified "lib-dashboard" Storage.Beam.PersonTier as BeamPT
import qualified "lib-dashboard" Storage.Beam.RegistrationToken as BeamRT
import qualified "lib-dashboard" Storage.Beam.Role as BeamR
import qualified "lib-dashboard" Storage.Beam.RoleCapability as BeamRC
import "lib-dashboard" Storage.Beam.SchemaName (resolveSchema)
import qualified "lib-dashboard" Storage.Beam.Transaction as BeamT

instance HasSchemaName BeamAA.AccessAuditT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamDU.DeletedUserT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamAM.AccessMatrixT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamC.CapabilityT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamPT.PersonTierT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamCE.CapabilityEndpointT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamPC.PersonCapabilityT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamRC.RoleCapabilityT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

-- Needed by the RiderPlatform (bap) modules mounted on this server.
instance HideSecrets A.Value where
  hideSecrets = Kernel.Prelude.identity

-- Instance exists to satisfy shared Beam constraints; `entity` table is not
-- created here. Any future SELECT against it will 500. See PT-employee PR notes.
instance HasSchemaName BeamE.EntityT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamM.MerchantT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamMA.MerchantAccessT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamMPair.MerchantPairT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamP.PersonT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamRT.RegistrationTokenT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamR.RoleT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamT.TransactionT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamMOC.MerchantOperatingCityT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")
