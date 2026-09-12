{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Schema resolution for the dashboard tables.
--
-- These used to be duplicated in each dashboard app's
-- @Storage.Beam.CommonInstances@. That worked while only the dashboards linked
-- these tables, but instances cannot be defined twice in one link, and
-- provider-dashboard depends on dynamic-offer-driver-app -- so the application
-- servers could never define their own copy. Defining them once here is what
-- lets an application server verify a dashboard session directly.
--
-- The schema is not fixed at compile time: 'resolveSchema' returns whatever the
-- app published with 'setDashboardSchemaName' (from @esqDBCfg.connectSchemaName@)
-- at startup, falling back to the argument. Each app therefore keeps its own
-- schema -- atlas_dashboard, atlas_bap_dashboard, atlas_safety_dashboard --
-- from one set of instances.
module Storage.Beam.SchemaInstances (dashboardSchema) where

import Data.Text as T
import Kernel.Beam.Lib.UtilsTH (HasSchemaName (..))
import qualified Storage.Beam.AccessAudit as BeamAA
import qualified Storage.Beam.Capability as BeamC
import qualified Storage.Beam.CapabilityEndpoint as BeamCE
import qualified Storage.Beam.DashboardTransaction as BeamDT
import qualified Storage.Beam.DeletedUser as BeamDU
import qualified Storage.Beam.Entity as BeamE
import qualified Storage.Beam.EntityAccess as BeamEA
import qualified Storage.Beam.Merchant as BeamM
import qualified Storage.Beam.MerchantAccess as BeamMA
import qualified Storage.Beam.MerchantPair as BeamMPair
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.PersonCapability as BeamPC
import qualified Storage.Beam.PersonResourceAccess as BeamPRA
import qualified Storage.Beam.PersonTier as BeamPT
import qualified Storage.Beam.RegistrationToken as BeamRT
import qualified Storage.Beam.Role as BeamR
import qualified Storage.Beam.RoleCapability as BeamRC
import Storage.Beam.SchemaName (resolveSchema)
import qualified Storage.Beam.Transaction as BeamT

dashboardSchema :: Text
dashboardSchema = resolveSchema (T.pack "atlas_dashboard")

instance HasSchemaName BeamAA.AccessAuditT where schemaName _ = dashboardSchema

instance HasSchemaName BeamC.CapabilityT where schemaName _ = dashboardSchema

instance HasSchemaName BeamCE.CapabilityEndpointT where schemaName _ = dashboardSchema

instance HasSchemaName BeamDU.DeletedUserT where schemaName _ = dashboardSchema

-- Instance exists to satisfy shared Beam constraints; the `entity` table is not
-- created in every dashboard schema. A SELECT against it there will 500.
instance HasSchemaName BeamE.EntityT where schemaName _ = dashboardSchema

instance HasSchemaName BeamEA.EntityAccessT where schemaName _ = dashboardSchema

instance HasSchemaName BeamM.MerchantT where schemaName _ = dashboardSchema

instance HasSchemaName BeamMA.MerchantAccessT where schemaName _ = dashboardSchema

instance HasSchemaName BeamMPair.MerchantPairT where schemaName _ = dashboardSchema

instance HasSchemaName BeamP.PersonT where schemaName _ = dashboardSchema

instance HasSchemaName BeamPC.PersonCapabilityT where schemaName _ = dashboardSchema

instance HasSchemaName BeamPT.PersonTierT where schemaName _ = dashboardSchema

instance HasSchemaName BeamRT.RegistrationTokenT where schemaName _ = dashboardSchema

instance HasSchemaName BeamR.RoleT where schemaName _ = dashboardSchema

instance HasSchemaName BeamRC.RoleCapabilityT where schemaName _ = dashboardSchema

instance HasSchemaName BeamT.TransactionT where schemaName _ = dashboardSchema

instance HasSchemaName BeamDT.DashboardTransactionT where schemaName _ = dashboardSchema

instance HasSchemaName BeamPRA.PersonResourceAccessT where
  schemaName _ = resolveSchema (T.pack "atlas_dashboard")
