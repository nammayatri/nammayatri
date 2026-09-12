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
import Kernel.Beam.Lib.UtilsTH (HasSchemaName (..))
import qualified Kernel.Prelude
import qualified Kernel.Storage.Beam.MerchantOperatingCity as BeamMOC
import Kernel.Types.HideSecrets (HideSecrets (..))
import "lib-dashboard" Storage.Beam.SchemaInstances ()
import "lib-dashboard" Storage.Beam.SchemaName (resolveSchema)

instance HideSecrets A.Value where
  hideSecrets = Kernel.Prelude.identity

-- Shared mobility-core table, mapped to this dashboard's schema. Not in
-- lib-dashboard's SchemaInstances: the application servers map the same table
-- to their own schemas, and one instance cannot serve both.
instance HasSchemaName BeamMOC.MerchantOperatingCityT where
  schemaName _ = resolveSchema (T.pack "atlas_bap_dashboard")
