{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module IssueManagement.Tools.UtilsTH (module Reexport, module IssueManagement.Tools.UtilsTH) where

import Kernel.Beam.Lib.UtilsTH as Reexport hiding (mkTableInstances, mkTableInstancesWithTModifier, schemaName)
import qualified Kernel.Beam.Lib.UtilsTH as TH
import Kernel.Prelude hiding (catch)
import Kernel.Types.Common as Reexport hiding (id)
import Language.Haskell.TH
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)

schemaName :: String
{-# NOINLINE schemaName #-}
schemaName = unsafePerformIO (getEnv "SCHEMA_NAME")

mkTableInstances :: Name -> String -> Q [Dec]
mkTableInstances name table = TH.mkTableInstances name table schemaName

mkTableInstancesWithTModifier :: Name -> String -> [(String, String)] -> Q [Dec]
mkTableInstancesWithTModifier name table = TH.mkTableInstancesWithTModifier name table schemaName
