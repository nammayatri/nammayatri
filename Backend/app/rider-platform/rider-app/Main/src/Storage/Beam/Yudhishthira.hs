{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Yudhishthira (module Reexport) where

import qualified Data.Text as T
import Lib.Yudhishthira.Storage.Beam.AppDynamicLogicElement as Reexport
import Lib.Yudhishthira.Storage.Beam.AppDynamicLogicRollout as Reexport
import Lib.Yudhishthira.Storage.Beam.ChakraQueries as Reexport
import Lib.Yudhishthira.Storage.Beam.NammaTag as Reexport
import Lib.Yudhishthira.Storage.Beam.NammaTagTrigger as Reexport
import Lib.Yudhishthira.Storage.Beam.TagActionNotificationConfig as Reexport
import Lib.Yudhishthira.Storage.Beam.TimeBoundConfig as Reexport
import Lib.Yudhishthira.Storage.Beam.UserData as Reexport
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)

instance HasSchemaName AppDynamicLogicElementT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName AppDynamicLogicRolloutT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName NammaTagT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName NammaTagTriggerT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName ChakraQueriesT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName UserDataT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName TimeBoundConfigT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName TagActionNotificationConfigT where
  schemaName _ = T.pack currentSchemaName
