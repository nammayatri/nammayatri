{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Producer.SchedulerJob (module Reexport) where

import qualified Data.Text as T
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Lib.Scheduler.JobStorageType.DB.Table as Reexport
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import "dynamic-offer-driver-app" Tools.Beam.UtilsTH (currentSchemaName)

instance HasSchemaName SchedulerJobT where
  {-# NOINLINE schemaName #-}
  schemaName _ = T.pack . fromMaybe currentSchemaName . unsafePerformIO $ lookupEnv "GET_MY_SCHEMA"
