{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.SchemaName
  ( setDashboardSchemaName,
    resolveSchema,
  )
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Kernel.Prelude
import System.IO.Unsafe (unsafePerformIO)

-- Dashboard DB schema resolution.
--
-- `HasSchemaName.schemaName :: Proxy t -> Text` is pure, so Beam instances
-- cannot read AppCfg (which only exists inside the flow monad). Each app
-- therefore hardcoded its schema in Storage.Beam.CommonInstances, which made
-- the unification cutover a code change + rebuild rather than a config change.
--
-- Instead the value comes from the dhall config we already have —
-- `esqDBCfg.connectSchemaName` — pushed into this ref by each app's
-- `runService` BEFORE any DB connection is opened. One dhall field then drives
-- both the Beam instances and the connection's schema, so they can never
-- disagree, and the cutover to `atlas_dashboard` is a dhall edit.
--
-- Unset (i.e. an app that never calls the setter) => each app keeps its
-- historical default, so this is a no-op until wired.

schemaNameRef :: IORef (Maybe Text)
schemaNameRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE schemaNameRef #-}

-- | Publish the configured schema. MUST be called by the app entrypoint before
-- the first DB query (see provider-dashboard/rider-dashboard `runService`);
-- after that the value is effectively immutable for the process.
setDashboardSchemaName :: MonadIO m => Text -> m ()
setDashboardSchemaName = liftIO . writeIORef schemaNameRef . Just

-- | @resolveSchema historicalDefault@ — the configured schema, falling back to
-- the app's historical default when the entrypoint hasn't published one.
--
-- Reading an IORef from pure code is safe here because the value is written
-- once at startup, strictly before anything can query the DB: the first
-- evaluation happens on the first query, by which point the setter has run.
resolveSchema :: Text -> Text
resolveSchema historicalDefault =
  unsafePerformIO $ fromMaybe historicalDefault <$> readIORef schemaNameRef
{-# NOINLINE resolveSchema #-}
