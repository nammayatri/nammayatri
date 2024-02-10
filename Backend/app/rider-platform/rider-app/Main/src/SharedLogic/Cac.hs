{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module SharedLogic.Cac where

import qualified Client.Main as CM
import qualified Data.Aeson as DA
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Utils.Logging

getFrontendConfigs :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Log m) => Person -> Maybe Int -> m (Maybe DA.Object)
getFrontendConfigs person mbToss = do
  ghcCond <- liftIO $ CM.hashMapToString $ HM.fromList [(T.pack "city", (DA.String . T.pack . show) person.currentCity)]
  contextValue <- case mbToss of
    Just toss -> liftIO $ CM.evalExperiment "atlas_customer_ui" ghcCond toss -- CACTODO: remove hardcoding of tenant name
    Nothing -> liftIO $ CM.evalCtx "atlas_customer_ui" ghcCond
  case contextValue of
    Left err -> do
      logError $ "Error in getting frontend configs: " <> show err <> "City: " <> show person.currentCity <> "toss: " <> show mbToss
      return Nothing
    Right cfgs -> return $ Just cfgs
