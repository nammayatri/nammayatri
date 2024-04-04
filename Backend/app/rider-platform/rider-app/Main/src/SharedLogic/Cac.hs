{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module SharedLogic.Cac where

import qualified Data.Aeson as DA
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Utils.Common
import Utils.Common.CacUtils

getFrontendConfigs :: (KvDbFlow m r, Log m) => Person -> Maybe Int -> m (Maybe DA.Object)
getFrontendConfigs person mbToss = do
  let ghcCond = [(City, show person.currentCity)]
  contextValue <- case mbToss of
    Just toss -> getConfigFromCac ghcCond (show RiderFrontEndTenant) toss Empty
    Nothing -> getConfigFromCac ghcCond (show RiderFrontEndTenant) 1 Empty
  case contextValue of
    Nothing -> do
      logError $ "Error in getting frontend configs for City: " <> show person.currentCity <> "toss: " <> show mbToss
      return Nothing
    Just cfgs -> return $ Just cfgs
