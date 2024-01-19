{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.FRFS.Utils where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import Control.Lens ((%~))
import qualified Data.Text as T
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Servant.BaseUrl

buildContext ::
  (MonadFlow m, HasFlowEnv m r '["nwAddress" ::: BaseUrl]) =>
  Spec.Action ->
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  m Spec.Context
buildContext action merchantId txnId msgId mTTL = do
  let bapId = merchantId
  now <- getCurrentTime
  bapUrl <- asks (.nwAddress) <&> #baseUrlPath %~ (<> "/" <> T.unpack merchantId) <&> showBaseUrlText
  return $
    Spec.Context
      { contextAction = Just $ encodeToText action,
        contextBapId = Just bapId,
        contextBapUri = Just bapUrl,
        contextBppId = Nothing,
        contextBppUri = Nothing,
        contextDomain = Just $ encodeToText Spec.FRFS,
        contextKey = Nothing,
        contextLocation = Nothing,
        contextMessageId = Just msgId,
        contextTimestamp = Just now,
        contextTransactionId = Just txnId,
        contextTtl = mTTL,
        contextVersion = Just "2.0.0"
      }
