{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Chat
  ( chat,
  )
where

import qualified API.Types.UI.Chat as DChat
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RiderConfig as DRC
import Environment
import Kernel.Prelude
import Kernel.Storage.Hedis qualified as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Utils as YUtils
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRiderConfig
import qualified Storage.Queries.Person as QPerson

checkChatRateLimit ::
  ( Redis.HedisFlow m r,
    HasFlowEnv m r '["chatRateLimitOptions" ::: APIRateLimitOptions]
  ) =>
  Id Person.Person ->
  m ()
checkChatRateLimit personId = do
  let key = chatHitsCountKey personId
  hitsLimit <- asks (.chatRateLimitOptions.limit)
  limitResetTimeInSec <- asks (.chatRateLimitOptions.limitResetTimeInSec)
  unlessM (slidingWindowLimiter key hitsLimit limitResetTimeInSec) $ do
    throwError $ HitsLimitError limitResetTimeInSec

chatHitsCountKey :: Id Person.Person -> Text
chatHitsCountKey personId = "BAP:Chat:" <> personId.getId <> ":hitsCount"

getIsChatEnabled :: Maybe [LYT.TagNameValueExpiry] -> Bool
getIsChatEnabled mbTags =
  let chatEnabledTagName = LYT.TagName "ChatEnabled"
      currentTags = fromMaybe [] mbTags
      isChatEnabledTag targetTagName customerTag =
        case YUtils.parseTagName customerTag of
          Just tagName -> tagName == targetTagName
          Nothing -> False
   in any (isChatEnabledTag chatEnabledTagName) currentTags

chat ::
  ( Redis.HedisFlow m r,
    HasFlowEnv m r '["chatRateLimitOptions" ::: APIRateLimitOptions],
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["version" ::: DeploymentVersion, "cloudType" ::: Maybe CloudType]
  ) =>
  (Id Person.Person, Id Merchant.Merchant) ->
  DChat.ChatReq ->
  m DChat.ChatRes
chat (personId, _merchantId) req = do
  -- Check rate limiting first
  checkChatRateLimit personId

  -- Fetch person and check if chat is enabled
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  -- Check if chat is enabled for this person via customerNammaTags
  let isChatEnabled = getIsChatEnabled person.customerNammaTags
  unless isChatEnabled $ throwError $ InvalidRequest "Chat is not enabled for this user"

  -- TODO: Integrate with LLM service for actual chat processing
  -- For now, return a mock response
  let responseMessage =
        DChat.ChatMessage
          { role = DChat.Assistant,
            content = "This is a placeholder response. LLM integration will be implemented here."
          }

  return $
    DChat.ChatRes
      { message = responseMessage,
        toolCalls = Nothing
      }
