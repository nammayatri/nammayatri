{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Production 'WaSender' over 'Kernel.External.Meta'. The engine emits abstract
-- buttons ('OutButton'); this adapter applies the @whatsapp.ts:124-175@ decision
-- — @<=3 buttons AND none with a description@ → reply buttons, else a list — and
-- calls the kernel senders (which hardcode the wire constants and truncate every
-- field). Never throws: a Meta / transport failure resolves to @False@
-- (@whatsapp.ts:204-231@); a 24h-window close (131047) is logged distinctly (L10).
-- The 'MetaCfg' (encrypted token) is closed over; the token is decrypted at send.
module WhatsappBot.Adapter.Sender (mkWaSender) where

import qualified Data.Text as T
import Environment (Flow)
import qualified Kernel.External.Meta as Meta
import Kernel.Prelude
import Kernel.Utils.Common (logError, logWarning)
import WhatsappBot.Handles (WaSender (..))
import WhatsappBot.Types ()

mkWaSender :: Meta.MetaCfg -> WaSender Flow
mkWaSender cfg =
  WaSender
    { sendText = \to body -> sendSafe "sendText" $ Meta.sendText cfg to body Nothing,
      sendButtons = \to body btns ->
        -- TS truthiness: an empty-string description does NOT count as a description
        -- (whatsapp.ts uses `buttons.some(b => b.description)`).
        let hasDesc = any (maybe False (not . T.null) . (.btnDesc)) btns
         in if length btns <= 3 && not hasDesc
              then
                sendSafe "sendButtons" $
                  Meta.sendInteractiveButtons cfg to Nothing body Nothing (map (\b -> (b.btnId, b.btnTitle)) btns)
              else
                let isEstimates = any (\b -> "estimate:" `T.isPrefixOf` b.btnId) btns
                    listLabel = if isEstimates then "View Rides" else "View options"
                    sectionTitle = if isEstimates then "View Rides" else "Options"
                    rows = map (\b -> (b.btnId, b.btnTitle, b.btnDesc)) (take 10 btns)
                 in sendSafe "sendList" $
                      Meta.sendInteractiveList cfg to Nothing body Nothing listLabel [(Just sectionTitle, rows)],
      sendLocationRequest = \to body -> sendSafe "sendLocationRequest" $ Meta.sendLocationRequest cfg to body,
      sendVideo = \to link mCaption -> sendSafe "sendVideo" $ Meta.sendVideo cfg to (Left link) mCaption
    }

-- | Send, never throw. A typed 'MetaError' → False (131047 logged distinctly);
-- any transport/other exception → False.
sendSafe :: Text -> Flow Meta.MetaSendMessageResp -> Flow Bool
sendSafe label act =
  ( do
      res <- try @_ @Meta.MetaError act
      case res of
        Right _ -> pure True
        Left e -> do
          if Meta.needsTemplateReopen e
            then logWarning $ "whatsapp-bot/" <> label <> ": 24h re-engagement window closed (131047), not delivered: " <> show e
            else logError $ "whatsapp-bot/" <> label <> ": Meta error: " <> show e
          pure False
  )
    `catch` \(se :: SomeException) -> do
      logError $ "whatsapp-bot/" <> label <> ": send failed: " <> show se
      pure False
