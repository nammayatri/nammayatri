{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Pre-ticket support bot: proxies the Xyne Conversation API so the app never
-- holds the Xyne app token. The workflow id and token come from the merchant's
-- XyneSpaces ticket service config; the rider's identity, language and ride
-- are injected here as the workflow's start context. The turn is returned as
-- Xyne sends it; the app renders it and falls back to the regular ticket flow
-- when the conversation finishes or hands off.
module Domain.Action.UI.SupportBot
  ( postSupportBotConversation,
    postSupportBotConversationEnd,
    probeSupportBot,
  )
where

import qualified API.Types.UI.SupportBot as Types
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import Domain.Types.MerchantServiceConfig
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.External.Ticket.Interface as TI
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import qualified Kernel.External.Ticket.Types as TicketTypes
import qualified Kernel.External.Ticket.XyneSpaces.Config as Xyne
import qualified Kernel.External.Ticket.XyneSpaces.Conversation as XC
import Kernel.External.Types (Language (..))
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson
import Tools.Error

postSupportBotConversation ::
  ( ( Maybe (Id DP.Person),
      Id DM.Merchant
    ) ->
    Types.SupportBotConversationReq ->
    Flow A.Value
  )
postSupportBotConversation (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  cfg <- xyneConfig merchantId person
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let isStart = isNothing req.conversationId
  when isStart $ void $ cfg.botWorkflowId & fromMaybeM (InternalError "Support bot workflow is not configured for this merchant")
  whenJust req.conversationId $ assertConversationOwner personId
  let xyneReq =
        XC.XyneConversationReq
          { XC.workflowId = Nothing, -- filled from the config by the interface layer
            XC.context = if isStart then Just (startContext merchant person req) else Nothing,
            XC.conversationId = req.conversationId,
            XC.turnId = req.turnId,
            XC.choice = req.choice,
            XC.text = req.text,
            XC.values = req.values
          }
  resp <- TI.advanceBotConversation (Ticket.XyneSpacesConfig cfg) xyneReq
  when isStart $ whenJust (responseConversationId resp) $ \cid -> Hedis.setExp (conversationOwnerKey cid) personId.getId conversationOwnerTtl
  pure resp

postSupportBotConversationEnd ::
  ( ( Maybe (Id DP.Person),
      Id DM.Merchant
    ) ->
    Text ->
    Flow APISuccess
  )
postSupportBotConversationEnd (mbPersonId, merchantId) conversationId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  cfg <- xyneConfig merchantId person
  assertConversationOwner personId conversationId
  TI.endBotConversation (Ticket.XyneSpacesConfig cfg) conversationId
  pure Success

-- | Conversations are bound to the rider who started them: the id alone must not
-- let another rider read, advance or end someone else's conversation.
-- | Is the pre-ticket bot usable for this rider right now? Gated by the
-- city's @enableXyneSupportBot@ rider config, then verified end to end by
-- starting a conversation with no ride and ending it straight away, so the
-- category list can switch the app to the bot only when Xyne actually
-- answers. Never throws: any failure reports @False@ and the app shows the
-- category list as before.
probeSupportBot :: Id DP.Person -> Id DM.Merchant -> Maybe Language -> Flow Bool
probeSupportBot personId merchantId mbLanguage = do
  result <- withTryCatch "supportBot:probe" $ do
    person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    riderConfig <-
      getConfig (RiderConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing
        >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
    if riderConfig.enableXyneSupportBot /= Just True
      then pure False
      else do
        let language = fromMaybe ENGLISH (mbLanguage <|> person.language)
            context =
              A.object
                [ ("ride_id", A.Null),
                  ("language", A.toJSON (show language :: Text)),
                  ("sos_clicked", A.Bool False)
                ]
            req =
              Types.SupportBotConversationReq
                { context = Just context,
                  conversationId = Nothing,
                  turnId = Nothing,
                  choice = Nothing,
                  text = Nothing,
                  values = Nothing
                }
        resp <- postSupportBotConversation (Just personId, merchantId) req
        case responseConversationId resp of
          Nothing -> pure False
          Just cid -> do
            endResult <- withTryCatch "supportBot:probeEnd" $ postSupportBotConversationEnd (Just personId, merchantId) cid
            case endResult of
              Left err -> logWarning $ "Support bot probe: could not end conversation " <> cid <> ": " <> show err
              Right _ -> pure ()
            pure True
  case result of
    Right ok -> pure ok
    Left err -> do
      logWarning $ "Support bot probe failed for person " <> personId.getId <> ": " <> show err
      pure False

conversationOwnerKey :: Text -> Text
conversationOwnerKey conversationId = "SupportBot:conversation:" <> conversationId <> ":owner"

conversationOwnerTtl :: Int
conversationOwnerTtl = 24 * 60 * 60

assertConversationOwner :: Id DP.Person -> Text -> Flow ()
assertConversationOwner personId conversationId = do
  mbOwner :: Maybe Text <- Hedis.get (conversationOwnerKey conversationId)
  unless (mbOwner == Just personId.getId) $ throwError (InvalidRequest "Conversation not found")

responseConversationId :: A.Value -> Maybe Text
responseConversationId (A.Object o) = case KM.lookup "conversationId" o of
  Just (A.String cid) -> Just cid
  _ -> Nothing
responseConversationId _ = Nothing

-- | The app sends whatever the workflow declares under "Started with"; the
-- server pins the rider's identity, name, city and merchant from its own records,
-- so the app cannot spoof them and need not know them.
startContext :: DM.Merchant -> DP.Person -> Types.SupportBotConversationReq -> A.Value
startContext merchant person req =
  A.Object (foldr (\(k, v) -> KM.insert k v) base pinned)
  where
    base = case req.context of
      Just (A.Object o) -> o
      _ -> mempty
    pinned =
      [ ("customer_id", A.toJSON person.id.getId),
        ("customer_name", A.toJSON (T.unwords (catMaybes [person.firstName, person.middleName, person.lastName]))),
        ("city", A.toJSON (show person.currentCity :: Text)),
        ("merchant", A.toJSON merchant.shortId.getShortId)
      ]
        <> maybe [] (\partner -> [("driver_merchant", A.toJSON partner)]) merchant.driverOfferMerchantShortId

xyneConfig :: Id DM.Merchant -> DP.Person -> Flow Xyne.XyneSpacesCfg
xyneConfig merchantId person = do
  merchantServiceConfig <-
    getOneConfig
      ( MerchantServiceConfigDimensions
          { merchantOperatingCityId = person.merchantOperatingCityId.getId,
            merchantId = merchantId.getId,
            serviceName = Just (IssueTicketService TicketTypes.XyneSpaces)
          }
      )
      Nothing
      >>= fromMaybeM (InternalError "XyneSpaces service config not found for this merchant")
  case merchantServiceConfig.serviceConfig of
    IssueTicketServiceConfig (Ticket.XyneSpacesConfig cfg) -> pure cfg
    _ -> throwError $ InternalError "Unexpected XyneSpaces service config type"
