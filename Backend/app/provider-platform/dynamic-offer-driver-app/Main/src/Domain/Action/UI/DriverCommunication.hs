module Domain.Action.UI.DriverCommunication
  ( getDriverCommunicationList,
    postDriverCommunicationRead,
  )
where

import qualified API.Types.UI.DriverCommunication as API
import qualified Data.Map.Strict as M
import qualified Domain.Types.Communication as DComm
import qualified Domain.Types.CommunicationDelivery as DDelivery
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Communication as QComm
import qualified Storage.Queries.CommunicationDelivery as QDelivery
import Tools.Error

-- | Only CH_PUSH belongs in an in-app feed; an SMS or WhatsApp delivery row left
-- the app entirely and the driver already has it in their inbox.
driverFeedChannel :: DComm.ChannelType
driverFeedChannel = DComm.CH_PUSH

getDriverCommunicationList ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Maybe Int ->
    Maybe Int ->
    Flow API.DriverCommunicationListRes
  )
getDriverCommunicationList (mbPersonId, _, _) mbLimit mbOffset = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Authenticated person required")
  deliveries <- B.runInReplica $ QDelivery.findByRecipientIdWithChannel personId (Just driverFeedChannel) mbLimit mbOffset Nothing Nothing
  communications <- B.runInReplica $ QComm.findAllByIds (map (.communicationId) deliveries)
  unreadCount <- B.runInReplica $ QDelivery.countUnreadByRecipientIdAndChannel personId driverFeedChannel
  let communicationById = M.fromList $ map (\communication -> (communication.id, communication)) communications
      items = mapMaybe (mkItem communicationById) deliveries
      orphanCount = length deliveries - length items
  when (orphanCount > 0) $
    logWarning $ "DriverCommunication: " <> show orphanCount <> " delivery rows without a communication for driver " <> personId.getId
  pure $
    API.DriverCommunicationListRes
      { communications = items,
        unreadCount,
        hasMore = length deliveries >= QDelivery.effectiveRecipientFeedLimit mbLimit
      }
  where
    mkItem communicationById delivery = do
      communication <- M.lookup delivery.communicationId communicationById
      pure $
        API.DriverCommunicationItem
          { communicationId = communication.id,
            title = communication.title,
            body = communication.body,
            senderName = communication.senderDisplayName,
            isRead = isJust delivery.readAt,
            sentAt = delivery.createdAt
          }

postDriverCommunicationRead ::
  ( ( Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Id DComm.Communication ->
    Flow APISuccess
  )
postDriverCommunicationRead (mbPersonId, _, _) communicationId = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Authenticated person required")
  -- Looked up by the caller's own id, so a driver can never mark another's row read.
  delivery <-
    QDelivery.findByRecipientIdAndCommunicationIdAndChannel personId communicationId driverFeedChannel
      >>= fromMaybeM (InvalidRequest "No notification for this driver")
  when (isNothing delivery.readAt) $ do
    now <- getCurrentTime
    let readStatus = if delivery.status `elem` [DDelivery.DS_SENT, DDelivery.DS_DELIVERED] then DDelivery.DS_READ else delivery.status
    QDelivery.updateStatusAndReadAt readStatus (Just now) personId communicationId driverFeedChannel
  pure Success
