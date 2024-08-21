{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Select (buildSelectReqV2) where

import qualified Beckn.Types.Core.Taxi.API.Select as Select
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import BecknV2.OnDemand.Utils.Common as OnDemandUtils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified BecknV2.Utils as Utils
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Domain.Action.Beckn.Select as DSelect
import qualified Domain.Types.Location as Location
import Domain.Types.Trip as Trip
import Kernel.Prelude hiding (error, setField)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import SharedLogic.CallBAP (mkTxnIdKey)
import Tools.Error
import Tools.Metrics (CoreMetrics)

buildSelectReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    CoreMetrics m,
    CacheFlow m r
  ) =>
  Subscriber.Subscriber ->
  Select.SelectReqV2 ->
  m DSelect.DSelectReq
buildSelectReqV2 subscriber req = do
  let context = req.selectReqContext
  ContextV2.validateContext Context.SELECT context
  now <- getCurrentTime
  bap_id <- context.contextBapId & fromMaybeM (InvalidRequest "Missing bap_id")
  bap_uriText <- context.contextBapUri & fromMaybeM (InvalidRequest "Missing bap_uri")
  bap_uri <- parseBaseUrl bap_uriText
  let order = req.selectReqMessage.confirmReqMessageOrder
  unless (subscriber.subscriber_id == bap_id) $
    throwError (InvalidRequest "Invalid bap_id")
  unless (subscriber.subscriber_url == bap_uri) $
    throwError (InvalidRequest "Invalid bap_uri")
  messageUuid <- context.contextMessageId & fromMaybeM (InvalidRequest "Missing message_id")
  transactionUuid <- context.contextTransactionId & fromMaybeM (InvalidRequest "Missing transaction_id")
  let messageId = UUID.toText messageUuid
      transactionId = UUID.toText transactionUuid
  void $ cacheSelectMessageId messageId transactionId
  item <- case order.orderItems of
    Just [item] -> pure item
    _ -> throwError $ InvalidRequest "There should be only one item"
  let customerExtraFee = getCustomerExtraFeeV2 item.itemTags
      autoAssignEnabled = getAutoAssignEnabledV2 item.itemTags
      isAdvancedBoookingEnabled = getAdvancedBookingEnabled item.itemTags
      bookAnyEstimates = getBookAnyEstimates item.itemTags
      (toUpdateDeviceIdInfo, isMultipleOrNoDeviceIdExist) = getDeviceIdInfo item.itemTags
  fulfillment <- case order.orderFulfillments of
    Just [fulfillment] -> pure $ Just fulfillment
    _ -> pure Nothing
  let tripCategory = fulfillment >>= \fm -> OnDemandUtils.fulfillmentTypeToTripCategory <$> fm.fulfillmentType
  dselectReqDetails <- case tripCategory of
    Just (Trip.Delivery _) -> do
      let deliveryDetails = getDeliveryDetails item.itemTags
      pure $ DSelect.DSelectReqDeliveryDetails <$> deliveryDetails
    _ -> pure Nothing
  estimateIdText <- getEstimateId fulfillment item & fromMaybeM (InvalidRequest "Missing item_id")
  let customerPhoneNum = getCustomerPhoneNumber fulfillment
  pure
    DSelect.DSelectReq
      { messageId = messageId,
        transactionId = transactionId,
        bapId = subscriber.subscriber_id,
        bapUri = subscriber.subscriber_url,
        pickupTime = now,
        autoAssignEnabled = autoAssignEnabled,
        customerExtraFee = customerExtraFee,
        estimateIds = [Id estimateIdText] <> (maybe [] (map Id) bookAnyEstimates),
        customerPhoneNum = customerPhoneNum,
        isAdvancedBookingEnabled = isAdvancedBoookingEnabled,
        isMultipleOrNoDeviceIdExist = isMultipleOrNoDeviceIdExist,
        toUpdateDeviceIdInfo = toUpdateDeviceIdInfo,
        selectReqDetails = dselectReqDetails
      }

getBookAnyEstimates :: Maybe [Spec.TagGroup] -> Maybe [Text]
getBookAnyEstimates tagGroups = do
  tagValue <- Utils.getTagV2 Tag.ESTIMATIONS Tag.OTHER_SELECT_ESTIMATES tagGroups
  readMaybe $ T.unpack tagValue

getCustomerExtraFeeV2 :: Maybe [Spec.TagGroup] -> Maybe HighPrecMoney
getCustomerExtraFeeV2 tagGroups = do
  tagValue <- Utils.getTagV2 Tag.CUSTOMER_TIP_INFO Tag.CUSTOMER_TIP tagGroups
  highPrecMoneyFromText tagValue

getAutoAssignEnabledV2 :: Maybe [Spec.TagGroup] -> Bool
getAutoAssignEnabledV2 tagGroups =
  let tagValue = Utils.getTagV2 Tag.AUTO_ASSIGN_ENABLED Tag.IS_AUTO_ASSIGN_ENABLED tagGroups
   in case tagValue of
        Just "True" -> True
        Just "False" -> False
        _ -> False

getAdvancedBookingEnabled :: Maybe [Spec.TagGroup] -> Bool
getAdvancedBookingEnabled tagGroups =
  let tagValue = Utils.getTagV2 Tag.FORWARD_BATCHING_REQUEST_INFO Tag.IS_FORWARD_BATCH_ENABLED tagGroups
   in case tagValue of
        Just "True" -> True
        Just "False" -> False
        _ -> False

getDeviceIdInfo :: Maybe [Spec.TagGroup] -> (Bool, Maybe Bool)
getDeviceIdInfo tagGroups = do
  let isMultipleDeviceIdExist = case (Utils.getTagV2 Tag.DEVICE_ID_INFO Tag.DEVICE_ID_FLAG tagGroups) of
        Just "True" -> Just True
        Just "False" -> Just False
        _ -> Nothing
  let toUpdateDeviceIdInfo = case (Utils.getTagV2 Tag.DEVICE_ID_INFO Tag.TO_UPDATE_DEVICE_ID tagGroups) of
        Just "True" -> True
        Just "False" -> False
        _ -> False
  (toUpdateDeviceIdInfo, isMultipleDeviceIdExist)

cacheSelectMessageId :: CacheFlow m r => Text -> Text -> m ()
cacheSelectMessageId messageId transactionId = do
  let msgKey = mkTxnIdKey transactionId
  Hedis.setExp msgKey messageId 3600

getCustomerPhoneNumber :: Maybe Spec.Fulfillment -> Maybe Text
getCustomerPhoneNumber (Just fulfillment) = fulfillment.fulfillmentCustomer >>= (.customerContact) >>= (.contactPhone)
getCustomerPhoneNumber Nothing = Nothing

-- NOTE : This is a temporary function to keep the code backward compatible
-- But this assumes that we will be sending fulfillmentId empty from next release? Change it maybe?
getEstimateId :: Maybe Spec.Fulfillment -> Spec.Item -> Maybe Text
getEstimateId (Just fulfillment) item = do
  case fulfillment.fulfillmentId of
    Just fulfillmentId -> Just fulfillmentId
    Nothing -> item.itemId
getEstimateId Nothing item = item.itemId

getDeliveryDetails :: Maybe [Spec.TagGroup] -> Maybe DSelect.DeliveryDetails
getDeliveryDetails tagGroups = do
  initiatedAs <- Utils.getTagV2 Tag.DELIVERY Tag.INITIATED_AS tagGroups
  senderName <- Utils.getTagV2 Tag.DELIVERY Tag.SENDER_NAME tagGroups
  senderPhone <- Utils.getTagV2 Tag.DELIVERY Tag.SENDER_NUMBER tagGroups
  senderLocIns <- Utils.getTagV2 Tag.DELIVERY Tag.SENDER_LOCATION_INSTRUCTIONS tagGroups
  receiverName <- Utils.getTagV2 Tag.DELIVERY Tag.RECEIVER_NAME tagGroups
  receiverPhone <- Utils.getTagV2 Tag.DELIVERY Tag.RECEIVER_NUMBER tagGroups
  receiverLocIns <- Utils.getTagV2 Tag.DELIVERY Tag.RECEIVER_LOCATION_INSTRUCTIONS tagGroups
  let (senderInstructions, senderAddressExtra) = splitInstructions senderLocIns
      (receiverInstructions, receiverAddressExtra) = splitInstructions receiverLocIns
      initiatedAsEnum = fromMaybe Enums.Sender (readMaybe @(Enums.DeliveryInitiation) $ T.unpack initiatedAs)
  pure $
    DSelect.DeliveryDetails
      { DSelect.senderDetails =
          DSelect.PersonDetails
            { DSelect.name = senderName,
              DSelect.phone = senderPhone,
              DSelect.address =
                Location.LocationAddress
                  { Location.extras = senderAddressExtra,
                    Location.instructions = senderInstructions,
                    Location.area = Nothing,
                    Location.areaCode = Nothing,
                    Location.building = Nothing,
                    Location.city = Nothing,
                    Location.country = Nothing,
                    Location.door = Nothing,
                    Location.fullAddress = Nothing,
                    Location.state = Nothing,
                    Location.street = Nothing
                  }
            },
        DSelect.receiverDetails =
          DSelect.PersonDetails
            { DSelect.name = receiverName,
              DSelect.phone = receiverPhone,
              DSelect.address =
                Location.LocationAddress
                  { Location.extras = receiverAddressExtra,
                    Location.instructions = receiverInstructions,
                    Location.area = Nothing,
                    Location.areaCode = Nothing,
                    Location.building = Nothing,
                    Location.city = Nothing,
                    Location.country = Nothing,
                    Location.door = Nothing,
                    Location.fullAddress = Nothing,
                    Location.state = Nothing,
                    Location.street = Nothing
                  }
            },
        DSelect.initiatedAs = initiatedAsEnum
      }
  where
    correctIns str = if T.null str then Nothing else Just str
    splitInstructions :: Text -> (Maybe Text, Maybe Text)
    splitInstructions ins = case T.splitOn "|" ins of
      [ins1, ins2] -> (correctIns ins1, correctIns ins2)
      _ -> (Nothing, Nothing)
