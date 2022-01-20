module API.Confirm.Handler where

import API.Common
import qualified API.Confirm.Types as Confirm
import App.Types
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Control.Lens (element)
import qualified Data.Text as T
import EulerHS.Prelude
import qualified ExternalAPI.Dunzo.Flow as DzAPI
import qualified ExternalAPI.Dunzo.Types as Dz
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Types.Beckn.API.OnConfirm as OnConfirm
import Types.Beckn.Context
import Types.Error
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.SearchRequest as SSearchRequest
import Types.Wrapper
import Utils.Callback
import Utils.Common

handler :: SignatureAuthResult -> BecknReq Confirm.OrderObject -> FlowHandler AckResponse
handler (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext CONFIRM $ req.context
    validateBapUrl subscriber $ req.context
    confirm bapOrg req

confirm ::
  Org.Organization ->
  BecknReq Confirm.OrderObject ->
  Flow AckResponse
confirm org req = do
  dconf@DunzoConfig {..} <- asks (.dzConfig)
  let confirmOrder = req.message.order
  dzBACreds <- getDzBAPCreds org
  orderId <- generateGUID
  now <- getCurrentTime
  taskReq <- mkCreateTaskReq orderId confirmOrder
  withCallback CONFIRM OnConfirm.onConfirmAPI req.context req.context.bap_uri do
    taskStatus <- createTaskAPI dzBACreds dconf taskReq
    let onConfirmOrder = mkOnConfirmOrder confirmOrder orderId now taskStatus
    checkAndLogPriceDiff confirmOrder onConfirmOrder
    createSearchRequest orderId onConfirmOrder taskStatus now
    return $ OnConfirm.OrderObject onConfirmOrder
  where
    createSearchRequest orderId onConfirmOrder taskStatus now = do
      let searchRequestId = Id req.context.transaction_id
      let searchRequest =
            SSearchRequest.SearchRequest
              { id = searchRequestId,
                name = Nothing,
                description = Nothing,
                shortId = ShortId orderId,
                industry = SSearchRequest.GROCERY,
                _type = SSearchRequest.RIDEORDER,
                exchangeType = SSearchRequest.ORDER,
                status = SSearchRequest.CONFIRMED,
                startTime = now,
                endTime = Nothing,
                validTill = now,
                provider = Just "Dunzo",
                providerType = Nothing,
                requestor = Just org.id.getId,
                requestorType = Nothing,
                udf1 = Just $ encodeToText onConfirmOrder,
                udf2 = Just $ encodeToText taskStatus,
                udf3 = Nothing,
                udf4 = Nothing,
                udf5 = Nothing,
                info = Nothing,
                createdAt = now,
                updatedAt = now
              }
      QSearchRequest.create searchRequest

    createTaskAPI dzBACreds@DzBAConfig {..} conf@DunzoConfig {..} req' = do
      token <- fetchToken dzBACreds conf
      DzAPI.createTask dzClientId token dzUrl dzTestMode req'

    checkAndLogPriceDiff confirmOrder onConfirmOrder = do
      let orderId = onConfirmOrder.id
      let initPrice = Confirm.convertDecimalValueToAmount =<< confirmOrder.payment.params.amount
      let confirmPrice = Confirm.convertDecimalValueToAmount =<< onConfirmOrder.payment.params.amount
      case (initPrice, confirmPrice) of
        (Just initAmount, Just confirmAmount) -> do
          when (initAmount /= confirmAmount) $
            logTagInfo ("Order_" <> orderId) ("Price diff of amount " <> show (confirmAmount - initAmount))
        _ -> pass

mkCreateTaskReq :: MonadFlow m => Text -> Confirm.Order -> m Dz.CreateTaskReq
mkCreateTaskReq orderId order = do
  let pickUpLoc = order.fulfillment.start.location
  let deliveryLoc = order.fulfillment.end.location
  packageCatId <-
    case order.items of
      [orderItem] -> pure orderItem.id
      _ -> throwError $ InvalidRequest "Exactly one order item expected."
  let pickupDet = mkLocationDetails pickUpLoc
  let dropDet = mkLocationDetails deliveryLoc
  let pickUpPerson = order.fulfillment.start.person
  let pickUpContact = order.fulfillment.start.contact
  let receivingPerson = order.fulfillment.end.person
  let receivingContact = order.fulfillment.end.contact
  let senderDet = mkPersonDetails pickUpPerson pickUpContact
  let receiverDet = mkPersonDetails receivingPerson receivingContact
  let pickupInstructions = formatInstructions "pickup" order.fulfillment.start.instructions
  let dropInstructions = formatInstructions "drop" order.fulfillment.end.instructions
  packageContent <- do
    (categoryId :: Int) <- fromMaybeErr "INVALID_CATEGORY_ID" (Just CORE003) (readMaybe $ T.unpack packageCatId)
    -- Category id is the index value of dzPackageContentList
    Dz.dzPackageContentList ^? element (categoryId - 1)
      & fromMaybeErr "INVALID_CATEGORY_ID" (Just CORE003)
  return $
    Dz.CreateTaskReq
      { request_id = orderId,
        pickup_details = pickupDet,
        drop_details = dropDet,
        sender_details = senderDet,
        receiver_details = receiverDet,
        special_instructions = joinInstructions pickupInstructions dropInstructions,
        package_approx_value = Nothing, -- FIXME. Don't know where BAP can specify this in the new spec.
        package_content = [packageContent],
        reference_id = Nothing
      }
  where
    mkLocationDetails :: Confirm.Location -> Dz.LocationDetails
    mkLocationDetails location = do
      -- FIXME: Much of these can be optional I'm pretty sure.
      let address = location.address
      Dz.LocationDetails
        { lat = location.gps.lat,
          lng = location.gps.lon,
          address =
            Dz.Address
              { apartment_address = Just (address.door <> maybe "" (" " <>) address.name <> maybe "" (" " <>) address.building),
                street_address_1 = address.street,
                street_address_2 = "",
                landmark = Nothing,
                city = Just address.city,
                state = address.state,
                pincode = Just address.area_code,
                country = Just address.country
              }
        }

    mkPersonDetails person contact =
      Dz.PersonDetails
        { name = person.name.getName,
          phone_number = contact.phone
        }

    formatInstructions tag mbDescriptor = do
      descriptor <- mbDescriptor
      pure $ tag <> ": " <> descriptor.name

    joinInstructions pickupInstructions dropInstructions =
      let orderMsg = "Order " <> orderId
       in case (pickupInstructions, dropInstructions) of
            (Just pickupInst, Just dropInst) -> Just $ orderMsg <> ": " <> pickupInst <> " and " <> dropInst
            (Nothing, Just dropInst) -> Just $ orderMsg <> ": " <> dropInst
            (Just pickupInst, Nothing) -> Just $ orderMsg <> ": " <> pickupInst
            _ -> Just orderMsg

--TODO Which of these Order fields can really change?
mkOnConfirmOrder :: Confirm.Order -> Text -> UTCTime -> Dz.TaskStatus -> OnConfirm.Order
mkOnConfirmOrder order@Confirm.Order {..} orderId now status = do
  let Confirm.Fulfillment {..} = order.fulfillment
  let uFulfillment =
        OnConfirm.Fulfillment
          { id = status.task_id.getTaskId,
            ..
          }
  OnConfirm.Order
    { id = orderId,
      state = mapTaskStateToOrderState (status.state),
      payment = mkPayment status.estimated_price,
      updated_at = now,
      fulfillment = uFulfillment,
      ..
    }
