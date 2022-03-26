module API.Confirm.Handler where

import API.Common
import qualified API.Confirm.Types as Confirm
import API.Order
import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Control.Lens (element)
import qualified Data.Text as T
import qualified Domain.Address as DAddress
import qualified Domain.Delivery as DDelivery
import qualified Domain.DunzoCreds as DDunzoCreds
import qualified Domain.Organization as DOrg
import qualified Domain.Person as DPerson
import EulerHS.Prelude hiding (id, state)
import qualified ExternalAPI.Dunzo.Flow as DzAPI
import qualified ExternalAPI.Dunzo.Types as Dz
import qualified Storage.Queries.Address as QAddress
import qualified Storage.Queries.Delivery as QDelivery
import qualified Storage.Queries.Person as QPerson
import qualified Types.Beckn.API.OnConfirm as OnConfirm
import qualified Types.Beckn.Address as Address
import Types.Beckn.Context
import qualified Types.Beckn.FulfillmentDetails as FDetails
import Types.Error
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
  DOrg.Organization ->
  BecknReq Confirm.OrderObject ->
  Flow AckResponse
confirm org req = do
  dconf@DunzoConfig {..} <- asks (.dzConfig)
  let confirmOrder = req.message.order
  dzBACreds <- getCreds org.dunzoCredsId
  orderId <- generateGUID
  now <- getCurrentTime
  categoryId <- getCategoryId confirmOrder
  taskReq <- mkCreateTaskReq orderId confirmOrder categoryId
  withCallback CONFIRM OnConfirm.onConfirmAPI req.context req.context.bap_uri do
    taskStatus <- createTaskAPI dzBACreds dconf taskReq
    let onConfirmOrder = mkOnConfirmOrder confirmOrder orderId now taskStatus
    checkAndLogPriceDiff confirmOrder onConfirmOrder
    sender <- buildPerson onConfirmOrder DAddress.Pickup
    receiver <- buildPerson onConfirmOrder DAddress.Drop
    pickupAddress <- buildAddress onConfirmOrder DAddress.Pickup
    dropAddress <- buildAddress onConfirmOrder DAddress.Drop
    let delivery = mkDeliveryEntity req.context.transaction_id org.id onConfirmOrder taskStatus now sender.id receiver.id pickupAddress.id dropAddress.id categoryId
    Esq.runTransaction $ do
      QPerson.create sender
      QPerson.create receiver
      QAddress.create pickupAddress
      QAddress.create dropAddress
      QDelivery.create delivery
    return $ OnConfirm.OrderObject onConfirmOrder
  where
    createTaskAPI dzBACreds@DDunzoCreds.DunzoCreds {..} conf@DunzoConfig {..} req' = do
      token <- fetchToken dzBACreds conf
      DzAPI.createTask clientId token dzUrl dzTestMode req'

    checkAndLogPriceDiff confirmOrder onConfirmOrder = do
      let orderId = onConfirmOrder.id
      let initPrice = Confirm.convertDecimalValueToAmount =<< confirmOrder.payment.params.amount
      let confirmPrice = Confirm.convertDecimalValueToAmount =<< onConfirmOrder.payment.params.amount
      case (initPrice, confirmPrice) of
        (Just initAmount, Just confirmAmount) -> do
          when (initAmount /= confirmAmount) $
            logTagInfo ("Order_" <> orderId) ("Price diff of amount " <> show (confirmAmount - initAmount))
        _ -> pass

mkDeliveryEntity ::
  Text ->
  Id DOrg.Organization ->
  OnConfirm.Order ->
  Dz.TaskStatus ->
  UTCTime ->
  Id DPerson.Person ->
  Id DPerson.Person ->
  Id DAddress.Address ->
  Id DAddress.Address ->
  Int ->
  DDelivery.DeliveryEntity
mkDeliveryEntity txnId orgId onConfirmOrder taskStatus now senderId receiverId pickupAddressId dropAddressId categoryId = do
  DDelivery.DeliveryEntity
    { id = Id txnId,
      orderId = onConfirmOrder.id,
      deliveryServiceOrderId = taskStatus.task_id.getTaskId,
      bapId = orgId,
      status = taskStatus.state,
      senderId = senderId,
      receiverId = receiverId,
      pickupAddressId = pickupAddressId,
      dropAddressId = dropAddressId,
      categoryId = categoryId,
      deliveryPrice = taskStatus.estimated_price,
      createdAt = now,
      updatedAt = now
    }

getFulfillmentDetails :: OnConfirm.Order -> DAddress.Stop -> FDetails.FulfillmentDetails
getFulfillmentDetails onConfirmOrder stop = case stop of
  DAddress.Pickup -> onConfirmOrder.fulfillment.start
  DAddress.Drop -> onConfirmOrder.fulfillment.end

buildPerson :: MonadGuid m => OnConfirm.Order -> DAddress.Stop -> m DPerson.Person
buildPerson onConfirmOrder stop = do
  id <- generateGUID
  let details = getFulfillmentDetails onConfirmOrder stop
  pure
    DPerson.Person
      { id = id,
        name = details.person.name,
        phone = details.contact.phone
      }

buildAddress :: MonadFlow m => OnConfirm.Order -> DAddress.Stop -> m DAddress.Address
buildAddress onConfirmOrder stop = do
  id <- generateGUID
  let details = getFulfillmentDetails onConfirmOrder stop
  let Address.Address {..} = details.location.address
  pure
    DAddress.Address
      { id = id,
        lat = details.location.gps.lat,
        lon = details.location.gps.lon,
        pincode = area_code,
        instructions = details.instructions <&> (.name),
        ..
      }

getCategoryId :: MonadFlow m => Confirm.Order -> m Int
getCategoryId order = do
  packageCatId <-
    case order.items of
      [orderItem] -> pure orderItem.id
      _ -> throwError $ InvalidRequest "Exactly one order item expected."
  fromMaybeErr "INVALID_CATEGORY_ID" (Just CORE003) (readMaybe $ T.unpack packageCatId)

mkCreateTaskReq :: MonadFlow m => Text -> Confirm.Order -> Int -> m Dz.CreateTaskReq
mkCreateTaskReq orderId order categoryId = do
  let pickUpLoc = order.fulfillment.start.location
  let deliveryLoc = order.fulfillment.end.location
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
        { name = person.name,
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
      state = mapTaskStateToOrderState status.state,
      payment = mkPayment status.estimated_price,
      updated_at = now,
      fulfillment = uFulfillment,
      ..
    }
