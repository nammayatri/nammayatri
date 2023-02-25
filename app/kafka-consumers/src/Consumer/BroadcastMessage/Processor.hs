module Consumer.BroadcastMessage.Processor
  ( broadcastMessage,
  )
where

import qualified Data.Map as HM
import qualified Domain.Types.Message.Message as Types
import qualified Domain.Types.Message.MessageReport as Types
import Environment
import EulerHS.Prelude
import qualified Kernel.External.FCM.Types as FCM
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Queries.Message.MessageReport as MRQuery
import qualified Storage.Queries.Person as Person
import Tools.Notifications (sendNotificationToDriver)

broadcastMessage :: Types.MessageDict -> Text -> Flow ()
broadcastMessage messageDict driverId = do
  mDriver <- Esq.runInReplica $ Person.findById (Id driverId)
  status <-
    case mDriver of
      Just driver -> do
        let message = maybe messageDict.defaultMessage (flip (HM.findWithDefault messageDict.defaultMessage) messageDict.translations . show) driver.language
        Esq.runTransaction $ MRQuery.updateDeliveryStatusByMessageIdAndDriverId message.id (Id driverId) Types.Sending
        exep <- try @_ @SomeException (sendNotificationToDriver driver.merchantId FCM.SHOW Nothing FCM.NEW_MESSAGE message.title message.description driver.id driver.deviceToken)
        return $
          case exep of
            Left _ -> Types.Failed
            Right _ -> Types.Success
      Nothing -> return Types.Failed
  void $ Esq.runTransaction $ MRQuery.updateDeliveryStatusByMessageIdAndDriverId messageDict.defaultMessage.id (Id driverId) status
