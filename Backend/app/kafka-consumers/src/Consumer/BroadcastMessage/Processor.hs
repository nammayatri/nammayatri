{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
import Tools.Notifications (sendMessageToDriver)

broadcastMessage :: Types.MessageDict -> Text -> Flow ()
broadcastMessage messageDict driverId = do
  mDriver <- Esq.runInReplica $ Person.findById (Proxy @Flow) (Id driverId)
  status <-
    case mDriver of
      Just driver -> do
        let message = maybe messageDict.defaultMessage (flip (HM.findWithDefault messageDict.defaultMessage) messageDict.translations . show) driver.language
        Esq.runTransaction $ MRQuery.updateDeliveryStatusByMessageIdAndDriverId @Flow message.id (Id driverId) Types.Sending
        exep <- try @_ @SomeException (sendMessageToDriver driver.merchantId FCM.SHOW Nothing FCM.NEW_MESSAGE message.title message.description driver.id message.id driver.deviceToken)
        return $
          case exep of
            Left _ -> Types.Failed
            Right _ -> Types.Success
      Nothing -> return Types.Failed
  void $ Esq.runTransaction $ MRQuery.updateDeliveryStatusByMessageIdAndDriverId @Flow messageDict.defaultMessage.id (Id driverId) status
