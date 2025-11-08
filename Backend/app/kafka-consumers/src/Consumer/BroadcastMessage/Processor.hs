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
import "dynamic-offer-driver-app" Domain.Types.Message as Types
import "dynamic-offer-driver-app" Domain.Types.MessageReport as Types
import Environment
import EulerHS.Prelude
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Types.Id
import Kernel.Types.Time
import Kernel.Utils.Common (withTryCatch)
import "dynamic-offer-driver-app" Storage.Queries.MessageReport as MRQuery
import "dynamic-offer-driver-app" Storage.Queries.Person as Person
import "dynamic-offer-driver-app" Tools.Notifications (sendMessageToDriver)

broadcastMessage :: Types.MessageDict -> Text -> Flow ()
broadcastMessage messageDict driverId = do
  mDriver <- Person.findById (Id driverId)
  whenJust mDriver $ \driver -> do
    let message = maybe messageDict.defaultMessage (flip (HM.findWithDefault messageDict.defaultMessage) messageDict.translations . show) driver.language
    exep <- withTryCatch "sendMessageToDriver" (sendMessageToDriver driver.merchantOperatingCityId FCM.SHOW (Just FCM.HIGH) FCM.NEW_MESSAGE message.title message.shortDescription driver message.id)
    let dStatus = case exep of
          Left _ -> Types.Failed
          Right _ -> Types.Success
    mkMessageReport dStatus driverId messageDict.defaultMessage.id
  where
    mkMessageReport status drvrId messageId = do
      now <- getCurrentTime
      let messageReport =
            Types.MessageReport
              { driverId = Id drvrId,
                messageId,
                deliveryStatus = status,
                readStatus = False,
                likeStatus = False,
                messageDynamicFields = HM.empty,
                reply = Nothing,
                createdAt = now,
                updatedAt = now
              }
      void $ withTryCatch "createMessageReport" $ MRQuery.create messageReport
