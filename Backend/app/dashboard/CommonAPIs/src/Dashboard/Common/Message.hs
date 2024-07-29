{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.Common.Message where

import Data.Aeson
import Data.OpenApi hiding (description, name, password, summary, title, url)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)

-- we need to save endpoint transactions only for POST, PUT, DELETE APIs
data MessageEndpoint
  = UploadFileEndpoint
  | AddLinkEndpoint
  | AddMessageEndpoint
  | SendMessageEndpoint
  | MessageListEndpoint
  | MessageInfoEndpoint
  | MessageDeliveryInfoEndpoint
  | MessageReceiverListEndpoint
  | PostMessageUploadFileEndpoint
  | PostMessageAddLinkEndpoint
  | PostMessageAddEndpoint
  | PostMessageSendEndpoint
  deriving (Show, Read, ToJSON, FromJSON, Generic, Eq, Ord, ToSchema)

derivePersistField "MessageEndpoint"
