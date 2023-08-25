{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Message.MessageReport where

import qualified Data.Aeson as A
import Data.ByteString
import qualified Data.Map as M
import Data.OpenApi
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.Message.Message as Msg
import Domain.Types.Person (Driver)
import Kernel.Prelude
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Types.Id

type MessageDynamicFieldsType = M.Map Text Text

fromFieldmessageDynamicFields ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion MessageDynamicFieldsType
fromFieldmessageDynamicFields f mbValue = do
  value' <- fromField f mbValue
  case A.fromJSON value' of
    A.Success a -> pure a
    _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed"

instance FromField MessageDynamicFieldsType where
  fromField = fromFieldmessageDynamicFields

instance HasSqlValueSyntax be A.Value => HasSqlValueSyntax be MessageDynamicFieldsType where
  sqlValueSyntax = sqlValueSyntax . A.toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be MessageDynamicFieldsType

instance FromField (Text, Text) => FromBackendRow Postgres MessageDynamicFieldsType

instance IsString MessageDynamicFieldsType where
  fromString = show

data DeliveryStatus = Success | Failed | Queued | Sending deriving (Generic, ToSchema, Show, Read, ToJSON, FromJSON, Eq)

instance FromField DeliveryStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be DeliveryStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DeliveryStatus

instance FromBackendRow Postgres DeliveryStatus

instance IsString DeliveryStatus where
  fromString = show

deriving stock instance Ord DeliveryStatus

data MessageReport = MessageReport
  { messageId :: Id Msg.Message,
    driverId :: Id Driver,
    deliveryStatus :: DeliveryStatus,
    readStatus :: Bool,
    likeStatus :: Bool,
    reply :: Maybe Text,
    messageDynamicFields :: MessageDynamicFieldsType,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
