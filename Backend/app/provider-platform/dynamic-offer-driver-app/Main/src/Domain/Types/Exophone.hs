{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Exophone where

import qualified Domain.Types.Merchant as DM
import Kernel.External.Call.Types (CallService)
import Kernel.Prelude
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data Exophone = Exophone
  { id :: Id Exophone,
    merchantId :: Id DM.Merchant,
    primaryPhone :: Text,
    backupPhone :: Text,
    isPrimaryDown :: Bool,
    exophoneType :: ExophoneType,
    -- isBackupDown Bool -- do we need this?
    callService :: CallService,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data ExophoneType = CALL_RIDE | END_RIDE
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''ExophoneType)
