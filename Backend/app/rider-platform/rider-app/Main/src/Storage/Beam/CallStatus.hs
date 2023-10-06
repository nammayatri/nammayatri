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

module Storage.Beam.CallStatus where

import qualified Database.Beam as B
import qualified Kernel.External.Call.Interface as CallTypes
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH as TH

data CallStatusT f = CallStatusT
  { id :: B.C f Text,
    callId :: B.C f Text,
    rideId :: B.C f Text,
    dtmfNumberUsed :: B.C f (Maybe Text),
    status :: B.C f CallTypes.CallStatus,
    recordingUrl :: B.C f (Maybe Text),
    conversationDuration :: B.C f Int,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CallStatusT where
  data PrimaryKey CallStatusT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type CallStatus = CallStatusT Identity

$(TH.enableKVPG ''CallStatusT ['id] [['callId]])

$(TH.mkTableInstances ''CallStatusT "call_status")
