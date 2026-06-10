{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Communication where

import qualified Data.Text as T
-- Communication-engine beam imports

import qualified Lib.CommunicationEngine.Storage.Beam.Communication as BeamCommunication
import qualified Lib.CommunicationEngine.Storage.Beam.CommunicationDelivery as BeamCommunicationDelivery
import Tools.Beam.UtilsTH (HasSchemaName (..), currentSchemaName)

instance HasSchemaName BeamCommunication.CommunicationT where
  schemaName _ = T.pack currentSchemaName

instance HasSchemaName BeamCommunicationDelivery.CommunicationDeliveryT where
  schemaName _ = T.pack currentSchemaName
