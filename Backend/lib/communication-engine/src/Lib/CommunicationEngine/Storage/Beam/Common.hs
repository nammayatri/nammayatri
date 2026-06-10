module Lib.CommunicationEngine.Storage.Beam.Common where

import qualified Database.Beam as B
import Kernel.Prelude (Generic)
import Lib.CommunicationEngine.Storage.Beam.BeamFlow (BeamFlow')
import qualified Lib.CommunicationEngine.Storage.Beam.Communication as BeamComm
import qualified Lib.CommunicationEngine.Storage.Beam.CommunicationDelivery as BeamCommDel

data CommunicationEngineDB f = CommunicationEngineDB
  { communication :: f (B.TableEntity BeamComm.CommunicationT),
    communicationDelivery :: f (B.TableEntity BeamCommDel.CommunicationDeliveryT)
  }
  deriving (Generic, B.Database be)

communicationEngineDB :: BeamFlow' => B.DatabaseSettings be CommunicationEngineDB
communicationEngineDB =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { communication = BeamComm.communicationTable,
        communicationDelivery = BeamCommDel.communicationDeliveryTable
      }
