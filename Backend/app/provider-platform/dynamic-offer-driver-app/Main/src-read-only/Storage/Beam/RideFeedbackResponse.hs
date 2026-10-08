{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RideFeedbackResponse where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.RideFeedbackResponse
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RideFeedbackResponseT f = RideFeedbackResponseT
  { actionResults :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    answer :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    bookingId :: (B.C f Kernel.Prelude.Text),
    configId :: (B.C f Kernel.Prelude.Text),
    configPilotVersions :: (B.C f (Kernel.Prelude.Maybe [Kernel.Prelude.Int])),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    driverId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    lat :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    logicVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    lon :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    parentResponseId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    questionKey :: (B.C f Kernel.Prelude.Text),
    rideId :: (B.C f Kernel.Prelude.Text),
    rideStatusAtResponse :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    secondsIntoRide :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    status :: (B.C f Domain.Types.RideFeedbackResponse.RideFeedbackResponseStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table RideFeedbackResponseT where
  data PrimaryKey RideFeedbackResponseT f = RideFeedbackResponseId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideFeedbackResponseId . id

type RideFeedbackResponse = RideFeedbackResponseT Identity

$(enableKVPG (''RideFeedbackResponseT) [('id)] [[('rideId)]])

$(mkTableInstances (''RideFeedbackResponseT) "ride_feedback_response")
