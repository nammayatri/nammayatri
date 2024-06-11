{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Feedback (module Domain.Types.Feedback, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.Feedback as ReExport
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data Feedback = Feedback
  { badge :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.Feedback.Feedback,
    rideId :: Kernel.Types.Id.Id Domain.Types.Ride.Ride
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

{-
	DSL Source Link: file://./../../../spec/Storage/FeedBack.yaml
-}
