{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PersonFlowStatus (module Domain.Types.PersonFlowStatus, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.PersonFlowStatus as ReExport
import qualified Domain.Types.Extra.PersonFlowStatus
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PersonFlowStatus = PersonFlowStatus {flowStatus :: Domain.Types.Extra.PersonFlowStatus.FlowStatus, personId :: Kernel.Types.Id.Id Domain.Types.Person.Person, updatedAt :: Kernel.Prelude.UTCTime}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
