{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.LmsCertificate where

import Data.Aeson
import qualified Domain.Types.LmsModule
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data LmsCertificate = LmsCertificate
  { driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.LmsCertificate.LmsCertificate,
    moduleCompletionId :: Kernel.Prelude.Text,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
