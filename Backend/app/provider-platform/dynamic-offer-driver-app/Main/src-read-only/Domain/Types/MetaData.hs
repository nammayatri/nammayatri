{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MetaData where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MetaData = MetaData
  { appPermissions :: Kernel.Prelude.Maybe Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    device :: Kernel.Prelude.Maybe Data.Text.Text,
    deviceDateTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    deviceOS :: Kernel.Prelude.Maybe Data.Text.Text,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
