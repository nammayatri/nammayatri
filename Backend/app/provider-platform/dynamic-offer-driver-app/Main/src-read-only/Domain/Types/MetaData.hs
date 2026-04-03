{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MetaData where
import Kernel.Prelude
import Data.Aeson
import qualified Data.Text
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Tools.Beam.UtilsTH



data MetaData
    = MetaData {appPermissions :: Kernel.Prelude.Maybe Data.Text.Text,
                createdAt :: Kernel.Prelude.UTCTime,
                device :: Kernel.Prelude.Maybe Data.Text.Text,
                deviceDateTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                deviceOS :: Kernel.Prelude.Maybe Data.Text.Text,
                driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



