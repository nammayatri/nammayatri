{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.LmsCertificate where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.LmsModule
import qualified Tools.Beam.UtilsTH



data LmsCertificate
    = LmsCertificate {driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                      id :: Kernel.Types.Id.Id Domain.Types.LmsCertificate.LmsCertificate,
                      moduleCompletionId :: Kernel.Prelude.Text,
                      moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
                      createdAt :: Kernel.Prelude.UTCTime,
                      updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



