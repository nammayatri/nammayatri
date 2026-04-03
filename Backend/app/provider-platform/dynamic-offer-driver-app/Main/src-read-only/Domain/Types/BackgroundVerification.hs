{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.BackgroundVerification where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Kernel.Types.Documents
import qualified Servant.Client.Core
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data BackgroundVerification
    = BackgroundVerification {candidateId :: Kernel.Prelude.Text,
                              driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                              expiresAt :: Kernel.Prelude.UTCTime,
                              invitationId :: Kernel.Prelude.Text,
                              invitationStatus :: Kernel.Types.Documents.VerificationStatus,
                              invitationUrl :: Servant.Client.Core.BaseUrl,
                              merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                              merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                              reportId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                              reportStatus :: Kernel.Types.Documents.VerificationStatus,
                              createdAt :: Kernel.Prelude.UTCTime,
                              updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



