{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.AadhaarOtpVerify where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Tools.Beam.UtilsTH



data AadhaarOtpVerify
    = AadhaarOtpVerify {driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                        id :: Kernel.Types.Id.Id Domain.Types.AadhaarOtpVerify.AadhaarOtpVerify,
                        requestId :: Kernel.Prelude.Text,
                        requestMessage :: Kernel.Prelude.Text,
                        statusCode :: Kernel.Prelude.Text,
                        transactionId :: Kernel.Prelude.Text,
                        createdAt :: Kernel.Prelude.UTCTime,
                        updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



