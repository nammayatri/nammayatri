{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AadhaarOtpReq where

import Data.Aeson
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data AadhaarOtpReq = AadhaarOtpReq
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.AadhaarOtpReq.AadhaarOtpReq,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    requestId :: Kernel.Prelude.Text,
    requestMessage :: Kernel.Prelude.Text,
    statusCode :: Kernel.Prelude.Text,
    transactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
