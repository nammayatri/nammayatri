{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.ApprovalRequest where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TripTransaction
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data ApprovalRequest = ApprovalRequest
  { body :: Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.ApprovalRequest.ApprovalRequest,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    reason :: Kernel.Prelude.Maybe Data.Text.Text,
    requestType :: Domain.Types.ApprovalRequest.RequestType,
    requesteeId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    requestorId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    status :: Domain.Types.ApprovalRequest.RequestStatus,
    title :: Data.Text.Text,
    tripTransactionId :: Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data RequestStatus = ACCEPTED | REJECTED | AWAITING_APPROVAL | REVOKED deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data RequestType = END_RIDE | CHANGE_ROUTE deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''RequestType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''RequestStatus)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''RequestType)

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum ''RequestStatus)
