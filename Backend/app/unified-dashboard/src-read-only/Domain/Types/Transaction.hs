{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.Transaction where

import Data.Aeson
import qualified Data.Text
import qualified Domain.Types.AccessMatrix
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Dhall
import qualified Tools.Beam.UtilsTH

data Transaction = Transaction
  { commonDriverId :: Kernel.Prelude.Maybe Data.Text.Text,
    commonRideId :: Kernel.Prelude.Maybe Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    endpoint :: Domain.Types.AccessMatrix.UserActionType,
    id :: Kernel.Types.Id.Id Domain.Types.Transaction.Transaction,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    request :: Kernel.Prelude.Maybe Data.Text.Text,
    requestorId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    response :: Kernel.Prelude.Maybe Data.Text.Text,
    responseError :: Kernel.Prelude.Maybe Data.Text.Text,
    serverName :: Kernel.Prelude.Maybe Domain.Types.AccessMatrix.ServerName,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
