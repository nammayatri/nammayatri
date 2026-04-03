{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MerchantAccess where
import Kernel.Prelude
import Data.Aeson
import Kernel.Utils.Dhall
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Kernel.Types.Beckn.Context
import qualified Domain.Types.Person
import qualified Data.Text
import qualified Tools.Beam.UtilsTH



data MerchantAccess
    = MerchantAccess {createdAt :: Kernel.Prelude.UTCTime,
                      id :: Kernel.Types.Id.Id Domain.Types.MerchantAccess.MerchantAccess,
                      is2faEnabled :: Kernel.Prelude.Bool,
                      merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                      merchantShortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
                      operatingCity :: Kernel.Types.Beckn.Context.City,
                      personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                      secretKey :: Kernel.Prelude.Maybe Data.Text.Text,
                      updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



