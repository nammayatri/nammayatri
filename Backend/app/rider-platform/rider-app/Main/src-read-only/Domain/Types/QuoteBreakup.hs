{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.QuoteBreakup where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Kernel.Types.Common
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data QuoteBreakup
    = QuoteBreakup {id :: Kernel.Types.Id.Id Domain.Types.QuoteBreakup.QuoteBreakup,
                    price :: Kernel.Types.Common.Price,
                    quoteId :: Kernel.Prelude.Text,
                    title :: Kernel.Prelude.Text,
                    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                    createdAt :: Kernel.Prelude.UTCTime,
                    updatedAt :: Kernel.Prelude.UTCTime}
    deriving Show



