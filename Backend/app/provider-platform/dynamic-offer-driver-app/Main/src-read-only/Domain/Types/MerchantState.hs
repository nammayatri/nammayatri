{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MerchantState where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Tools.Beam.UtilsTH



data MerchantState
    = MerchantState {allowedDestinationStates :: [Kernel.Types.Beckn.Context.IndianState],
                     merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                     state :: Kernel.Types.Beckn.Context.IndianState,
                     createdAt :: Kernel.Prelude.UTCTime,
                     updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



