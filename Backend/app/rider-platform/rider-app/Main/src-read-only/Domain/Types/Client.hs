{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.Client where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Tools.Beam.UtilsTH



data Client
    = Client {id :: Kernel.Types.Id.Id Domain.Types.Client.Client,
              shortId :: Kernel.Types.Id.ShortId Domain.Types.Client.Client,
              merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
              createdAt :: Kernel.Prelude.UTCTime,
              updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



