{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.SuspectStatusChangeRequest where
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.SuspectFlagRequest
import qualified "lib-dashboard" Domain.Types.Merchant



data SuspectStatusChangeRequest
    = SuspectStatusChangeRequest {createdAt :: Kernel.Prelude.UTCTime,
                                  id :: Kernel.Types.Id.Id Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest,
                                  merchantShortId :: Kernel.Prelude.Text,
                                  reasonToChange :: Kernel.Prelude.Text,
                                  reqStatus :: Domain.Types.SuspectFlagRequest.AdminApproval,
                                  suspectId :: Kernel.Prelude.Text,
                                  updatedAt :: Kernel.Prelude.UTCTime,
                                  merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



