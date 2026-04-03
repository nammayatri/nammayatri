{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.SuspectStatusHistory where
import Kernel.Prelude
import qualified Domain.Types.SuspectFlagRequest
import qualified Domain.Types.Suspect
import qualified Kernel.Types.Id
import qualified "lib-dashboard" Domain.Types.Merchant



data SuspectStatusHistory
    = SuspectStatusHistory {adminApproval :: Kernel.Prelude.Maybe Domain.Types.SuspectFlagRequest.AdminApproval,
                            approvedBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                            createdAt :: Kernel.Prelude.UTCTime,
                            dl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                            firstName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                            flaggedBy :: Kernel.Prelude.Maybe [Domain.Types.Suspect.FlaggedBy],
                            flaggedStatus :: Domain.Types.Suspect.FlaggedStatus,
                            id :: Kernel.Types.Id.Id Domain.Types.SuspectStatusHistory.SuspectStatusHistory,
                            lastName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                            merchantShortId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                            statusChangedReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                            updatedAt :: Kernel.Prelude.UTCTime,
                            voterId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                            merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



