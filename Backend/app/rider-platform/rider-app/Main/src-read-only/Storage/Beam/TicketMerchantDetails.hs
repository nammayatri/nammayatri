{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.TicketMerchantDetails where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.External.Encryption
import qualified Domain.Types.TicketMerchantDetails
import qualified Database.Beam as B



data TicketMerchantDetailsT f
    = TicketMerchantDetailsT {agreementLetterEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              agreementLetterHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
                              bankAccountNumberEncrypted :: (B.C f Kernel.Prelude.Text),
                              bankAccountNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
                              bankAccountType :: (B.C f Domain.Types.TicketMerchantDetails.BankAccountType),
                              bankBeneficiaryName :: (B.C f Kernel.Prelude.Text),
                              bankIfscEncrypted :: (B.C f Kernel.Prelude.Text),
                              bankIfscHash :: (B.C f Kernel.External.Encryption.DbHash),
                              contactDetailsEmail :: (B.C f Kernel.Prelude.Text),
                              contactDetailsName :: (B.C f Kernel.Prelude.Text),
                              contactDetailsNumber :: (B.C f Kernel.Prelude.Text),
                              createdAt :: (B.C f Kernel.Prelude.UTCTime),
                              docCancelledChequeEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              docCancelledChequeHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
                              docPanEncrypted :: (B.C f Kernel.Prelude.Text),
                              docPanHash :: (B.C f Kernel.External.Encryption.DbHash),
                              gstinEncrypted :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              gstinHash :: (B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash)),
                              id :: (B.C f Kernel.Prelude.Text),
                              isBankOnboarded :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                              orgAddress :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                              orgName :: (B.C f Kernel.Prelude.Text),
                              panEncrypted :: (B.C f Kernel.Prelude.Text),
                              panHash :: (B.C f Kernel.External.Encryption.DbHash),
                              state :: (B.C f Kernel.Prelude.Text),
                              updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table TicketMerchantDetailsT
    where data PrimaryKey TicketMerchantDetailsT f = TicketMerchantDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = TicketMerchantDetailsId . id
type TicketMerchantDetails = TicketMerchantDetailsT Identity

$(enableKVPG (''TicketMerchantDetailsT) [('id)] [])

$(mkTableInstances (''TicketMerchantDetailsT) "ticket_merchant_details")

