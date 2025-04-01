{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketMerchantDetails where

import qualified Domain.Types.TicketMerchantDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TicketMerchantDetails as Beam
import Storage.Queries.Transformers.TicketMerchantDetails

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketMerchantDetails.TicketMerchantDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TicketMerchantDetails.TicketMerchantDetails] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketMerchantDetails.TicketMerchantDetails -> m (Maybe Domain.Types.TicketMerchantDetails.TicketMerchantDetails))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketMerchantDetails.TicketMerchantDetails -> m (Maybe Domain.Types.TicketMerchantDetails.TicketMerchantDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketMerchantDetails.TicketMerchantDetails -> m ())
updateByPrimaryKey (Domain.Types.TicketMerchantDetails.TicketMerchantDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bankAccountNumber bankAccountNumber,
      Se.Set Beam.bankAccountType bankAccountType,
      Se.Set Beam.bankBeneficiaryName bankBeneficiaryName,
      Se.Set Beam.bankIfsc bankIfsc,
      Se.Set Beam.contactDetailsEmail (Domain.Types.TicketMerchantDetails.email contactDetails),
      Se.Set Beam.contactDetailsName (Domain.Types.TicketMerchantDetails.name contactDetails),
      Se.Set Beam.contactDetailsNumber (Domain.Types.TicketMerchantDetails.number contactDetails),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.docCancelledChequeEncrypted (((docCancelledCheque & unEncrypted . encrypted))),
      Se.Set Beam.docCancelledChequeHash ((docCancelledCheque & hash)),
      Se.Set Beam.docPanEncrypted (((docPan & unEncrypted . encrypted))),
      Se.Set Beam.docPanHash ((docPan & hash)),
      Se.Set Beam.gstinEncrypted (((gstin & unEncrypted . encrypted))),
      Se.Set Beam.gstinHash ((gstin & hash)),
      Se.Set Beam.isBlocked isBlocked,
      Se.Set Beam.orgAddress orgAddress,
      Se.Set Beam.orgName orgName,
      Se.Set Beam.panEncrypted (((pan & unEncrypted . encrypted))),
      Se.Set Beam.panHash ((pan & hash)),
      Se.Set Beam.state state,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TicketMerchantDetails Domain.Types.TicketMerchantDetails.TicketMerchantDetails where
  fromTType' (Beam.TicketMerchantDetailsT {..}) = do
    pure $
      Just
        Domain.Types.TicketMerchantDetails.TicketMerchantDetails
          { bankAccountNumber = bankAccountNumber,
            bankAccountType = bankAccountType,
            bankBeneficiaryName = bankBeneficiaryName,
            bankIfsc = bankIfsc,
            contactDetails = makeContactDetails contactDetailsEmail contactDetailsName contactDetailsNumber,
            createdAt = createdAt,
            docCancelledCheque = EncryptedHashed (Encrypted docCancelledChequeEncrypted) docCancelledChequeHash,
            docPan = EncryptedHashed (Encrypted docPanEncrypted) docPanHash,
            gstin = EncryptedHashed (Encrypted gstinEncrypted) gstinHash,
            id = Kernel.Types.Id.Id id,
            isBlocked = isBlocked,
            orgAddress = orgAddress,
            orgName = orgName,
            pan = EncryptedHashed (Encrypted panEncrypted) panHash,
            state = state,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketMerchantDetails Domain.Types.TicketMerchantDetails.TicketMerchantDetails where
  toTType' (Domain.Types.TicketMerchantDetails.TicketMerchantDetails {..}) = do
    Beam.TicketMerchantDetailsT
      { Beam.bankAccountNumber = bankAccountNumber,
        Beam.bankAccountType = bankAccountType,
        Beam.bankBeneficiaryName = bankBeneficiaryName,
        Beam.bankIfsc = bankIfsc,
        Beam.contactDetailsEmail = Domain.Types.TicketMerchantDetails.email contactDetails,
        Beam.contactDetailsName = Domain.Types.TicketMerchantDetails.name contactDetails,
        Beam.contactDetailsNumber = Domain.Types.TicketMerchantDetails.number contactDetails,
        Beam.createdAt = createdAt,
        Beam.docCancelledChequeEncrypted = ((docCancelledCheque & unEncrypted . encrypted)),
        Beam.docCancelledChequeHash = (docCancelledCheque & hash),
        Beam.docPanEncrypted = ((docPan & unEncrypted . encrypted)),
        Beam.docPanHash = (docPan & hash),
        Beam.gstinEncrypted = ((gstin & unEncrypted . encrypted)),
        Beam.gstinHash = (gstin & hash),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isBlocked = isBlocked,
        Beam.orgAddress = orgAddress,
        Beam.orgName = orgName,
        Beam.panEncrypted = ((pan & unEncrypted . encrypted)),
        Beam.panHash = (pan & hash),
        Beam.state = state,
        Beam.updatedAt = updatedAt
      }
