{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TicketMerchantDetails where

import qualified Domain.Types.TicketMerchantDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
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

updateIsBankOnboarded ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.TicketMerchantDetails.TicketMerchantDetails -> m ())
updateIsBankOnboarded isBankOnboarded id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.isBankOnboarded isBankOnboarded, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TicketMerchantDetails.TicketMerchantDetails -> m (Maybe Domain.Types.TicketMerchantDetails.TicketMerchantDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TicketMerchantDetails.TicketMerchantDetails -> m ())
updateByPrimaryKey (Domain.Types.TicketMerchantDetails.TicketMerchantDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.agreementLetterEncrypted (agreementLetter <&> unEncrypted . (.encrypted)),
      Se.Set Beam.agreementLetterHash (agreementLetter <&> (.hash)),
      Se.Set Beam.bankAccountNumberEncrypted (bankAccountNumber & unEncrypted . encrypted),
      Se.Set Beam.bankAccountNumberHash (bankAccountNumber & hash),
      Se.Set Beam.bankAccountType bankAccountType,
      Se.Set Beam.bankBeneficiaryName bankBeneficiaryName,
      Se.Set Beam.bankIfscEncrypted (bankIfsc & unEncrypted . encrypted),
      Se.Set Beam.bankIfscHash (bankIfsc & hash),
      Se.Set Beam.contactDetailsEmail (Domain.Types.TicketMerchantDetails.email contactDetails),
      Se.Set Beam.contactDetailsName (Domain.Types.TicketMerchantDetails.name contactDetails),
      Se.Set Beam.contactDetailsNumber (Domain.Types.TicketMerchantDetails.number contactDetails),
      Se.Set Beam.docCancelledChequeEncrypted (docCancelledCheque <&> unEncrypted . (.encrypted)),
      Se.Set Beam.docCancelledChequeHash (docCancelledCheque <&> (.hash)),
      Se.Set Beam.docPanEncrypted (docPan & unEncrypted . encrypted),
      Se.Set Beam.docPanHash (docPan & hash),
      Se.Set Beam.gstinEncrypted (gstin <&> unEncrypted . (.encrypted)),
      Se.Set Beam.gstinHash (gstin <&> (.hash)),
      Se.Set Beam.isBankOnboarded isBankOnboarded,
      Se.Set Beam.orgAddress orgAddress,
      Se.Set Beam.orgName orgName,
      Se.Set Beam.panEncrypted (pan & unEncrypted . encrypted),
      Se.Set Beam.panHash (pan & hash),
      Se.Set Beam.state state,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TicketMerchantDetails Domain.Types.TicketMerchantDetails.TicketMerchantDetails where
  fromTType' (Beam.TicketMerchantDetailsT {..}) = do
    pure $
      Just
        Domain.Types.TicketMerchantDetails.TicketMerchantDetails
          { agreementLetter = EncryptedHashed <$> (Encrypted <$> agreementLetterEncrypted) <*> agreementLetterHash,
            bankAccountNumber = EncryptedHashed (Encrypted bankAccountNumberEncrypted) bankAccountNumberHash,
            bankAccountType = bankAccountType,
            bankBeneficiaryName = bankBeneficiaryName,
            bankIfsc = EncryptedHashed (Encrypted bankIfscEncrypted) bankIfscHash,
            contactDetails = makeContactDetails contactDetailsEmail contactDetailsName contactDetailsNumber,
            createdAt = createdAt,
            docCancelledCheque = EncryptedHashed <$> (Encrypted <$> docCancelledChequeEncrypted) <*> docCancelledChequeHash,
            docPan = EncryptedHashed (Encrypted docPanEncrypted) docPanHash,
            gstin = EncryptedHashed <$> (Encrypted <$> gstinEncrypted) <*> gstinHash,
            id = Kernel.Types.Id.Id id,
            isBankOnboarded = isBankOnboarded,
            orgAddress = orgAddress,
            orgName = orgName,
            pan = EncryptedHashed (Encrypted panEncrypted) panHash,
            state = state,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TicketMerchantDetails Domain.Types.TicketMerchantDetails.TicketMerchantDetails where
  toTType' (Domain.Types.TicketMerchantDetails.TicketMerchantDetails {..}) = do
    Beam.TicketMerchantDetailsT
      { Beam.agreementLetterEncrypted = agreementLetter <&> unEncrypted . (.encrypted),
        Beam.agreementLetterHash = agreementLetter <&> (.hash),
        Beam.bankAccountNumberEncrypted = bankAccountNumber & unEncrypted . encrypted,
        Beam.bankAccountNumberHash = bankAccountNumber & hash,
        Beam.bankAccountType = bankAccountType,
        Beam.bankBeneficiaryName = bankBeneficiaryName,
        Beam.bankIfscEncrypted = bankIfsc & unEncrypted . encrypted,
        Beam.bankIfscHash = bankIfsc & hash,
        Beam.contactDetailsEmail = Domain.Types.TicketMerchantDetails.email contactDetails,
        Beam.contactDetailsName = Domain.Types.TicketMerchantDetails.name contactDetails,
        Beam.contactDetailsNumber = Domain.Types.TicketMerchantDetails.number contactDetails,
        Beam.createdAt = createdAt,
        Beam.docCancelledChequeEncrypted = docCancelledCheque <&> unEncrypted . (.encrypted),
        Beam.docCancelledChequeHash = docCancelledCheque <&> (.hash),
        Beam.docPanEncrypted = docPan & unEncrypted . encrypted,
        Beam.docPanHash = docPan & hash,
        Beam.gstinEncrypted = gstin <&> unEncrypted . (.encrypted),
        Beam.gstinHash = gstin <&> (.hash),
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isBankOnboarded = isBankOnboarded,
        Beam.orgAddress = orgAddress,
        Beam.orgName = orgName,
        Beam.panEncrypted = pan & unEncrypted . encrypted,
        Beam.panHash = pan & hash,
        Beam.state = state,
        Beam.updatedAt = updatedAt
      }
