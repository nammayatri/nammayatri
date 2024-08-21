{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SearchRequestDeliveryDetails where

import qualified Domain.Types.DeliveryPersonDetails
import qualified Domain.Types.SearchRequestDeliveryDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SearchRequestDeliveryDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SearchRequestDeliveryDetails.SearchRequestDeliveryDetails -> m ())
create = createWithKV

findBySearchRequestId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.SearchRequestDeliveryDetails.SearchRequestDeliveryDetails))
findBySearchRequestId searchRequestId = do findOneWithKV [Se.Is Beam.searchRequestId $ Se.Eq searchRequestId]

instance FromTType' Beam.SearchRequestDeliveryDetails Domain.Types.SearchRequestDeliveryDetails.SearchRequestDeliveryDetails where
  fromTType' (Beam.SearchRequestDeliveryDetailsT {..}) = do
    pure $
      Just
        Domain.Types.SearchRequestDeliveryDetails.SearchRequestDeliveryDetails
          { initiatedAs = initiatedAs,
            receiverDetails = Domain.Types.DeliveryPersonDetails.DeliveryPersonDetails (Kernel.Types.Id.Id receiverRiderId) receiverName (EncryptedHashed (Encrypted receiverPhoneNumberEncrypted) receiverPhoneNumberHash),
            searchRequestId = searchRequestId,
            senderDetails = Domain.Types.DeliveryPersonDetails.DeliveryPersonDetails (Kernel.Types.Id.Id senderRiderId) senderName (EncryptedHashed (Encrypted senderPhoneNumberEncrypted) senderPhoneNumberHash),
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.SearchRequestDeliveryDetails Domain.Types.SearchRequestDeliveryDetails.SearchRequestDeliveryDetails where
  toTType' (Domain.Types.SearchRequestDeliveryDetails.SearchRequestDeliveryDetails {..}) = do
    Beam.SearchRequestDeliveryDetailsT
      { Beam.initiatedAs = initiatedAs,
        Beam.receiverName = receiverDetails & (.name),
        Beam.receiverPhoneNumberEncrypted = (receiverDetails & (.phone)) & unEncrypted . encrypted,
        Beam.receiverPhoneNumberHash = (receiverDetails & (.phone)) & hash,
        Beam.receiverRiderId = (receiverDetails & (.id)) & Kernel.Types.Id.getId,
        Beam.searchRequestId = searchRequestId,
        Beam.senderName = senderDetails & (.name),
        Beam.senderPhoneNumberEncrypted = (senderDetails & (.phone)) & unEncrypted . encrypted,
        Beam.senderPhoneNumberHash = (senderDetails & (.phone)) & hash,
        Beam.senderRiderId = (senderDetails & (.id)) & Kernel.Types.Id.getId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
