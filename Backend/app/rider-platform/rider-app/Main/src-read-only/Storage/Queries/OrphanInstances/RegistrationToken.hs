{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.RegistrationToken where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.RegistrationToken
import qualified Storage.Beam.RegistrationToken as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.RegistrationToken Domain.Types.RegistrationToken.RegistrationToken
    where fromTType' (Beam.RegistrationTokenT {..}) = do pure $ Just Domain.Types.RegistrationToken.RegistrationToken{attempts = attempts,
                                                                                                                      authExpiry = authExpiry,
                                                                                                                      authMedium = authMedium,
                                                                                                                      authType = authType,
                                                                                                                      authValueHash = authValueHash,
                                                                                                                      createdAt = createdAt,
                                                                                                                      createdViaPartnerOrgId = Kernel.Types.Id.Id <$> createdViaPartnerOrgId,
                                                                                                                      entityId = entityId,
                                                                                                                      entityType = entityType,
                                                                                                                      id = Kernel.Types.Id.Id id,
                                                                                                                      info = info,
                                                                                                                      merchantId = merchantId,
                                                                                                                      token = token,
                                                                                                                      tokenExpiry = tokenExpiry,
                                                                                                                      updatedAt = updatedAt,
                                                                                                                      verified = verified}
instance ToTType' Beam.RegistrationToken Domain.Types.RegistrationToken.RegistrationToken
    where toTType' (Domain.Types.RegistrationToken.RegistrationToken {..}) = do Beam.RegistrationTokenT{Beam.attempts = attempts,
                                                                                                        Beam.authExpiry = authExpiry,
                                                                                                        Beam.authMedium = authMedium,
                                                                                                        Beam.authType = authType,
                                                                                                        Beam.authValueHash = authValueHash,
                                                                                                        Beam.createdAt = createdAt,
                                                                                                        Beam.createdViaPartnerOrgId = Kernel.Types.Id.getId <$> createdViaPartnerOrgId,
                                                                                                        Beam.entityId = entityId,
                                                                                                        Beam.entityType = entityType,
                                                                                                        Beam.id = Kernel.Types.Id.getId id,
                                                                                                        Beam.info = info,
                                                                                                        Beam.merchantId = merchantId,
                                                                                                        Beam.token = token,
                                                                                                        Beam.tokenExpiry = tokenExpiry,
                                                                                                        Beam.updatedAt = updatedAt,
                                                                                                        Beam.verified = verified}



