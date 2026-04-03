{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.PurchasedPassPayment where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.PurchasedPassPayment
import qualified Storage.Beam.PurchasedPassPayment as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id



instance FromTType' Beam.PurchasedPassPayment Domain.Types.PurchasedPassPayment.PurchasedPassPayment
    where fromTType' (Beam.PurchasedPassPaymentT {..}) = do pure $ Just Domain.Types.PurchasedPassPayment.PurchasedPassPayment{amount = amount,
                                                                                                                               benefitDescription = Kernel.Prelude.fromMaybe "" benefitDescription,
                                                                                                                               benefitType = benefitType,
                                                                                                                               benefitValue = benefitValue,
                                                                                                                               endDate = endDate,
                                                                                                                               id = Kernel.Types.Id.Id id,
                                                                                                                               isDashboard = isDashboard,
                                                                                                                               merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                               merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                               orderId = Kernel.Types.Id.Id orderId,
                                                                                                                               passCode = passCode,
                                                                                                                               passEnum = passEnum,
                                                                                                                               passName = passName,
                                                                                                                               personId = Kernel.Types.Id.Id personId,
                                                                                                                               profilePicture = profilePicture,
                                                                                                                               purchasedPassId = Kernel.Types.Id.Id purchasedPassId,
                                                                                                                               startDate = startDate,
                                                                                                                               status = status,
                                                                                                                               createdAt = createdAt,
                                                                                                                               updatedAt = updatedAt}
instance ToTType' Beam.PurchasedPassPayment Domain.Types.PurchasedPassPayment.PurchasedPassPayment
    where toTType' (Domain.Types.PurchasedPassPayment.PurchasedPassPayment {..}) = do Beam.PurchasedPassPaymentT{Beam.amount = amount,
                                                                                                                 Beam.benefitDescription = Kernel.Prelude.Just benefitDescription,
                                                                                                                 Beam.benefitType = benefitType,
                                                                                                                 Beam.benefitValue = benefitValue,
                                                                                                                 Beam.endDate = endDate,
                                                                                                                 Beam.id = Kernel.Types.Id.getId id,
                                                                                                                 Beam.isDashboard = isDashboard,
                                                                                                                 Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                                 Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                                 Beam.orderId = Kernel.Types.Id.getId orderId,
                                                                                                                 Beam.passCode = passCode,
                                                                                                                 Beam.passEnum = passEnum,
                                                                                                                 Beam.passName = passName,
                                                                                                                 Beam.personId = Kernel.Types.Id.getId personId,
                                                                                                                 Beam.profilePicture = profilePicture,
                                                                                                                 Beam.purchasedPassId = Kernel.Types.Id.getId purchasedPassId,
                                                                                                                 Beam.startDate = startDate,
                                                                                                                 Beam.status = status,
                                                                                                                 Beam.createdAt = createdAt,
                                                                                                                 Beam.updatedAt = updatedAt}



