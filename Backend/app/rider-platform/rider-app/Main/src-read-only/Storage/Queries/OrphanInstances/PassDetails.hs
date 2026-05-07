{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PassDetails where

import qualified Data.Aeson
import qualified Domain.Types.PassDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PassDetails as Beam

instance FromTType' Beam.PassDetails Domain.Types.PassDetails.PassDetails where
  fromTType' (Beam.PassDetailsT {..}) = do
    pure $
      Just
        Domain.Types.PassDetails.PassDetails
          { aadharNo = EncryptedHashed <$> (Encrypted <$> aadharNoEncrypted) <*> aadharNoHash,
            address = address,
            age = age,
            applicableRouteIds = applicableRouteIds,
            createdAt = createdAt,
            gender = gender,
            graduationDate = graduationDate,
            guardianName = guardianName,
            id = Kernel.Types.Id.Id id,
            idCardPicture = idCardPicture,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            numberOfStages = numberOfStages,
            passEnum = passEnum,
            passOrganizationId = Kernel.Types.Id.Id passOrganizationId,
            personId = Kernel.Types.Id.Id personId,
            pincode = pincode,
            referenceNumber = referenceNumber,
            registerNo = registerNo,
            remark = remark,
            routePairs = fromMaybe [] ((\val -> case Data.Aeson.fromJSON val of Data.Aeson.Success x -> Just x; Data.Aeson.Error _ -> Nothing) =<< routePairs),
            selfImage = selfImage,
            studentClass = studentClass,
            updatedAt = updatedAt,
            validTill = validTill,
            verificationStatus = verificationStatus
          }

instance ToTType' Beam.PassDetails Domain.Types.PassDetails.PassDetails where
  toTType' (Domain.Types.PassDetails.PassDetails {..}) = do
    Beam.PassDetailsT
      { Beam.aadharNoEncrypted = ((aadharNo <&> unEncrypted . (.encrypted))),
        Beam.aadharNoHash = (aadharNo <&> (.hash)),
        Beam.address = address,
        Beam.age = age,
        Beam.applicableRouteIds = applicableRouteIds,
        Beam.createdAt = createdAt,
        Beam.gender = gender,
        Beam.graduationDate = graduationDate,
        Beam.guardianName = guardianName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.idCardPicture = idCardPicture,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.numberOfStages = numberOfStages,
        Beam.passEnum = passEnum,
        Beam.passOrganizationId = Kernel.Types.Id.getId passOrganizationId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.pincode = pincode,
        Beam.referenceNumber = referenceNumber,
        Beam.registerNo = registerNo,
        Beam.remark = remark,
        Beam.routePairs = Just (Kernel.Prelude.toJSON routePairs),
        Beam.selfImage = selfImage,
        Beam.studentClass = studentClass,
        Beam.updatedAt = updatedAt,
        Beam.validTill = validTill,
        Beam.verificationStatus = verificationStatus
      }
