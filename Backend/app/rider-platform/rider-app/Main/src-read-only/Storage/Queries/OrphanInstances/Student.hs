{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Student where

import qualified Domain.Types.Student
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Student as Beam

instance FromTType' Beam.Student Domain.Types.Student.Student where
  fromTType' (Beam.StudentT {..}) = do
    pure $
      Just
        Domain.Types.Student.Student
          { collegeId = Kernel.Types.Id.Id collegeId,
            collegeName = collegeName,
            createdAt = createdAt,
            destinationStop = destinationStop,
            graduationDate = graduationDate,
            guardianName = guardianName,
            id = Kernel.Types.Id.Id id,
            intermediateStops = fromMaybe [] intermediateStops,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            sourceStop = sourceStop,
            studentAddress = studentAddress,
            studentAge = studentAge,
            studentClass = studentClass,
            studentName = studentName,
            studentPicture = studentPicture,
            updatedAt = updatedAt,
            verificationDate = verificationDate,
            verificationStatus = verificationStatus
          }

instance ToTType' Beam.Student Domain.Types.Student.Student where
  toTType' (Domain.Types.Student.Student {..}) = do
    Beam.StudentT
      { Beam.collegeId = Kernel.Types.Id.getId collegeId,
        Beam.collegeName = collegeName,
        Beam.createdAt = createdAt,
        Beam.destinationStop = destinationStop,
        Beam.graduationDate = graduationDate,
        Beam.guardianName = guardianName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.intermediateStops = Just intermediateStops,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.sourceStop = sourceStop,
        Beam.studentAddress = studentAddress,
        Beam.studentAge = studentAge,
        Beam.studentClass = studentClass,
        Beam.studentName = studentName,
        Beam.studentPicture = studentPicture,
        Beam.updatedAt = updatedAt,
        Beam.verificationDate = verificationDate,
        Beam.verificationStatus = verificationStatus
      }
