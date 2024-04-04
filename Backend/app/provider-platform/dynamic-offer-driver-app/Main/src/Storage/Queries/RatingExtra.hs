{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RatingExtra where

import qualified Data.Text
-- Extra code goes here --

import qualified Database.Beam as B
import Domain.Types.Person (Person)
import qualified Domain.Types.Person
import qualified Domain.Types.Rating
import qualified Domain.Types.Ride
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id (Id (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Rating as Beam
import qualified Storage.Beam.Rating as BeamR
import Storage.Queries.OrphanInstances.Rating

findAllRatingUsersCountByPerson :: KvDbFlow m r => Id Domain.Types.Person.Person -> m Int
findAllRatingUsersCountByPerson (Id driverId) = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
          B.filter_'
            (\rating' -> BeamR.driverId rating' B.==?. B.val_ driverId)
            do
              B.all_ (BeamCommon.rating BeamCommon.atlasDB)
  pure $ either (const 0) (\r -> if null r then 0 else head r) res
