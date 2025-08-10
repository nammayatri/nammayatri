module Storage.Queries.Transformers.JourneyLeg where

import qualified Domain.Types.Person
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Journey as QJ
import Tools.Error

getRiderId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Kernel.Prelude.Text -> Maybe Kernel.Prelude.Text -> m (Kernel.Types.Id.Id Domain.Types.Person.Person)
getRiderId journeyId mbRiderId = do
  case mbRiderId of
    Just id -> return (Id id)
    Nothing -> do
      journey <- QJ.findByPrimaryKey (Kernel.Types.Id.Id journeyId) >>= fromMaybeM (InternalError "Journey Not Found")
      return journey.riderId
