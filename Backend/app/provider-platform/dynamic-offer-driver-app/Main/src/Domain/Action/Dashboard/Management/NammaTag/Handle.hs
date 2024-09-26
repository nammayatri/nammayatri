module Domain.Action.Dashboard.Management.NammaTag.Handle (kaalChakraHandle) where

import qualified Domain.Types.Person as DPerson
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Event.KaalChakra as KaalChakra
import qualified Lib.Yudhishthira.Types
import qualified Storage.Queries.Person as QPerson

-- Moved here because of overlapping instances for HasSchemaName SchedulerJob between bpp and kaal-chakra
kaalChakraHandle :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => KaalChakra.Handle m
kaalChakraHandle =
  KaalChakra.Handle
    { getUserTags = \userId -> do
        mbDriver <- QPerson.findById $ cast @Lib.Yudhishthira.Types.User @DPerson.Person userId
        pure $ mbDriver <&> (\driver -> fromMaybe [] driver.driverTag),
      updateUserTags = \userId driverTags -> QPerson.updateDriverTag (Just driverTags) (cast @Lib.Yudhishthira.Types.User @DPerson.Person userId)
    }
