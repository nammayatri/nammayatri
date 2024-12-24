module Domain.Action.Dashboard.NammaTag.Handle (kaalChakraHandle) where

import qualified Domain.Types.Person as DPerson
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Event.KaalChakra as KaalChakra
import qualified Lib.Yudhishthira.Types
import qualified SharedLogic.KaalChakra.Actions as Actions
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.Person as QPerson

type HandlerFlow m r = (EsqDBFlow m r, MonadFlow m, CacheFlow m r)

-- Moved here because of overlapping instances for HasSchemaName SchedulerJob between bpp and kaal-chakra
kaalChakraHandle ::
  HandlerFlow m r =>
  KaalChakra.Handle m Actions.Action
kaalChakraHandle =
  KaalChakra.Handle
    { getUserTags = \userId -> do
        mbRider <- QPerson.findById $ cast @Lib.Yudhishthira.Types.User @DPerson.Person userId
        pure $ mbRider <&> (\rider -> Lib.Yudhishthira.Types.TagNameValue <$> fromMaybe [] rider.customerNammaTags),
      updateUserTags = \userId customerTags -> QPerson.updateCustomerTags (Just $ Lib.Yudhishthira.Types.getTagNameValue <$> customerTags) (cast @Lib.Yudhishthira.Types.User @DPerson.Person userId),
      action = Actions.kaalChakraAction . cast @Lib.Yudhishthira.Types.User @DPerson.Person
    }
