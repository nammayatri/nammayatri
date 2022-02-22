module Core.ACL.Search where

import Beckn.Prelude
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import Beckn.Utils.Common
import Core.Context
import qualified Core.Spec.Common.Context as Context
import qualified Core.Spec.Common.Gps as Intent
import Core.Spec.Common.Time as Time
import qualified Core.Spec.Search as Search
import qualified Core.Spec.Search.Intent as Intent
import qualified Domain.Action.Search as DSearch

buildSearchReq ::
  ( MonadFlow m,
    MonadReader r m,
    HasInConfig r c "bapId" Text,
    HasInConfig r c "bapURI" BaseUrl
  ) =>
  DSearch.SearchMessage ->
  m (BecknReq Search.SearchIntent)
buildSearchReq msg = do
  let txnId = getId (msg.searchId)
  bapId <- asks (.config.bapId)
  bapURI <- asks (.config.bapURI)
  context <- buildContext Context.SEARCH txnId bapId bapURI Nothing Nothing
  let intent = mkIntent msg
  pure (BecknReq context $ Search.SearchIntent intent)

mkIntent :: DSearch.SearchMessage -> Intent.Intent
mkIntent msg = do
  Intent.Intent
    { fulfillment =
        Intent.FulFillmentInfo
          { start =
              Intent.LocationAndTime
                { location =
                    Intent.Location
                      { gps = toGps msg.gps
                      },
                  time =
                    Time.Time
                      { timestamp = msg.toDate
                      }
                },
            end =
              Intent.Location
                { gps = toGps msg.gps
                }
          }
    }
  where
    toGps LatLong {..} = Intent.Gps {..}
