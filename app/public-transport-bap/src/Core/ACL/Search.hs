module Core.ACL.Search where

import Beckn.Prelude
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Core.Spec.Common.Context as Context
import Core.Spec.Common.Time as Time
import qualified Core.Spec.Search as Search
import qualified Core.Spec.Search.Intent as Intent
import qualified Domain.Endpoints.UI.Search as DSearch
import Tools.Context

buildSearchReq ::
  ( MonadFlow m,
    MonadReader r m,
    HasInConfig r c "selfId" Text,
    HasInConfig r c "selfURI" BaseUrl
  ) =>
  DSearch.SearchMessage ->
  m (BecknReq Search.SearchIntent)
buildSearchReq msg = do
  let txnId = getId (msg.searchId)
  bapId <- asks (.config.selfId)
  bapURI <- asks (.config.selfURI)
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
                      { gps = msg.gps
                      },
                  time =
                    Time.Time
                      { timestamp = msg.toDate
                      }
                },
            end =
              Intent.Location
                { gps = msg.gps
                }
          }
    }
