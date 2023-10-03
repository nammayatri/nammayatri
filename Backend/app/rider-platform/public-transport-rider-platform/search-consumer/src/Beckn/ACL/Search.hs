{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Search where

import Beckn.Context
import qualified Beckn.Spec.Common.Context as Context
import Beckn.Spec.Common.Gps
import Beckn.Spec.Search
import qualified Domain.Action.Search as DSearch
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Id
import Kernel.Utils.Common hiding (buildContext)

buildSearchReq ::
  ( MonadFlow m,
    MonadReader r m,
    HasField "bapId" r Text,
    HasField "bapURI" r BaseUrl
  ) =>
  DSearch.SearchMessage ->
  m (BecknReq SearchMessage)
buildSearchReq msg = do
  let txnId = getId (msg.searchId)
  bapId <- asks (.bapId)
  bapURI <- asks (.bapURI)
  context <- buildContext Context.SEARCH txnId bapId bapURI Nothing Nothing
  let intent = mkIntent msg
  pure (BecknReq context $ SearchMessage intent)

mkIntent :: DSearch.SearchMessage -> Intent
mkIntent msg = do
  Intent
    { fulfillment =
        Fulfillment
          { start =
              StartInfo
                { location =
                    LocationGps
                      { gps = toGps msg.gps
                      },
                  time =
                    StartTime $
                      TimeRange
                        { start = msg.fromDate,
                          end = msg.toDate
                        }
                },
            end =
              EndInfo $
                LocationGps
                  { gps = toGps msg.gps
                  }
          }
    }
  where
    toGps LatLong {..} = Gps {..}
