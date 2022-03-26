module API.Search.Handler where

import API.Common
import qualified API.Search.Types as Search
import App.Types
import Beckn.Types.Amount (Amount (Amount))
import Beckn.Types.Core.ReqTypes
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Data.Text as T
import qualified Domain.DunzoCreds as DDunzoCreds
import qualified Domain.Organization as DOrg
import EulerHS.Prelude hiding (id)
import qualified ExternalAPI.Dunzo.Flow as DzAPI
import qualified ExternalAPI.Dunzo.Types as Dz
import qualified Types.Beckn.API.OnSearch as OnSearch
import Types.Beckn.Context
import Types.Error
import Types.Wrapper
import Utils.Callback
import Utils.Common

handler :: SignatureAuthResult -> SignatureAuthResult -> BecknReq Search.SearchIntent -> FlowHandler AckResponse
handler (SignatureAuthResult _ subscriber) (SignatureAuthResult _ gateway) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext SEARCH $ req.context
    validateBapUrl subscriber $ req.context
    bapOrg <- findOrg subscriber
    search bapOrg gateway.subscriber_url req

search ::
  DOrg.Organization ->
  BaseUrl ->
  BecknReq Search.SearchIntent ->
  Flow AckResponse
search org cbUrl req = do
  config@DunzoConfig {..} <- asks (.dzConfig)
  let quoteReq = mkQuoteReqFromSearch req
  dzBACreds <- getCreds org.dunzoCredsId
  withCallback' withRetry SEARCH OnSearch.onSearchAPI req.context cbUrl $
    getQuote dzBACreds config quoteReq
      <&> mkOnSearchCatalog

getQuote ::
  DDunzoCreds.DunzoCreds ->
  DunzoConfig ->
  Dz.QuoteReq ->
  Flow Dz.QuoteRes
getQuote ba@DDunzoCreds.DunzoCreds {..} conf@DunzoConfig {..} quoteReq = do
  token <- fetchToken ba conf
  DzAPI.getQuote clientId token dzUrl quoteReq

dunzoServiceCategoryId :: Text
dunzoServiceCategoryId = "1"

mkQuoteReqFromSearch :: BecknReq Search.SearchIntent -> Dz.QuoteReq
mkQuoteReqFromSearch BecknReq {..} = do
  let intent = message.intent
  let pickupGps = intent.fulfillment.start.location.gps
  let dropGps = intent.fulfillment.end.location.gps
  Dz.QuoteReq
    { pickup_lat = pickupGps.lat,
      pickup_lng = pickupGps.lon,
      drop_lat = dropGps.lat,
      drop_lng = dropGps.lon,
      category_id = "pickup_drop"
    }

readCoord :: MonadFlow m => Text -> m Double
readCoord text = do
  readMaybe (T.unpack text)
    & fromMaybeM (InvalidRequest "Location read error.")

mkOnSearchCatalog :: Dz.QuoteRes -> OnSearch.OnSearchCatalog
mkOnSearchCatalog res@Dz.QuoteRes {..} =
  OnSearch.OnSearchCatalog catalog
  where
    catalog =
      OnSearch.Catalog
        { bpp_providers =
            [ OnSearch.Provider
                { descriptor =
                    OnSearch.DescriptorInfo
                      { name = "Dunzo Digital Private Limited"
                      },
                  categories =
                    [ OnSearch.Category
                        { id = dunzoServiceCategoryId,
                          descriptor =
                            OnSearch.Descriptor
                              { name = "Pickup and drop",
                                code = "pickup_drop"
                              }
                        }
                    ],
                  items =
                    foldWIndex
                      (\index acc packageContent -> acc <> [mkSearchItem (index + 1) packageContent res])
                      []
                      Dz.dzPackageContentList
                }
            ]
        }

mkSearchItem :: Integer -> Dz.PackageContent -> Dz.QuoteRes -> OnSearch.Item
mkSearchItem index packageContent Dz.QuoteRes {..} =
  OnSearch.Item
    { id = show index,
      descriptor =
        OnSearch.Descriptor
          { name = packageContent.content,
            code = packageContent.content
          },
      price = price,
      category_id = dunzoServiceCategoryId
    }
  where
    price =
      OnSearch.Price
        { currency = "INR",
          estimated_value = value
        }
    value = OnSearch.convertAmountToDecimalValue (Amount $ toRational estimated_price)
