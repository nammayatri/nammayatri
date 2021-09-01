module Product.Quote where

import App.Types
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.SearchReqLocation as Location
import qualified Types.API.Quote as API
import Types.Error
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as PI
import qualified Types.Storage.Quote as SQuote
import qualified Types.Storage.SearchReqLocation as Location
import Utils.Common

getQuotes :: Id Case.Case -> Id Person.Person -> FlowHandler API.GetQuotesRes
getQuotes caseId _ = withFlowHandlerAPI $ do
  case_ <- QCase.findById caseId >>= fromMaybeM CaseDoesNotExist
  fromLocation <- Location.findLocationById case_.fromLocationId >>= fromMaybeM LocationNotFound
  toLocation <- Location.findLocationById case_.toLocationId >>= fromMaybeM LocationNotFound
  piList <- QPI.findAllByCaseId case_.id
  quotes <- traverse buildQuote piList
  return $
    API.GetQuotesRes
      { fromLocation = Location.makeSearchReqLocationAPIEntity fromLocation,
        toLocation = Location.makeSearchReqLocationAPIEntity toLocation,
        quotes
      }
  where
    buildQuote :: DBFlow m r => PI.ProductInstance -> m SQuote.QuoteAPIEntity
    buildQuote prodInst = do
      org <- QOrg.findOrganizationById prodInst.organizationId >>= fromMaybeM OrgNotFound
      completedRidesCount <- QPI.countCompletedRides org.id
      return $
        SQuote.QuoteAPIEntity
          { id = prodInst.id,
            estimatedPrice = fromMaybe 0 prodInst.price,
            agencyName = org.name,
            agencyNumber = fromMaybe "" $ org.mobileCountryCode <> org.mobileNumber,
            agencyCompletedRidesCount = completedRidesCount,
            nearestDriverDistance = maybe 0 (read . T.unpack) prodInst.udf1,
            createdAt = prodInst.createdAt
          }
