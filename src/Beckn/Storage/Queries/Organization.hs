module Beckn.Storage.Queries.Organization where

import           Database.Beam                    ((&&.), (<-.), (==.))
import           EulerHS.Prelude                  hiding (id)

import qualified Beckn.Storage.Queries            as DB
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.DB           as DB
import qualified Beckn.Types.Storage.Organization as Storage
import qualified Database.Beam                    as B
import qualified EulerHS.Language                 as L
import qualified EulerHS.Types                    as T


dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.OrganizationT)
dbTable = DB._organization DB.becknDb

create :: Storage.Organization -> L.Flow ()
create Storage.Organization {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Organization {..}) >>=
  either DB.throwDBError pure

findOrganizationById ::
     OrganizationId -> L.Flow (Maybe Storage.Organization)
findOrganizationById id = do
  DB.findOne dbTable predicate >>=
    either DB.throwDBError pure
  where
    predicate Storage.Organization {..} = (_id ==. B.val_ id)


listOrganizations ::
   Maybe Double
   -> Maybe Double
   -> Maybe Text
   -> Maybe LocationType
   -> Maybe Text
   -> Maybe Text
   -> Maybe Text
   -> Maybe Text
   -> Maybe Int
   -> L.Flow [Storage.Organization]
listOrganizations latM longM wardM locationTypeM cityM districtM stateM countryM pincodeM = do
  DB.findAll dbTable predicate >>=
    either DB.throwDBError pure
  where
    predicate Storage.Organization {..} =
      foldl (&&.)
        (B.val_ True)
        [ _lat ==. (B.val_ latM)
        , _long ==. (B.val_ longM)
        , _ward ==. (B.val_ wardM)
        , _locationType ==. (B.val_ locationTypeM)
        , _district ==. (B.val_ districtM)
        , maybe (B.val_ True) ((_city ==.) . B.val_) cityM
        , maybe (B.val_ True) ((_state ==.) . B.val_) stateM
        , maybe (B.val_ True) ((_country ==.) . B.val_) countryM
        , maybe (B.val_ True) ((_pincode ==.) . B.val_) pincodeM
        ]

