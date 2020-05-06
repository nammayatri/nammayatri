module Storage.Queries.BookingReference where

import           Database.Beam                    ((&&.), (<-.), (==.), (||.))
import           EulerHS.Prelude                  hiding (id)

import qualified Beckn.Storage.Queries            as DB
import           Types.App
import           Beckn.Types.Common
import qualified Types.Storage.DB                 as DB
import qualified Types.Storage.BookingReference   as Storage
import           Beckn.Utils.Common
import           Data.Time
import qualified Database.Beam                    as B
import qualified EulerHS.Language                 as L
import qualified EulerHS.Types                    as T

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.BookingReferenceT)
dbTable = DB._bookingReference DB.transporterDb

create :: Storage.BookingReference -> L.Flow ()
create Storage.BookingReference {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.BookingReference {..}) >>=
  either DB.throwDBError pure

findBookingReferenceById ::
     BookingReferenceId -> L.Flow (Maybe Storage.BookingReference)
findBookingReferenceById id = do
  DB.findOne dbTable predicate >>=
    either DB.throwDBError pure
  where
    predicate Storage.BookingReference {..} = (_id ==. B.val_ id)