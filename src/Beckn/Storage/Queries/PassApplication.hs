module Beckn.Storage.Queries.PassApplication where

import qualified Beckn.Storage.Queries               as DB
import           Beckn.Types.App
import qualified Beckn.Types.Common                  as Storage (PassType (..))
import qualified Beckn.Types.Storage.DB              as DB
import qualified Beckn.Types.Storage.PassApplication as Storage
import           Beckn.Utils.Common
import           Data.Time.LocalTime
import           Database.Beam                       ((&&.), (<-.), (==.))
import qualified Database.Beam                       as B
import qualified EulerHS.Language                    as L
import           EulerHS.Prelude                     hiding (id)
import qualified EulerHS.Types                       as T

dbTable ::
     B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.PassApplicationT)
dbTable = DB._passApplication DB.becknDb

create :: Storage.PassApplication -> L.Flow ()
create Storage.PassApplication {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.PassApplication {..}) >>=
  either DB.throwDBError pure

findById ::
     PassApplicationId -> L.Flow (T.DBResult (Maybe Storage.PassApplication))
findById id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.PassApplication {..} = (_id ==. B.val_ id)

findAllWithLimitOffsetWhere :: [Storage.Status] -> [Storage.PassType] -> Maybe Int -> Maybe Int -> L.Flow (T.DBResult [Storage.PassApplication])
findAllWithLimitOffsetWhere statusArr passTypeArr mlimit moffset =
  DB.findAllWithLimitOffsetWhere dbTable (predicate statusArr passTypeArr) limit offset orderByDesc
  where
    limit = (toInteger $ fromMaybe 10 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)

    orderByDesc Storage.PassApplication {..} = B.desc_ _createdAt

    predicate sArr pArr Storage.PassApplication {..} = (_status `B.in_` (B.val_ <$> sArr)) &&. (_passType `B.in_` (B.val_ <$> pArr))

update :: PassApplicationId -> Storage.Status -> Int -> Text -> L.Flow (T.DBResult ())
update id status approvedCount remarks = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update dbTable
    (setClause status approvedCount remarks currTime)
    (predicate id)
  where
    setClause status approvedCount remarks currTime Storage.PassApplication {..} =
      mconcat
        [ _status <-. B.val_ status
        , _approvedCount <-. B.val_ approvedCount
        , _remarks <-. B.val_ remarks
        , _updatedAt <-. B.val_ currTime
        ]

    predicate id Storage.PassApplication {..} = _id ==. B.val_ id
