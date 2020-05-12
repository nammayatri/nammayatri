module Storage.Queries.Case where

import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as Storage
import           Beckn.Utils.Common
import           Data.Time
import           Database.Beam            ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam            as B
import qualified EulerHS.Language         as L
import           EulerHS.Prelude          hiding (id)
import qualified EulerHS.Types            as T
import qualified Storage.Queries          as DB
import           Types.App
import qualified Types.Storage.DB         as DB

dbTable :: B.DatabaseEntity be DB.AppDb (B.TableEntity Storage.CaseT)
dbTable = DB._case DB.appDb

create :: Storage.Case -> L.Flow ()
create Storage.Case {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Case {..})
    >>= either DB.throwDBError pure

findAllByType :: Integer -> Integer -> Storage.CaseType -> Storage.CaseStatus -> L.Flow [Storage.Case]
findAllByType limit offset caseType caseStatus =
  DB.findAllWithLimitOffsetWhere dbTable (predicate caseType caseStatus) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate caseType caseStatus Storage.Case {..} =
      ( _type ==. (B.val_ caseType)
          &&. _status ==. (B.val_ caseStatus)
      )

findById :: CaseId -> L.Flow Storage.Case
findById caseId =
  DB.findOneWithErr dbTable (predicate caseId)
  where
    predicate caseId Storage.Case {..} = _id ==. (B.val_ caseId)


updateStatus :: CaseId -> Storage.CaseStatus  -> L.Flow ()
updateStatus id status = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update
    dbTable
    (setClause status currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause status currTime Storage.Case {..} =
      mconcat
         [ _status <-. B.val_ status,
            _updatedAt <-. B.val_ currTime
         ]

    predicate id Storage.Case {..} = _id ==. B.val_ id
