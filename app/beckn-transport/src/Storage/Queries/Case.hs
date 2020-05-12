module Storage.Queries.Case where

import qualified Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.App
import qualified Beckn.Types.Storage.Case as Storage
import           Beckn.Utils.Common
import           Data.Time
import           Database.Beam ((&&.), (<-.), (==.), (||.))
import qualified Database.Beam as B
import qualified EulerHS.Language as L
import           EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T
import           Types.App
import qualified Types.Storage.DB as DB

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.CaseT)
dbTable = DB._case DB.transporterDb

create :: Storage.Case -> L.Flow ()
create Storage.Case {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Case {..})
    >>= either DB.throwDBError pure

findAllByType :: Integer -> Integer -> Storage.CaseType -> Storage.CaseStatus -> LocaTime -> L.Flow [Storage.Case]
findAllByType limit offset caseType caseStatus now =
  DB.findAllWithLimitOffsetWhere dbTable (predicate caseType caseStatus now) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    orderByDesc Storage.Case {..} = B.desc_ _createdAt
    predicate caseType caseStatus Storage.Case {..} =
      ( _type ==. (B.val_ caseType)
         &&. _status ==. (B.val_ caseStatus)
          &&. _validTill >. (B.val_ now))

findAllByIds :: [CaseId] ->  L.Flow [Storage.Case]
findAllByIds ids =
  DB.findAllOrErr dbTable (pred ids)
  where
    pred ids Storage.Case {..} =
     B.in_ _id (B.val_ <$> ids)

findById :: CaseId -> L.Flow Storage.Case
findById caseId =
  DB.findOneWithErr dbTable (predicate caseId)
  where
    predicate caseId Storage.Case {..} = _id ==. (B.val_ caseId)

