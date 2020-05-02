module Beckn.Storage.Queries.PassApplication where

import qualified Beckn.Storage.Queries               as DB
import           Beckn.Types.App
import qualified Beckn.Types.Common                  as Storage (PassType (..))
import qualified Beckn.Types.Storage.DB              as DB
import qualified Beckn.Types.Storage.PassApplication as Storage
import           Beckn.Utils.Common
import           Data.Time.LocalTime
import           Database.Beam                       ((&&.), (<-.), (==.),
                                                      (||.))
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
     PassApplicationId -> L.Flow (Maybe Storage.PassApplication)
findById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.PassApplication {..} = (_id ==. B.val_ id)

findAllWithLimitOffsetWhere ::
 [Int]
  -> [Text]
  -> [Text]
  -> [Text]
  -> [Text]
  -> [Int]
  -> [Text]
  -> [Text]
  -> [Text]
  -> [Text]
  -> [Storage.Status]
  -> [OrganizationId]
  -> [Storage.PassType]
  -> Maybe Int -> Maybe Int -> L.Flow [Storage.PassApplication]
findAllWithLimitOffsetWhere fPins fCities fDists fWards fStates toPins toCities toDists toWards toStates statuses orgIds passType mlimit moffset =
  DB.findAllWithLimitOffsetWhere dbTable (predicate fPins fCities fDists fWards fStates toPins toCities toDists toWards toStates statuses orgIds passType) limit offset orderByDesc
    >>= either DB.throwDBError pure
  where
    limit = (toInteger $ fromMaybe 100 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)
    orderByDesc Storage.PassApplication {..} = B.desc_ _createdAt

    predicate fPins fCities fDists fWards fStates toPins toCities toDists toWards toStates statuses orgIds ptypes
      Storage.PassApplication {..} =
        foldl (&&.)
          (B.val_ True)
          [ _passType `B.in_` (B.val_ <$> ptypes) ||. complementVal ptypes
          , _status `B.in_` (B.val_ <$> statuses) ||. complementVal statuses
          , _OrganizationId `B.in_` ((B.val_ . Just) <$> orgIds) ||. complementVal orgIds
          , _toPincode `B.in_` ((B.val_ . Just) <$> toPins) ||. complementVal toPins
          , _toCity `B.in_` ((B.val_ . Just) <$> toCities) ||. complementVal toCities
          , _toState `B.in_` ((B.val_ . Just) <$> toStates) ||. complementVal toStates
          , _toDistrict `B.in_` ((B.val_ . Just) <$> toDists) ||. complementVal toDists
          , _toWard `B.in_` ((B.val_ . Just) <$> toWards) ||. complementVal toWards
          , _fromPincode `B.in_` ((B.val_ . Just) <$> fPins) ||. complementVal fPins
          , _fromCity `B.in_` ((B.val_ . Just) <$> fCities) ||. complementVal fCities
          , _fromState `B.in_` ((B.val_ . Just) <$> fStates) ||. complementVal fStates
          , _fromDistrict `B.in_` ((B.val_ . Just) <$> fDists) ||. complementVal fDists
          , _fromWard `B.in_` ((B.val_ . Just) <$> fWards) ||. complementVal fWards
          ]

complementVal l
  | (null l) = B.val_ True
  | otherwise = B.val_ False



update :: PassApplicationId -> Storage.Status -> Int -> Text -> L.Flow ()
update id status approvedCount remarks = do
  (currTime :: LocalTime) <- getCurrTime
  DB.update dbTable
    (setClause status approvedCount remarks currTime)
    (predicate id)
    >>= either DB.throwDBError pure
  where
    setClause status approvedCount remarks currTime Storage.PassApplication {..} =
      mconcat
        [ _status <-. B.val_ status
        , _approvedCount <-. B.val_ approvedCount
        , _remarks <-. B.val_ remarks
        , _updatedAt <-. B.val_ currTime
        ]

    predicate id Storage.PassApplication {..} = _id ==. B.val_ id
