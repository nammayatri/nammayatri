{-# LANGUAGE RecordWildCards #-}

module Storage.Queries.RegistrationToken where

import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Error
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Person as SP
import qualified Types.Storage.RegistrationToken as Storage
import Utils.Common

getDbTable ::
  (Functor m, HasSchemaName m) =>
  m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RegistrationTokenT))
getDbTable =
  DB.registrationToken . DB.transporterDb <$> getSchemaName

create :: DBFlow m r => Storage.RegistrationToken -> m ()
create Storage.RegistrationToken {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.RegistrationToken {..})

findRegistrationToken :: DBFlow m r => Text -> m (Maybe Storage.RegistrationToken)
findRegistrationToken tokenId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.RegistrationToken {..} = id ==. B.val_ tokenId

updateVerified :: DBFlow m r => Text -> Bool -> m ()
updateVerified rtId verified_ = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause verified_ now) (predicate rtId)
  where
    setClause scVerified currTime Storage.RegistrationToken {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          verified <-. B.val_ scVerified
        ]
    predicate rtid Storage.RegistrationToken {..} = id ==. B.val_ rtid

findRegistrationTokenByToken :: DBFlow m r => RegToken -> m (Maybe Storage.RegistrationToken)
findRegistrationTokenByToken regToken = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate regToken)
  where
    predicate token_ Storage.RegistrationToken {..} = token ==. B.val_ token_

updateAttempts :: DBFlow m r => Int -> Text -> m Storage.RegistrationToken
updateAttempts attemps rtId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause attemps now) (predicate rtId)
  findRegistrationToken rtId >>= fromMaybeM (TokenNotFound rtId)
  where
    predicate i Storage.RegistrationToken {..} = id ==. B.val_ i
    setClause a n Storage.RegistrationToken {..} =
      mconcat [attempts <-. B.val_ a, updatedAt <-. B.val_ n]

deleteByEntitiyId :: Text -> DB.SqlDB ()
deleteByEntitiyId id_ = do
  dbTable <- getDbTable
  DB.delete' dbTable (predicate id_)
  where
    predicate rtid Storage.RegistrationToken {..} = entityId ==. B.val_ rtid

deleteByEntitiyIdExceptNew :: DBFlow m r => Text -> Id Storage.RegistrationToken -> m ()
deleteByEntitiyIdExceptNew id_ (Id newRT) = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate id_ newRT)
  where
    predicate rtid newRTId Storage.RegistrationToken {..} =
      entityId ==. B.val_ rtid
        B.&&. B.not_ (id B.==. B.val_ newRTId)

findAllByPersonId :: DBFlow m r => Id SP.Person -> m [Storage.RegistrationToken]
findAllByPersonId personId = do
  dbTable <- getDbTable
  DB.findAll dbTable identity (predicate $ getId personId)
  where
    predicate persId Storage.RegistrationToken {..} =
      entityId ==. B.val_ persId
