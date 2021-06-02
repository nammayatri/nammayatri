{-# LANGUAGE RecordWildCards #-}

module Storage.Queries.RegistrationToken where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.RegistrationToken as Storage
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Error
import qualified Types.Storage.DB as DB
import Utils.Common

getDbTable ::
  Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RegistrationTokenT))
getDbTable =
  DB.registrationToken . DB.transporterDb <$> getSchemaName

create :: Storage.RegistrationToken -> Flow ()
create Storage.RegistrationToken {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.RegistrationToken {..})

findRegistrationToken :: Text -> Flow (Maybe Storage.RegistrationToken)
findRegistrationToken tokenId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.RegistrationToken {..} = id ==. B.val_ tokenId

updateVerified :: Text -> Bool -> Flow ()
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

verifyToken :: RegToken -> Flow Storage.RegistrationToken
verifyToken regToken = do
  logInfo "Verifying Token"
  findRegistrationTokenByToken regToken >>= fromMaybeM (InvalidToken regToken)

findRegistrationTokenByToken :: RegToken -> Flow (Maybe Storage.RegistrationToken)
findRegistrationTokenByToken regToken = do
  dbTable <- getDbTable
  DB.findOne dbTable (predicate regToken)
  where
    predicate token_ Storage.RegistrationToken {..} = token ==. B.val_ token_

updateAttempts :: Int -> Text -> Flow Storage.RegistrationToken
updateAttempts attemps rtId = do
  dbTable <- getDbTable
  now <- getCurrentTime
  DB.update dbTable (setClause attemps now) (predicate rtId)
  findRegistrationToken rtId >>= fromMaybeM (TokenNotFound rtId)
  where
    predicate i Storage.RegistrationToken {..} = id ==. B.val_ i
    setClause a n Storage.RegistrationToken {..} =
      mconcat [attempts <-. B.val_ a, updatedAt <-. B.val_ n]

deleteByEntitiyId :: Text -> Flow ()
deleteByEntitiyId rtId = do
  dbTable <- getDbTable
  DB.delete dbTable (predicate rtId)
  where
    predicate rtid Storage.RegistrationToken {..} = entityId ==. B.val_ rtid
