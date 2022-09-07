{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Person where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Control.Applicative (liftA2)
import Domain.Types.Person as Person
import Storage.Tabular.Person

create :: Person -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Person ->
  m (Maybe Person)
findById = Esq.findById

findByEmailAndPassword ::
  (Transactionable m, EncFlow m r) =>
  Text ->
  Text ->
  m (Maybe Person)
findByEmailAndPassword email_ password = do
  emailDbHash <- getDbHash email_
  passwordDbHash <- getDbHash password
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmailHash ==. val emailDbHash
        &&. person ^. PersonPasswordHash ==. val passwordDbHash
    return person

findAllWithLimitOffset ::
  ( Transactionable m,
    EncFlow m r
  ) =>
  Maybe Text ->
  Maybe Integer ->
  Maybe Integer ->
  m [Person]
findAllWithLimitOffset mbSearchString mbLimit mbOffset = do
  mbSearchStrDBHash <- getDbHash `traverse` mbSearchString
  findAll $ do
    person <- from $ table @PersonT
    where_ $
      Esq.whenJust_ (liftA2 (,) mbSearchString mbSearchStrDBHash) (filterBySearchString person)
    orderBy [desc $ person ^. PersonCreatedAt]
    limit limitVal
    offset offsetVal
    return person
  where
    limitVal = maybe 100 fromIntegral mbLimit
    offsetVal = maybe 0 fromIntegral mbOffset

    filterBySearchString person (searchStr, searchStrDBHash) = do
      let likeSearchStr = (%) ++. val searchStr ++. (%)
      ( concat_ @Text [unMaybe $ person ^. PersonFirstName, val " ", unMaybe $ person ^. PersonLastName]
          `ilike` likeSearchStr
        )
        ||. person ^. PersonMobileNumberHash ==. val (Just searchStrDBHash) --find by email also?
    unMaybe = maybe_ (val "") identity
