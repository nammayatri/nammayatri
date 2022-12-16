{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Person where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Control.Applicative (liftA2)
import Database.Esqueleto.PostgreSQL
import Domain.Types.Merchant as Merchant
import Domain.Types.Person as Person
import Domain.Types.Role as Role
import Storage.Tabular.Merchant as Merchant
import Storage.Tabular.MerchantAccess as Access
import Storage.Tabular.Person as Person
import Storage.Tabular.Role as Role

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
findByEmailAndPassword email password = do
  emailDbHash <- getDbHash email
  passwordDbHash <- getDbHash password
  findOne $ do
    person <- from $ table @PersonT
    where_ $
      person ^. PersonEmailHash ==. val emailDbHash
        &&. person ^. PersonPasswordHash ==. val passwordDbHash
    return person

-- TODO add filtering by role
findAllWithLimitOffset ::
  Transactionable m =>
  Maybe Text ->
  Maybe DbHash ->
  Maybe Integer ->
  Maybe Integer ->
  m [(Person, Role, [ShortId Merchant.Merchant])]
findAllWithLimitOffset mbSearchString mbSearchStrDBHash mbLimit mbOffset =
  fromMaybeList <$> do
    findAll $ do
      merchantAccessAggTable <- with $ do
        (merchantAccess :& merchant) <-
          from $
            table @MerchantAccessT
              `innerJoin` table @MerchantT
                `Esq.on` ( \(merchantAccess :& merchant) ->
                             merchantAccess ^. Access.MerchantAccessMerchantId ==. merchant ^. Merchant.MerchantTId
                         )
        groupBy (merchantAccess ^. MerchantAccessPersonId)
        return (merchantAccess ^. MerchantAccessPersonId, arrayAgg (merchant ^. MerchantShortId))

      (person :& role :& (_, mbMerchantShortIds)) <-
        from $
          table @PersonT
            `innerJoin` table @RoleT
              `Esq.on` ( \(person :& role) ->
                           person ^. Person.PersonRoleId ==. role ^. Role.RoleTId
                       )
            `leftJoin` merchantAccessAggTable
              `Esq.on` ( \(person :& _ :& (mbPersonId, _mbMerchantShortIds)) ->
                           just (person ^. Person.PersonTId) ==. mbPersonId
                       )
      where_ $
        Esq.whenJust_ (liftA2 (,) mbSearchString mbSearchStrDBHash) (filterBySearchString person)
      orderBy [desc $ person ^. PersonCreatedAt]
      limit limitVal
      offset offsetVal
      return (person, role, mbMerchantShortIds)
  where
    limitVal = maybe 100 fromIntegral mbLimit
    offsetVal = maybe 0 fromIntegral mbOffset

    filterBySearchString person (searchStr, searchStrDBHash) = do
      let likeSearchStr = (%) ++. val searchStr ++. (%)
      ( concat_ @Text [person ^. PersonFirstName, person ^. PersonLastName]
          `ilike` likeSearchStr
        )
        ||. person ^. PersonMobileNumberHash ==. val searchStrDBHash --find by email also?
    fromMaybeList :: [(Person, Role, Maybe [Text])] -> [(Person, Role, [ShortId Merchant.Merchant])]
    fromMaybeList = map (\(person, role, mbMerchantShortIds) -> (person, role, ShortId <$> fromMaybe [] mbMerchantShortIds))

updatePersonRole :: Id Person -> Id Role -> SqlDB ()
updatePersonRole personId roleId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonRoleId =. val (toKey roleId),
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)

updatePersonPassword :: Id Person -> DbHash -> SqlDB ()
updatePersonPassword personId newPasswordHash = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonPasswordHash =. val newPasswordHash,
        PersonUpdatedAt =. val now
      ]
    where_ $ tbl ^. PersonTId ==. val (toKey personId)
