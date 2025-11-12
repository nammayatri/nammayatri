{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.Person where

import Data.Text
import qualified Domain.Types.Person as DP
import Kernel.External.Encryption (DbHash (..))
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data PersonT f = PersonT
  { id :: C f (Id DP.Person),
    firstName :: C f Text,
    middleName :: C f (Maybe Text),
    lastName :: C f (Maybe Text),
    mobileNumberHash :: C f (Maybe DbHash)
  }
  deriving (Generic)

deriving instance Show Person

personTTable :: PersonT (FieldModification PersonT)
personTTable =
  PersonT
    { id = "id",
      firstName = "first_name",
      middleName = "middle_name",
      lastName = "last_name",
      mobileNumberHash = "mobile_number_hash"
    }

type Person = PersonT Identity

$(TH.mkClickhouseInstances ''PersonT 'SELECT_FINAL_MODIFIER)

data PersonBasic = PersonBasic
  { personId :: Text,
    firstNameValue :: Text,
    middleNameValue :: Maybe Text,
    lastNameValue :: Maybe Text
  }
  deriving (Show, Eq, Generic)

mkPersonBasic :: (Id DP.Person, Text, Maybe Text, Maybe Text) -> PersonBasic
mkPersonBasic (personId', firstName', middleName', lastName') =
  PersonBasic
    { personId = personId'.getId,
      firstNameValue = firstName',
      middleNameValue = middleName',
      lastNameValue = lastName'
    }

findPersonsByIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Id DP.Person] ->
  Maybe Text ->
  Maybe DbHash ->
  m [PersonBasic]
findPersonsByIds [] _ _ = pure []
findPersonsByIds personIds mbNameFilter mbMobileHash = do
  res <-
    CH.findAll $
      CH.select_
        (\person -> CH.notGrouped (person.id, person.firstName, person.middleName, person.lastName))
        $ CH.filter_
          ( \person ->
              person.id `CH.in_` personIds
                CH.&&. CH.whenJust_ mbNameFilter (\name -> nameFilterPredicate person name)
                CH.&&. CH.whenJust_ mbMobileHash (\hashTxt -> person.mobileNumberHash CH.==. Just hashTxt)
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE personTTable)
  pure $ mkPersonBasic <$> res
  where
    nameFilterPredicate person trimmedName =
      let searchPattern = "%" <> trimmedName <> "%"
          fullName = CH.concat_ (person.firstName :| [CH.valColumn " ", CH.ifNull_ person.middleName (CH.valColumn ""), CH.valColumn " ", CH.ifNull_ person.lastName (CH.valColumn "")])
       in CH.like_ fullName searchPattern
