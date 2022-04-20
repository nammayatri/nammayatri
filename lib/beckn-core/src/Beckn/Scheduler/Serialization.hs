{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Beckn.Scheduler.Serialization where

import Beckn.Prelude
import Beckn.Scheduler.Error (JobDecodeError (InvalidJobData, InvalidJobType))
import Beckn.Scheduler.Types
import Beckn.Types.Id
import Beckn.Utils.Common (decodeFromText, encodeToText)

class (Eq a, Ord a, Show a) => JobTypeSerializable a where
  jobTypeToText :: a -> Text
  jobTypeFromText :: Text -> Maybe a

class Eq a => JobDataSerializable a where
  jobDataToText :: a -> Text
  jobDataFromText :: Text -> Maybe a

instance JobTypeSerializable Text where
  jobTypeToText = identity
  jobTypeFromText = Just

instance JobDataSerializable Text where
  jobDataToText = identity
  jobDataFromText = Just

deriving via JSONable () instance JobDataSerializable ()

newtype JSONable a = JSONable a
  deriving (Eq, Ord, Show)

instance (Eq a, Ord a, Show a, FromJSON a, ToJSON a) => JobTypeSerializable (JSONable a) where
  jobTypeToText (JSONable x) = encodeToText x
  jobTypeFromText text = JSONable <$> decodeFromText text

instance (Eq a, FromJSON a, ToJSON a) => JobDataSerializable (JSONable a) where
  jobDataToText (JSONable x) = encodeToText x
  jobDataFromText text = JSONable <$> decodeFromText text

encodeJob :: (JobTypeSerializable a, JobDataSerializable b) => Job a b -> JobText
encodeJob Job {..} =
  Job
    { id = cast id,
      jobType = jobTypeToText jobType,
      jobData = jobDataToText jobData,
      ..
    }

decodeJob :: forall a b. (JobTypeSerializable a, JobDataSerializable b) => JobText -> Either JobDecodeError (Job a b)
decodeJob Job {..} = do
  jobType_ <- maybe (Left $ InvalidJobType jobType) Right $ jobTypeFromText jobType
  jobData_ <- maybe (Left $ InvalidJobData jobData) Right $ jobDataFromText jobData
  pure
    Job
      { id = cast id,
        jobType = jobType_,
        jobData = jobData_,
        ..
      }
