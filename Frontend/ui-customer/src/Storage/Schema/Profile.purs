module Storage.Schema.Profile where

import Prelude
import Engineering.Helpers.SQLiteUtils
import Services.API
import Data.Maybe

profileSchema :: SqlSchema 
profileSchema = 
  [ { "key" : "middleName", "type" : "string" }
  , { "key" : "lastName", "type" : "string" }
  , { "key" : "maskedDeviceToken", "type" : "string" }
  , { "key" : "firstName", "type" : "string" }
  , { "key" : "id", "type" : "string" }
  , { "key" : "maskedMobileNumber", "type" : "string" }
  , { "key" : "email", "type" : "string" }
  , { "key" : "hasTakenRide", "type" : "boolean" }
  , { "key" : "referralCode", "type" : "string" }
  , { "key" : "language", "type" : "string" }
  , { "key" : "gender", "type" : "string" }
  , { "key" : "disability", "type" : "string" }
  , { "key" : "hasDisability", "type" : "boolean" }
  , { "key" : "hasCompletedSafetySetup", "type" : "boolean" }
  , { "key" : "hasCompletedMockSafetyDrill", "type" : "boolean" }
  , { "key" : "followsRide", "type" : "boolean" }
  ]

type ProfileData = 
  { middleName :: String
  , lastName :: String
  , maskedDeviceToken :: String
  , firstName :: String
  , id :: String
  , maskedMobileNumber :: String
  , email :: String
  , hasTakenRide :: Boolean
  , referralCode :: String
  , language :: String
  , gender :: String
  , disability :: String
  , hasDisability :: Boolean
  , hasCompletedSafetySetup :: Boolean
  , hasCompletedMockSafetyDrill :: Boolean
  , followsRide :: Boolean
  }

transformFromProfileToTable :: GetProfileRes -> Array ProfileData
transformFromProfileToTable profile = 
  let (GetProfileRes unwrappedResp) = profile
  in [{
    middleName : fromMaybe "" unwrappedResp.middleName
  , lastName : fromMaybe "" unwrappedResp.lastName 
  , maskedDeviceToken : fromMaybe "" unwrappedResp.maskedDeviceToken
  , firstName : fromMaybe "" unwrappedResp.firstName
  , id : unwrappedResp.id
  , maskedMobileNumber : fromMaybe "" unwrappedResp.maskedMobileNumber
  , email : fromMaybe "" unwrappedResp.email
  , hasTakenRide : unwrappedResp.hasTakenRide
  , referralCode : fromMaybe "" unwrappedResp.referralCode
  , language : fromMaybe "" unwrappedResp.language
  , gender : fromMaybe "" unwrappedResp.gender
  , disability : fromMaybe "" unwrappedResp.disability
  , hasDisability : fromMaybe false unwrappedResp.hasDisability
  , hasCompletedSafetySetup : fromMaybe true unwrappedResp.hasCompletedSafetySetup
  , hasCompletedMockSafetyDrill :fromMaybe true unwrappedResp.hasCompletedMockSafetyDrill
  , followsRide : fromMaybe false unwrappedResp.followsRide
  }]

transformFromTableToProfile :: ProfileData -> GetProfileRes
transformFromTableToProfile cachedProfile = 
   GetProfileRes {
    middleName : Just cachedProfile.middleName
  , lastName : Just cachedProfile.lastName 
  , maskedDeviceToken : Just cachedProfile.maskedDeviceToken
  , firstName : Just cachedProfile.firstName
  , id : cachedProfile.id
  , maskedMobileNumber : Just cachedProfile.maskedMobileNumber
  , email : Just cachedProfile.email
  , hasTakenRide : cachedProfile.hasTakenRide
  , referralCode : Just cachedProfile.referralCode
  , language : Just cachedProfile.language
  , gender : Just cachedProfile.gender
  , disability : Just cachedProfile.disability
  , hasDisability : Just cachedProfile.hasDisability
  , hasCompletedSafetySetup : Just cachedProfile.hasCompletedSafetySetup
  , hasCompletedMockSafetyDrill : Just cachedProfile.hasCompletedMockSafetyDrill
  , followsRide : Just cachedProfile.followsRide
  , bundleVersion : Nothing
  , clientVersion : Nothing
  }
