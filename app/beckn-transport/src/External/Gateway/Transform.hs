{-# LANGUAGE OverloadedLabels #-}

module External.Gateway.Transform where

import App.Types
import Beckn.Types.App
import Beckn.Types.Core.Amount
import Beckn.Types.Core.Api
import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Category
import Beckn.Types.Core.Contact
import Beckn.Types.Core.Context
import Beckn.Types.Core.Item
import Beckn.Types.Core.Person as BPerson
import Beckn.Types.Core.Price
import Beckn.Types.Core.Provider
import Beckn.Types.Mobility.Driver
import Beckn.Types.Mobility.Service
import Beckn.Types.Mobility.Tracking
import Beckn.Types.Mobility.Trip
import Beckn.Types.Mobility.Vehicle as BVehicle
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Organization as Organization
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.Products as Product
import Beckn.Types.Storage.Vehicle as Vehicle
import Data.Aeson
import Data.Map
import Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Utils.Defaults as Defaults

mkCatalog :: [ProductInstance] -> Catalog
mkCatalog prodInsts =
  Catalog
    { _category_tree = Category {_id = "", _subcategories = []},
      _items = mkItem <$> prodInsts
    }

mkItem :: ProductInstance -> Item
mkItem prodInst =
  Item
    { _id = _getProductInstanceId $ prodInst ^. #_id,
      _description = "",
      _name = "",
      _image = Nothing,
      _price = mkPrice prodInst,
      _primary = False,
      _selected = False,
      _quantity = 1,
      _policy = Nothing,
      _category_id = "",
      _tags = []
    }

mkPrice :: ProductInstance -> Price
mkPrice prodInst =
  Price
    { _currency = "INR", -- TODO : Fetch this from product
      _estimated_value = prodInst ^. #_price,
      _computed_value = prodInst ^. #_price,
      _listed_value = prodInst ^. #_price,
      _offered_value = prodInst ^. #_price,
      _unit = "Rs", -- TODO : Fetch this from product
      _discount = 0.0,
      _tax = Nothing
    }

mkServiceOffer :: Case -> [ProductInstance] -> [ProductInstance] -> Maybe Trip -> Maybe Organization -> Flow Service
mkServiceOffer c pis allPis trip orgInfo =
  let x =
        Service
          { _id = _getCaseId $ c ^. #_id,
            _catalog = Just $ mkCatalog pis,
            _matched_items = _getProductInstanceId . ProductInstance._id <$> pis,
            _selected_items =
              catMaybes $
                ( \x ->
                    if x ^. #_status == ProductInstance.CONFIRMED
                      then Just (_getProductInstanceId $ x ^. #_id)
                      else Nothing
                )
                  <$> allPis,
            _fare_product = Nothing,
            _offers = [],
            _provider = mkProvider <$> orgInfo,
            _trip = trip,
            _policies = [],
            _billing_address = Nothing
          }
   in return x

baseTrackingUrl :: Text
baseTrackingUrl = "http://api.sandbox.beckn.juspay.in/transport/v1/location"

mkTracking :: Text -> Text -> Text -> Tracking
mkTracking method dataUrl embedUrl =
  Tracking
    { method = method,
      pull = if method == "PULL" then Just $ mkPullTrackingData dataUrl embedUrl else Nothing
    }

mkPullTrackingData :: Text -> Text -> PullTrackingData
mkPullTrackingData dataUrl embed =
  PullTrackingData
    { data_url = dataUrl,
      embed_url = embed
    }

mkDriverObj :: Person.Person -> Driver
mkDriverObj person =
  Driver
    { descriptor = mkPerson person,
      experience = Nothing
    }

mkPerson :: Person.Person -> BPerson.Person
mkPerson person =
  BPerson.Person
    { title = "",
      first_name = fromMaybe "" (person ^. #_firstName),
      middle_name = fromMaybe "" (person ^. #_middleName),
      last_name = fromMaybe "" (person ^. #_lastName),
      full_name = fromMaybe "" (person ^. #_fullName),
      image = Nothing,
      dob = Nothing,
      gender = show $ person ^. #_gender,
      contact =
        Contact
          { email = person ^. #_email,
            mobile =
              Just
                Mobile
                  { country_code = person ^. #_mobileCountryCode,
                    number = person ^. #_mobileNumber -- TODO: need to take last 10
                  },
            landline = Nothing,
            ivr = []
          }
    }

mkVehicleObj :: Vehicle.Vehicle -> BVehicle.Vehicle
mkVehicleObj vehicle =
  BVehicle.Vehicle
    { category = show <$> vehicle ^. #_category,
      capacity = vehicle ^. #_capacity,
      make = vehicle ^. #_make,
      model = vehicle ^. #_model,
      size = vehicle ^. #_size,
      variant = maybe "" show (vehicle ^. #_variant),
      color = vehicle ^. #_color,
      energy_type = show <$> vehicle ^. #_energyType,
      registration =
        Just
          Registration
            { category = maybe "COMMERCIAL" show (vehicle ^. #_registrationCategory),
              number = vehicle ^. #_registrationNo
            }
    }

mkProvider :: Organization -> Provider
mkProvider orgInfo =
  Provider
    { _id = _getOrganizationId $ orgInfo ^. #_id,
      _name = orgInfo ^. #_name,
      _website = fromMaybe "" (orgInfo ^. #_callbackUrl),
      _contact = mkContact (orgInfo ^. #_mobileNumber),
      _api = Api "" Defaults.localTime
    }

mkContact :: Maybe Text -> Contact
mkContact mobileNumber =
  Contact
    { email = Nothing,
      mobile = Just (Mobile Nothing mobileNumber),
      landline = Nothing,
      ivr = []
    }
