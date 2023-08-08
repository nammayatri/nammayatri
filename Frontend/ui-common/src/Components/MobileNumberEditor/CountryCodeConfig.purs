module Components.MobileNumberEditor.CountryCodeConfig where

import Common.Types.App (CountryCodeObj)

getCountryCodesObj :: Array CountryCodeObj
getCountryCodesObj = [
  {
    countryName : "Bangladesh" 
  , countryCode  : "+880" 
  , countryShortCode : "BD"
  },
  {
    countryName : "India" 
  , countryCode  : "+91" 
  , countryShortCode : "IN"
  }
]