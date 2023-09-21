module Common.Styles.Colors where

import Styles.Types
import MerchantConfig.Utils (getValueFromConfig)

-- General Colors
black :: Color
black = "#000000"

black500 :: Color
black500 = "#B9BABE"

black600 :: Color
black600 = "#A7A7A7"

black650 :: Color
black650 = "#868B98"

black700 :: Color
black700 = "#6D7280"

black9000 :: Color
black9000 = "#CC2C2F3A"

blueGrey :: Color
blueGrey = "#9899a1"

blue600 :: Color
blue600 = "#f4F7FF"

blue700 :: Color
blue700 = "#80B2FF"

blue800 :: Color
blue800 = "#2194FF"

blue900 :: Color
blue900 = "#0066FF"

grey700 :: Color
grey700 = "#F4F4F6"

grey800 :: Color
grey800 = "#F1F1F1"

grey900 :: Color
grey900 = "#E5E7EB"

greyDavy :: Color
greyDavy = "#555555"

lightMaroon :: Color
lightMaroon = "#E45454"

red100 :: Color
red100 = "#FEF8F8"

white900 :: Color
white900 = "#FFFFFF"

yellow900 :: Color
yellow900 = "#FCC32C"

-- Theme Colors

borderColorLight :: Color
borderColorLight = "#e4e4e4"

primaryButtonColor :: Color
primaryButtonColor = "#FDD836"

textSecondary :: Color
textSecondary = "#5B5E69"

warningRed :: Color
warningRed  = "#f04c31"

green900 :: Color 
green900 = "#53BB6F"

yellow100 :: Color
yellow100 = "#FFFDF7"

green100 :: Color
green100 = "#F8FCF9"

dustyRed :: Color
dustyRed = "#BF4A4E"

selectiveYellow :: Color
selectiveYellow = "#FCB700"

pearl :: Color 
pearl = "#FFECED"

floralWhite :: Color
floralWhite = "#FFFAEE"

orange900 :: Color
orange900 = "#FF8533"

pickledBlue :: Color
pickledBlue = "#29334A"

linen :: Color 
linen = "#FEF1E5" 

pigmentGreen :: Color 
pigmentGreen = "#40AA5C" 

almond :: Color 
almond = "#F0DCCA"

frenchSkyBlue :: Color 
frenchSkyBlue = "#80B2FF"

blackLessTrans :: Color
blackLessTrans = "#99000000"

transparent :: Color
transparent = "#00FFFFFF"


black800 :: Color
black800 = getValueFromConfig "black800"

black900 :: Color
black900 = getValueFromConfig "black900"

red :: Color
red = getValueFromConfig "red"

blueMagenta :: Color 
blueMagenta = "#9747FF"

darkCharcoal :: Color
darkCharcoal = "#333333"

pink :: Color 
pink = "#FEF1E5"

blue650 :: Color
blue650 = "#E3EAFF"

yellow500 :: Color
yellow500 = "#F5C061"

yellow700 :: Color
yellow700 = "#F09A01"