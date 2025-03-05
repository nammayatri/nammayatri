module Common.Styles.Colors where

import Styles.Types
import ConfigProvider

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

black712 :: Color
black712 = "#806D7280"

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

blue9000 :: Color
blue9000 = "#1A0066FF"

grey700 :: Color
grey700 = "#F4F4F6"

grey800 :: Color
grey800 = "#F1F1F1"

grey900 :: Color
grey900 = "#E5E7EB"

green400 :: Color
green400 = "#26A047"

greyDavy :: Color
greyDavy = "#555555"

lightMaroon :: Color
lightMaroon = "#E45454"

red100 :: Color
red100 = "#FEF8F8"

white900 :: Color
white900 = "#FFFFFF"

white40Alpha :: Color
white40Alpha = "#66FFFFFF"

white27 ::Color
white27 = "#45FCFCFC"

white300 :: Color
white300 = "#FDD56B"

white200 :: Color
white200 = "#99FFFFFF"

yellow900 :: Color
yellow900 = "#FCC32C"

yellow600 :: Color
yellow600 = "#FCF6E7"

yellowOpacity23 :: Color
yellowOpacity23 = "#3BFCC32C"

shadowGray :: Color
shadowGray = "#D1D5DB"
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

greenAlpha :: Color
greenAlpha = "#1A53BB6F"

yellow100 :: Color
yellow100 = "#FFFDF7"

greenOpacity10 :: Color
greenOpacity10 = "#1A53BB6F"

green300 :: Color
green300 = "#DDFFEB"

green100 :: Color
green100 = "#1A53BB6F"

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

orange200 :: Color
orange200 = "#FFE7C2"

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

frenchSkyBlue800 :: Color
frenchSkyBlue800 = "#99c1ff"

frenchSkyBlue400 :: Color
frenchSkyBlue400 = "#a9cbff"

green700 :: Color
green700 = "#268C6E"

blackLessTrans :: Color
blackLessTrans = "#99000000"

transparent :: Color
transparent = "#00FFFFFF"

transparentMid :: Color
transparentMid = "#60FFFFFF"

transparentHigh :: Color
transparentHigh = "#99FFFFFF"

black800 :: Color
black800 =
  let config = getAppConfig appConfig
  in config.colors.black800

black900 :: Color
black900 =   let config = getAppConfig appConfig
  in config.colors.black900

black6000 :: Color
black6000 = "#565961"

black400 :: Color
black400 = "#40000000"

red :: Color
red = let config = getAppConfig appConfig
  in config.colors.red

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

yellow800 :: Color
yellow800 = "#FEEBB9"

yellowOpacity16 :: Color
yellowOpacity16 = "#29FCC32C"

yellowOpacity10 :: Color
yellowOpacity10 = "#1AFCC32C"

greenOpacity16 :: Color
greenOpacity16 = "#2953BB6F"

redOpacity30 :: Color
redOpacity30 = "#4DE55454"

redOpacity20 :: Color
redOpacity20 = "#33E55454"

redOpacity16 :: Color
redOpacity16 = "#29E55454"

redOpacity10 :: Color
redOpacity10 = "#1AE55454"

fadedPurple :: Color
fadedPurple = "#339747FF"

red900 :: Color
red900 = "#E55454"

red900Alpha16 :: Color
red900Alpha16 = "#29E55454"

yellow200 :: Color
yellow200 = "#33FCC32C"

black200 :: Color
black200 = "#1A2C2F3A"

black80 :: Color
black80 = "#142C2F3A"

purple100 :: Color
purple100 = "#1A9747FF"

blue100 :: Color
blue100 = "#1A2194FF"

green600 :: Color
green600 = "#269574"

ghostWhite :: Color
ghostWhite = "#F3F4F8"

manatee200 :: Color
manatee200 = "#206D7280"

blue200 :: Color
blue200 = "#202194FF"

carnation100 :: Color
carnation100 = "#10E55454"

squidInkBlue :: Color
squidInkBlue = "#303440"

rippleShade :: Color
rippleShade = "#D5D5D8"
green200 :: Color
green200 = "#2053BB6F"

transparentGrey :: Color
transparentGrey = "#66888888"

brightBlue :: Color
brightBlue = "#0066FF"

yellowOpacity40 :: Color
yellowOpacity40 = "#66FEEBB9"

yellowOpacity50 :: Color
yellowOpacity50 = "#80FCC32C"

red600 :: Color
red600 = "#FFDDDD"

yellowCoinGradient1 ::Color
yellowCoinGradient1 = "#FFF8EE"

yellowCoinGradient2 ::Color
yellowCoinGradient2 = "#FFEBCC"

blue400 :: Color
blue400 = "#662194FF"

atlantisGreen :: Color
atlantisGreen = "#E1E7F5"

greenCoin :: Color
greenCoin = "#1A53BB6F"

blackOpacity12 :: Color
blackOpacity12 = "#373A45"

ivory :: Color
ivory = "#FFF9EA"

red200 :: Color
red200 = "#FFE0E0"

darkRed :: Color
darkRed = "#DC1C4A"

tealishGreen :: Color
tealishGreen = "#E5F5E9"

darkGradientBlue :: Color
darkGradientBlue = "#E2EAFF"

lightBlueTeal :: Color
lightBlueTeal = "#B2E4EF"
black7000 :: Color
black7000 = "#B32C2F3A"

white13 :: Color
white13 = "#21000000"

black150 :: Color
black150 = "#373F50"

policeBlue :: Color
policeBlue = "#2A5465"

azureWhite :: Color
azureWhite = "#D6EBF5"

mountainFig :: Color
mountainFig = "#373B49"


blueGreen :: Color
blueGreen = "#42B8BA"

blueGreenBg :: Color
blueGreenBg = "#E8F5F6"

azureine :: Color
azureine = "#0157D8"

lightCyan :: Color
lightCyan = "#EEF3FF"

white800 :: Color
white800 = "#80FFFFFF"

tealBlue :: Color
tealBlue = "#3D475C"

transparentWhite :: Color
transparentWhite = "#30FFFFFF"

greyDark :: Color
greyDark = "#D8D8D8"

greyBackDarkColor :: Color
greyBackDarkColor = "#D3D3D3"

black12 :: Color
black12 = "#1F000000"

aliceBlueLight :: Color
aliceBlueLight = "#F4F7FF"

metroBlue :: Color
metroBlue = "#2250BF"

pumpkin30 :: Color
pumpkin30 = "#4DFF8533"

pumpkin10 :: Color
pumpkin10 = "#1AFF8533"

seaShell :: Color
seaShell = "#FFF8F4"

darkOrange :: Color
darkOrange = "#F88600"

dodgerBlue30 :: Color
dodgerBlue30 = "#4D2194FF"

dodgerBlue8 :: Color
dodgerBlue8 = "#142194FF"

indianRed8 :: Color
indianRed8 = "#14E55454"

lightBlue80 :: Color
lightBlue80 = "#CC80B2FF"

surfaceRed :: Color
surfaceRed = "#FDEAEA"

limeGreen :: Color
limeGreen = "#3AA215"

brownishYellow :: Color
brownishYellow = "#FFCC6E"

argent :: Color
argent = "#888888"
