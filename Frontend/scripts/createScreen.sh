#!/bin/bash
#creating a new screen given the directory name 

cd ..

lowercase=$(echo "$1" | tr '[:upper:]' '[:lower:]')
lowerCamelCase=$(echo "${1:0:1}" | tr '[:upper:]' '[:lower:]')${1:1}
upperSnakeCase=$(echo "$lowerCamelCase" | sed -e 's/\([a-z]\)\([A-Z]\)/\1_\2/g' -e 's/\([a-zA-Z]\+\)/\U&/g')
capitalized=$(echo "$upperSnakeCase" | tr '[:lower:]' '[:upper:]')


updateTypes () {
  # ----------------------------- updating Types.purs -----------------------------------------------

  echo "

-- ######################################### $1ScreenState ####################################################

type $1ScreenState = {
  data :: $1ScreenData ,
  props :: $1ScreenProps
}

type $1ScreenData = {} 

type $1ScreenProps = {} " >> $2/src/Screens/Types.purs

}

updateFlow () {
  #----------------------------------------- Flow.purs ------------------------------------------
  echo " 
  
-- ######################################### $1ScreenFlow ####################################################

${lowercase}ScreenFlow :: FlowBT String Unit 
${lowercase}ScreenFlow = do 
  _ <- UI.${lowerCamelCase}Screen
  pure unit" >> "$2/src/Flow.purs"

}

updateAppFile () {
  #--------------------------- App.purs --------------------------
  appfile="$2/src/App.purs"
  echo "
  | $1ScreenStateType ($1ScreenState -> $1ScreenState)" >> "$appfile"

  awk -v il="data ${capitalized}_SCREEN_OUTPUT = DummyScreenOutput" -v nl="" '/^data ScreenType =/ { print il; print nl; } 1' "$appfile" > temp_file
  mv temp_file "$appfile"
  # rm temp_file

  sed -i '' '/^defaultGlobalState :: GlobalState/ { n; s/$/\n'"  ${lowerCamelCase}Screen : $1ScreenData.initData , "'/; }' "$appfile"
  sed -i '' '/^newtype GlobalState = GlobalState/ { n; s/$/\n'"    ${lowerCamelCase}Screen :: $1ScreenState , "'/; }' "$appfile"
  sed -i '' '/^import Screens.Types/ s/.$//' "$appfile"
  sed -i '' '/^import Screens.Types (/ s/$/, '"$1ScreenState ) "'/;' "$appfile"
  sed -i '' '/^import Screens.Types/ {s/^\(.*\)$/import Screens.'"$1"'Screen.ScreenData as '"$1"'ScreenData\n\1/;}' "$appfile"
}




#------------------------------------------ ModifyScreenTypes.purs -----------------------------------------------
sed -i '' '/^modifyScreenState st =/ { 
    n; 
    s/$/\n    '"$1"'ScreenStateType a -> modifyState (\\(GlobalState state) -> GlobalState \$ state{ '"$lowerCamelCase"'Screen = a state.'"$lowerCamelCase"'Screen}) /; 
}' "$2/src/ModifyScreenTypes.purs"


#--------------------------- Handlers.purs --------------------------
echo "
import Screens.$1Screen.Handler (${lowerCamelCase}Screen) as UI" >> "$2/src/Screens/Handlers.purs"

# -------------------------------------- ScreenData.purs ---------------------------------------

mkdir $2/src/Screens/$1
cd $2/src/Screens/$1

cd ../../../..

# --------------------------------------- generating Views ----------------------------------------
sh ./scripts/createView.sh $1 $2 $2/src/Screens/$1
sh ./scripts/createController.sh $1 $2 $2/src/Screens/$1
sh ./scripts/createHandler.sh $1 $2 $2/src/Screens/$1
sh ./scripts/createComponentConfig.sh $1 $2 $2/src/Screens/$1
sh ./scripts/createScreenData.sh $1 $2 $2/src/Screens/$1

updateTypes $1 $2
updateFlow $1 $2
updateAppFile $1 $2
