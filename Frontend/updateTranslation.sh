OUTPUT_FILE="output.csv"
SPREADSHEET_URL="https://docs.google.com/spreadsheets/d/e/2PACX-1vT_la2vbHpCbD39BDDGvivYxLPzYlDKwHWBYQvyax8jrtK-YTp9GDAcTfiRVY2ro06VslD7Vjy4QPM9/pub?gid=0&single=true&output=tsv"
RESOURCE="Resources"
TYPES_FILE="types.txt"
CONSTRUCTOR_COLUMN=1
HELPER_FILE="new_file.txt"
COPYRIGHT_TEXT="../copyrightText.txt"
DI="$1"


if [ "$1" == "ui-driver" ]; then 
    SPREADSHEET_URL="https://docs.google.com/spreadsheets/d/e/2PACX-1vT_la2vbHpCbD39BDDGvivYxLPzYlDKwHWBYQvyax8jrtK-YTp9GDAcTfiRVY2ro06VslD7Vjy4QPM9/pub?gid=641543613&single=true&output=tsv"
    RESOURCE="Resource"
fi

TYPES_FILE_PATH="src/$RESOURCE/Localizable/Types.purs"

curl -sL "$SPREADSHEET_URL" > "$OUTPUT_FILE"

echo "Successfully downloaded the translations --------------------------------- ✓"

TRANSLATED_STR="translatedString.csv"
TRANSLATED_KEY_VALUE="translated_key_value.csv"
FINAL_OUTPUT="final_output.csv"


awk 'NR>1 && !/#VALUE!/' "$OUTPUT_FILE" > "$TRANSLATED_STR" 

updateTypesFile() {

    > "$TYPES_FILE_PATH"

    # adding the copy right text to the Types.purs file
    cat "$COPYRIGHT_TEXT" >> "$TYPES_FILE_PATH"
    echo "

module Language.Types where

data STR =" >> "$TYPES_FILE_PATH"
    # updating Types.purs file with the constructors 
    awk -F'\t' -v first_col="$CONSTRUCTOR_COLUMN" '{gsub(/^[ \t]+|[ \t]+$/, "", $first_col); print "       | " $first_col }' "$TRANSLATED_STR" > "$TYPES_FILE"
    sed '1 s/|//' "$TYPES_FILE" > "$HELPER_FILE"
    sed 's/ (stringForConfig)/ String/g' "$HELPER_FILE" > "$TYPES_FILE"
    cat "$TYPES_FILE" >> "$TYPES_FILE_PATH"

    echo "Generated the Types.purs file ----------------------------------------- ✓"
}


addImportStatement(){
    echo "module Resources.Localizable.${1} where

import Language.Types (STR(..))

get${PATH_TO_STRINGS_FILE} :: STR -> String
get${PATH_TO_STRINGS_FILE} stringKey = 
    case stringKey of" >> "$FINAL_OUTPUT"
    }


generateStringsFile () {

    > "$FINAL_OUTPUT"
    > "$TRANSLATED_KEY_VALUE"

        # removing empty values from the translated string column
    COLUMN_INDEX=$1
    CONSTRUCTOR_COLUMN=1
    PATH_TO_STRINGS_FILE="EN"

    case $COLUMN_INDEX in
      2)  PATH_TO_STRINGS_FILE="EN" ;;
      3)  PATH_TO_STRINGS_FILE="HI" ;;
      4)  PATH_TO_STRINGS_FILE="BN" ;;
      5)  PATH_TO_STRINGS_FILE="TA" ;;
      6)  PATH_TO_STRINGS_FILE="TE" ;;
      7)  PATH_TO_STRINGS_FILE="KN" ;;
      8)  PATH_TO_STRINGS_FILE="ML" ;;
      9)  PATH_TO_STRINGS_FILE="FR" ;;
      *)  PATH_TO_STRINGS_FILE="EN" ;;
    esac
   
    # adding import statements and function declaration
    addImportStatement "$PATH_TO_STRINGS_FILE"

    # converting the translated string column to key value pair
    # gsub(/^[ \t]+|[ \t]+$/, "", $first_col -> substituting the leading and trailing spaces with empty string
    # print "       " ,  -> concatenating tab space before the key value pair
    # $first_col " -> \"" $target_col "\"" -> first_col ( constructor STR ) , target_col ( translated string )
    
    awk -F'\t' -v first_col="$CONSTRUCTOR_COLUMN" -v target_col="$COLUMN_INDEX" '{gsub(/^[ \t]+|[ \t]+$/, "", $first_col); print "       " , $first_col " -> \"" $target_col "\"" }' "$TRANSLATED_STR" > "$TRANSLATED_KEY_VALUE" 
    > "$HELPER_FILE"
    cat "$FINAL_OUTPUT" >> "$HELPER_FILE"
    # adding the key value pair to the final output file
    cat "$TRANSLATED_KEY_VALUE" >> "$HELPER_FILE"
    sed 's/ (stringForConfig)/ stringForConfig/g' "$HELPER_FILE" > "$FINAL_OUTPUT"
    cat "$FINAL_OUTPUT" > "src/$RESOURCE/Localizable/$PATH_TO_STRINGS_FILE.purs"
    
    echo "Generated the $PATH_TO_STRINGS_FILE.purs file ------------------------- ✓"

} 

# function call to update the Types.purs file
updateTypesFile ""

for COLUMN_INDEX in {2..9}; do
    generateStringsFile $COLUMN_INDEX
done

rm "output.csv"
rm "translatedString.csv"
rm "translated_key_value.csv"
rm "final_output.csv"
rm "new_file.txt"
rm "types.txt"