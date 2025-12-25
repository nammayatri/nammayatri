import json
import os


# config file for the generating .purs file for crowdin downloaded translations
config_file_path = 'crowdin_python_generator_configuration.json'  


# utility Function to read JSON files
def read_json_file(file_path):
    with open(file_path, 'r') as file:
        data = json.load(file)
        return data


# template code (init code) for purescript localization file
def purescript_code_template(module_name, function_name):
    return '''module Generated.Localization.{mod_name} where

import Language.Types (STR(..))
import Prelude ((<>))

{func_name} :: STR -> String
{func_name} stringKey =
    case stringKey of
'''.format(func_name=function_name, mod_name=module_name)



# function to generate .purs tranlstions in format key -> value (purs) rather than key : value (json)
def create_translations(variant):

    # read the config file
    config = read_json_file(config_file_path)

    # get the required file locations from config file
    source_file_path = config[variant + "_SOURCE_FILE"]
    translation_location = config[variant + "_TRANSLATION_PATH"]
    localization_location = config[variant + "_PURS_LOCALIZATION_LOCATION"]

    # print ( for debugging) some config values
    print("source file path : " + source_file_path)
    print("translation location path : " + translation_location)
    print("language config : ", config["LANGUAGE_TRANSLATION_LIST"])


    # looping through each language defined in config
    for each_language in config["LANGUAGE_TRANSLATION_LIST"]:

        loc_function = each_language["purs_localization_function"]
        loc_two_letter = each_language["two_letter_code"].upper()
        translated_file_location = translation_location + loc_two_letter.lower() + ".json"
        purs_translated_file_location = localization_location + loc_two_letter + ".purs"

        print(translated_file_location)

        # start creating purs code
        purs_code = purescript_code_template(loc_two_letter, loc_function)

        # if translation exists for a given language then only generate purescript code else do nothing
        if os.path.exists(translated_file_location):

            if not os.path.exists(localization_location):
                os.makedirs(localization_location)
                print(f'Created folder: {localization_location}')

            data = read_json_file(translated_file_location)

            for key, value in data.items():
                formatted_value = value.replace('$<', '" <> ').replace('>$', ' <> "')
                value = "        " + key + " -> " + "\"" + formatted_value + "\"" + "\n"
                purs_code += value
            
            purs_code += "        " + "_" + " -> " + "\"" + each_language["default_translation"] + "\"" + "\n"
            with open(purs_translated_file_location, 'w') as file:
              file.write(purs_code)
              print("translations added successfully for variant : " , variant, " : language - " , loc_two_letter)
  
        else:
            print("Nothing to add for translation " + loc_two_letter)
 



if __name__ == "__main__":
    create_translations("DRIVER")
    create_translations("CUSTOMER")