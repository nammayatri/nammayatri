import os
import re
import json
from pathlib import Path

def generate_identifier(text):
    identifier = re.sub(r'[^a-zA-Z0-9 ]', '', text).upper().replace(' ', '_')
    snake_case = identifier.lower()
    return identifier, snake_case

def existing_keys_in_file(file_path):
    if not os.path.exists(file_path):
        return set()
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    return set(re.findall(r'\b([A-Z_]+)\b', content))

def process_view_file(view_file, existing_keys):
    with open(view_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    text_pattern = re.findall(r'text\s+"([^"]+)"', content)
    translations = {}
    
    for text in text_pattern:
        if text.strip():  # Skip empty strings and empty keys
            identifier, snake_case = generate_identifier(text)
            if identifier and snake_case:
                if identifier not in existing_keys:  # Ensure identifier is unique
                    translations[identifier] = text  # Use the actual text as translation
                content = content.replace(f'text "{text}"', f'text $ getString {identifier}')
    
    with open(view_file, 'w', encoding='utf-8') as f:
        f.write(content)
    
    return translations

def update_types_purs(types_file, translations):
    existing_keys = existing_keys_in_file(types_file)
    with open(types_file, 'a', encoding='utf-8') as f:
        for identifier in translations:
            if identifier not in existing_keys:
                f.write(f'    | {identifier}\n')

def update_types_v2_purs(types_v2_file, translations):
    existing_keys = existing_keys_in_file(types_v2_file)
    with open(types_v2_file, 'a', encoding='utf-8') as f:
        for identifier, _ in translations.items():
            if identifier not in existing_keys:
                f.write(f'{identifier.lower()} :: Proxy "{identifier.lower()}"\n')
                f.write(f'{identifier.lower()} = a\n\n')

def update_keymap_types_v2(types_v2_file, translations):
    existing_keys = existing_keys_in_file(types_v2_file)
    with open(types_v2_file, 'r+', encoding='utf-8') as f:
        lines = f.readlines()
        keymap_start = None
        for i, line in enumerate(lines):
            if line.strip().startswith("newtype Keymap = Keymap {"):  # Find keyMap definition
                keymap_start = i + 1
                break
        
        if keymap_start is not None:
            f.seek(0)
            new_lines = lines[:keymap_start] + [f'    {identifier.lower()}  :: String ,\n' for identifier in translations if identifier not in existing_keys] + lines[keymap_start:]
            f.seek(0)
            f.writelines(new_lines)

def update_language_files(lang_folder, translations):
    languages = ["EN", "TA", "TE", "ML", "BN", "HI", "KN", "OD"]
    for lang in languages:
        lang_file = Path(lang_folder) / f"{lang}.purs"
        existing_keys = existing_keys_in_file(lang_file)
        with open(lang_file, 'r+', encoding='utf-8') as f:
            lines = f.readlines()
            end_index = len(lines)
            for i, line in enumerate(lines):
                if line.strip().startswith('}'):  # Find end of Keymap
                    end_index = i
                    break
            
            f.seek(0)
            new_lines = lines[:end_index] + [f'    , {identifier.lower()}: "{translation}"\n' for identifier, translation in translations.items() if identifier not in existing_keys] + lines[end_index:]
            f.seek(0)
            f.writelines(new_lines)

def update_strings_v2(strings_v2_file, translations):
    existing_keys = existing_keys_in_file(strings_v2_file)
    with open(strings_v2_file, 'r+', encoding='utf-8') as f:
        lines = f.readlines()
        proxy_start = None
        for i, line in enumerate(lines):
            if line.strip().startswith("getProxy str = case str of"):
                proxy_start = i + 1
                break
        
        if proxy_start is not None:
            f.seek(0)
            new_lines = lines[:proxy_start] + [f'    {identifier} -> \\a -> a @~ {identifier.lower()}\n' for identifier in translations if identifier not in existing_keys] + lines[proxy_start:]
            f.seek(0)
            f.writelines(new_lines)

def main():
    folder = "/Users/vignesh.s/Documents/nammayatri/Frontend/ui-customer/src/Screens/RideBookingFlow/HomeScreen"
    types_file = "/Users/vignesh.s/Documents/nammayatri/Frontend/ui-customer/src/Resources/Localizable/Types.purs"
    types_v2_file = "/Users/vignesh.s/Documents/nammayatri/Frontend/ui-customer/src/Resources/LocalizableV2/Types.purs"
    lang_folder = "/Users/vignesh.s/Documents/nammayatri/Frontend/ui-customer/src/Resources/LocalizableV2"
    strings_v2_file = "/Users/vignesh.s/Documents/nammayatri/Frontend/ui-customer/src/Resources/LocalizableV2/Strings.purs"
    
    existing_keys = existing_keys_in_file(types_file) | existing_keys_in_file(types_v2_file) | existing_keys_in_file(strings_v2_file)
    for lang in ["EN", "TA", "TE", "ML", "BN", "HI", "KN", "OD"]:
        existing_keys |= existing_keys_in_file(Path(lang_folder) / f"{lang}.purs")
    
    for view_file in Path(folder).rglob("View.purs"):
        translations = process_view_file(view_file, existing_keys)
        if translations:
            update_types_purs(types_file, translations)
            update_types_v2_purs(types_v2_file, translations)
            update_keymap_types_v2(types_v2_file, translations)
            print(translations)
            update_language_files(lang_folder, translations)
            update_strings_v2(strings_v2_file, translations)
    
if __name__ == "__main__":
    main()
