import json
import os
from pathlib import Path

def load_translations(json_file):
    with open(json_file, 'r', encoding='utf-8') as f:
        return json.load(f)

def update_language_files(lang_folder, translations):
    languages = ["EN", "TA", "TE", "ML", "BN", "HI", "KN", "OD"]
    
    for lang in languages:
        lang_file = Path(lang_folder) / f"{lang}.purs"
        if not lang_file.exists():
            print(f"Skipping {lang_file}, file not found.")
            continue

        with open(lang_file, 'r+', encoding='utf-8') as f:
            lines = f.readlines()
            existing_lines = {line.strip().split(':')[0] for line in lines if ':' in line}
            
            updated_lines = []
            for line in lines:
                stripped_line = line.strip()
                if ':' in stripped_line:
                    key = stripped_line.split(':')[0]
                    newKey = key.split(',')
                    finalKey = newKey[1] if len(newKey) > 1 else  key
                    stripped_key = finalKey.strip()
                    if stripped_key in translations and lang.lower() in translations[stripped_key]:
                        new_value = translations[stripped_key][lang.lower()].replace('\n', '\\n')
                        updated_lines.append(f'    {key}: "{new_value}"\n')
                        continue
                updated_lines.append(line)
            
            f.seek(0)
            f.writelines(updated_lines)
            f.truncate()
            
        print(f"Updated translations in {lang_file}")

def main():
    json_file = "translations.json"  # Update this path if needed
    lang_folder = "/Users/vignesh.s/Documents/nammayatri/Frontend/ui-customer/src/Resources/LocalizableV2"
    
    translations = load_translations(json_file)
    update_language_files(lang_folder, translations)
    
if __name__ == "__main__":
    main()
