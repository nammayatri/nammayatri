import os
import requests
import zipfile
import json
import sys
import xml.etree.ElementTree as ET


########################################## To update MainActivity.java ##########################################

# Define the path to the MainActivity.java file
java_file_path = "app/src/main/java/in/juspay/mobility/MainActivity.java"

# Check if the file exists
if os.path.exists(java_file_path):
    # Read the content of the file
    with open(java_file_path, "r", encoding="utf-8") as f:
        lines = f.readlines()

    # Modify the content to set setWebContentsDebuggingEnabled to false
    modified_lines = []
    for line in lines:
        if "WebView.setWebContentsDebuggingEnabled" in line:
            modified_lines.append("WebView.setWebContentsDebuggingEnabled(false);\n")
        else:
            modified_lines.append(line)

    # Write the modified content back to the file
    with open(java_file_path, "w", encoding="utf-8") as f:
        f.writelines(modified_lines)
    
    print(f"Modified {java_file_path}")
else:
    print(f"File {java_file_path} not found.")
    sys.exit(1)


########################################## To update Bools.XML ##########################################
# Define the paths to the bools.xml files
# xml_file_paths = ["app/src/main/res/values/bools.xml", "app/src/dev/res/values/bools.xml"]

# # Iterate over each XML file
# for xml_file_path in xml_file_paths:
#     # Check if the file exists
#     if os.path.exists(xml_file_path):
#         # Parse the XML file
#         tree = ET.parse(xml_file_path)
#         root = tree.getroot()

#         # Find the <bool> element with the name "local_assets"
#         # for bool_element in root.findall("./bool[@name='local_assets']"):
#         #     # Set the text of the <bool> element to "false"
#         #     bool_element.text = "false"
#         for bool_element in root.findall("./bool"):
#             bool_name = bool_element.get("name")
#             if bool_name in ["local_assets", "use_local_assets"]:
#                 # Set the text of the <bool> element to "false"
#                 bool_element.text = "false"

#         # Write the modified XML back to the file
#         tree.write(xml_file_path, encoding="utf-8", xml_declaration=True)
#         print(f"Modified {xml_file_path}")

#         with open(xml_file_path, "r", encoding="utf-8") as f:
#             modified_content = f.read()
#             print(f"Modified content of {xml_file_path}:")
#             print(modified_content)
#     else:
#         print(f"File {xml_file_path} not found.")
#         sys.exit(1)



########################################## To fetch the config.json and config.zip ##########################################
# Get URL from command-line argument
param = sys.argv[1]

# Define the URL based on the parameter
if param == "NY":
    url = "https://assets.juspay.in/hyper/bundles/in.juspay.merchants/nammayatriconsumer/android/cug/config.json"
    appName = "in.yatri.consumer"
    print(f"URL NY :::::: " + url)
elif param == "MY":
    url = "https://assets.juspay.in/hyper/bundles/in.juspay.merchants/nammayatriconsumer/android/cug/config.json"
    appName = "in.yatri.consumer"
    print(f"URL MY :::::: " + url)
elif param == "Y":
    # Default URL if no match is found
    url = "https://assets.juspay.in/hyper/bundles/in.juspay.merchants/yatriconsumer/android/cug/config.json"
    appName = "in.yatri.consumer"
    print(f"URL Y :::::: " + url)
elif param == "NYP":
    # Default URL if no match is found
    url = "https://assets.juspay.in/hyper/bundles/in.juspay.merchants/nammayatriprovider/android/cug/config.json"
    appName = "in.yatri.provider"
    print(f"URL NYP :::::: " + url)
elif param == "MYP":
    # Default URL if no match is found
    url = "https://assets.juspay.in/hyper/bundles/in.juspay.merchants/nammayatriprovider/android/cug/config.json"
    appName = "in.yatri.provider"
    print(f"URL MYP :::::: " + url)
elif param == "YP":
    # Default URL if no match is found
    url = "https://assets.juspay.in/hyper/bundles/in.juspay.merchants/yatriprovider/android/cug/config.json"
    appName = "in.yatri.provider"
    print(f"URL YP :::::: " + url)
elif param == "OYP":
    # Default URL if no match is found
    url = "https://assets.juspay.in/hyper/bundles/in.juspay.merchants/nammayatriprovider/android/cug/config.json"
    appName = "in.yatri.provider"
    print(f"URL OYP :::::: " + url)


# Define the directory to save the config files
directory = 'configs'
os.makedirs(directory, exist_ok=True)

# Fetch the JSON data from the URL
response = requests.get(url)
if response.status_code == 200:
    data = response.json()

    # Save the JSON data as config.json
    config_json_path = os.path.join(directory, "config.json")
    with open(config_json_path, 'w') as f:
        json.dump(data, f, indent=4)
    print(f"Exported config.json")

# Fetch the JSON data from the URL
response = requests.get(url)
if response.status_code == 200:
    data = response.json()
    # Check if the specified app exists in the JSON data
    if appName in data['new']['assets']:
        # Get the URL for the configuration file
        config_url = data['new']['assets'][appName].get('configuration')
        
        # If the URL exists, download the configuration file
        if config_url:
            response = requests.get(config_url)
            if response.status_code == 200:
                # Save the downloaded ZIP file
                config_zip_path = os.path.join(directory, "config.zip")
                with open(config_zip_path, 'wb') as f:
                    f.write(response.content)
                    print(f"Downloaded config.zip")
                
                # Unzip the downloaded file and extract .jsa file
                with zipfile.ZipFile(config_zip_path, 'r') as zip_ref:
                    extracted_files = zip_ref.namelist()
                    for file in extracted_files:
                        if file.endswith('.jsa'):
                            zip_ref.extract(file, directory)
                            print(f"Extracted {file}")
                
                # Remove the downloaded ZIP file
                os.remove(config_zip_path)
                
            else:
                print("Failed to download configuration file")
                sys.exit(1)
        else:
            print("Configuration URL not found for appName")
            sys.exit(1)
    
    # Check if the specified package exists in the JSON data
    if appName in data['new']['package']:
        # Get the URL for the package file
        package_url = data['new']['package'].get(appName)
        
        # If the URL exists, download the package file
        if package_url:
            response = requests.get(package_url)
            if response.status_code == 200:
                # Save the downloaded ZIP file
                package_zip_path = os.path.join(directory, "index_bundle.zip")
                with open(package_zip_path, 'wb') as f:
                    f.write(response.content)
                    print(f"Downloaded index_bundle.zip")
                
                # Unzip the downloaded file and extract .jsa file
                with zipfile.ZipFile(package_zip_path, 'r') as zip_ref:
                    extracted_files = zip_ref.namelist()
                    for file in extracted_files:
                        if file.endswith('.jsa'):
                            zip_ref.extract(file, directory)
                            print(f"Extracted {file}")
                
                # Remove the downloaded ZIP file
                os.remove(package_zip_path)
                
            else:
                print("Failed to download package file")
                sys.exit(1)
        else:
            print("Package URL not found for appName ")
            sys.exit(1)
    else:
        print("appName not found in JSON data")
        sys.exit(1)
else:
    print("Failed to fetch JSON data from the URL Error: {response.text}")
    sys.exit(1)
