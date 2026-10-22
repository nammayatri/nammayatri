import os
import sys

# Define the path to the build.gradle file
gradle_file_path = "app/build.gradle"
# Define the path to the version information file
version_info_file = "versions.txt"

# Function to read version information from the file
def read_version_info(file_path):
    version_info = {}
    with open(file_path, "r") as f:
        for line in f:
            app_type, version_code, version_name = line.strip().split(", ")
            version_info[app_type] = {"versionCode": int(version_code), "versionName": version_name}
    return version_info

# Read version information from the file
version_info = read_version_info(version_info_file)

app_type = sys.argv[1]

# Check if the file exists
if os.path.exists(gradle_file_path):
    # Read the content of the file
    with open(gradle_file_path, "r", encoding="utf-8") as f:
        lines = f.readlines()

    # Modify the content to update versionCode and versionName
    modified_lines = []
    for line in lines:
        # Remove leading and trailing spaces
        tempLine = line.strip()

        if tempLine.startswith("versionCode"):
            # Extract version information based on app type (customer or driver)
            app_type = (app_type == "customer") * "customer" or "driver"
            version_code = version_info[app_type]["versionCode"] + 1
            modified_lines.append(f"    versionCode {version_code}\n")  # Add back the spaces
        elif tempLine.startswith("versionName"):
            app_type = (app_type == "customer") * "customer" or "driver"
            version_name = version_info[app_type]["versionName"]
            major, minor, patch = version_name.split('.')
            patch = int(patch) + 1
            version_name = f"{major}.{minor}.{patch}"
            modified_lines.append(f'    versionName "{version_name}"\n')  # Add back the spaces
        else:
            modified_lines.append(line)  # Add back the newline character

    # Write the modified content back to the file
    with open(gradle_file_path, "w", encoding="utf-8") as f:
        f.writelines(modified_lines)
    
    print(f"Modified {gradle_file_path}")
else:
    print(f"File {gradle_file_path} not found.")