import sys

def update_version(user_type):
    file_path = "versions.txt"
    updated_lines = []
    with open(file_path, 'r') as file:
        for line in file:
            parts = line.strip().split(', ')
            if parts[0] == user_type:
                version_code = int(parts[1]) + 1
                version_name_parts = parts[2].split('.')
                version_name_parts[-1] = str(int(version_name_parts[-1]) + 1)
                updated_version_name = '.'.join(version_name_parts)
                updated_lines.append(f"{user_type}, {version_code}, {updated_version_name}\n")
            else:
                updated_lines.append(line)
    
    with open(file_path, 'w') as file:
        file.writelines(updated_lines)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python script_name.py user_type")
        sys.exit(1)
    
    user_type = sys.argv[1]
    update_version(user_type)
    print(f'{user_type.capitalize()} version updated successfully!')
