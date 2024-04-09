#!/bin/bash

# Define the source and target repositories
TARGET_REPOS=("https://github.com/MercyQueen/asset-store")
branch_name=$1
BRANCH_NAME=$2
COMMIT_HASH=$3
token=$4

files_to_be_added=();


add_file_for_commit() { #dir , sub_dir, asset_type, asset_name, source_path
    local dir="$1"
    local sub_dir="$2"
    local asset_type="$3"
    local asset_name="$4"
    local updated_path="beckn/$dir/$sub_dir/$asset_type/$asset_name"
    files_to_be_added+=("../../$source_path:$updated_path")

    echo "Inside add_file_for_commit" $source_path "->" $updated_path
}

# Function to create a Pull Request
create_pull_request() {
    local target_repo="$TARGET_REPOS"
    local target_repo_name="$(basename "$target_repo")" || { echo "Error: Invalid target repository URL"; return 1; }

    if [ -z "$branch_name" ]; then
        echo "Error: Branch name not provided"
        return 1
    fi

    git checkout $BRANCH_NAME

    # Fetch the changes
    git fetch origin $BRANCH_NAME

    # Get the commit hash of the latest commit on the branch

    # Get the list of files changed in the latest commit
    CHANGED_FILES=$(git diff-tree --no-commit-id --name-only -r $COMMIT_HASH)

# Print the list of changed files
    echo "Changed files:"
    echo "$CHANGED_FILES"
    # Check if there are staged files
    local CHANGED_FILES
    # CHANGED_FILES="$(git diff --cached --name-only)" || { echo "Error: Failed to retrieve staged files"; return 1; }
    declare -a staged_files_array

    while IFS= read -r line; do
        staged_files_array+=("$line")
         echo "files are " $line
    done <<< "$CHANGED_FILES"
    echo $target_repo_name

    # Clone or update the target repository
    current_directory=$(pwd)
    echo "Current directory: $current_directory"
    echo "Target repository: $target_repo_name"
    # git clone "$target_repo" || { echo "Error: Failed to clone repository"; return 1; }
    if [ ! -d "$target_repo_name" ]; then
        git clone "$target_repo" "$target_repo_name" || { echo "Error: Failed to clone repository"; return 1; }
    else
        echo "Repository already exists in $target_repo_name"
    fi

    cd "$target_repo_name" || { echo "Error: Directory $target_repo_name does not exist after cloning"; return 1; }
    git checkout main
    git pull origin --rebase main || { echo "Error: Failed to pull latest changes"; return 1; }
    git branch -D "$branch_name" >/dev/null 2>&1 || true
    git checkout -b "$branch_name" || { echo "Error: Failed to create or checkout branch $branch_name"; return 1; }
    git pull origin --rebase main || { echo "Error: Failed to pull latest changes"; return 1; }


    # Process staged files and copy them to appropriate locations
    local allowed_extensions=("png" "jpg" "xml" "json")
    local filestobeadded=()

    for file in "${staged_files_array[@]}"; do
    extension="${file##*.}"

    if [[ " ${allowed_extensions[@]} " =~ " $extension " ]]; then
        source_path="$file"
        IFS="/" read -ra src_path_components <<< "$source_path"
        
        length=${#src_path_components[@]}
        dir=${src_path_components[5]}
        dir_array=()
        sub_dir=${src_path_components[4]}
        asset_type=${file_type}
        asset_name=${src_path_components[length-1]}

        # Determine file type based on path_components[3]
        if [[ ${src_path_components[length-2]} == "drawable" ]]; then
            file_type="images"
        else
            file_type="lottie"
        fi

        # Check if the source path contains any of the following keywords for directory
        if echo ${dir} | grep -q "jatriSaathi"; then
            dir_array=("jatriSaathi")
        elif echo ${dir} | grep -q "nammaYatri"; then
            dir_array=("nammaYatri")
        elif echo ${dir} | grep -q "yatri"; then
            dir_array=("yatri")
        elif echo ${dir} | grep -q "manayatri"; then
            dir_array=("manayatri")
        else 
            dir_array=("jatriSaathi" "nammaYatri" "yatri" "manayatri")
        fi

        # Iterate through dir_array and call add_file_for_commit
        for dir in "${dir_array[@]}"; do
            sub_directory="${sub_dir}"
            if [[ ${sub_directory} == "main" ]]; then 
                sub_directory="${dir}common"
            fi 
            add_file_for_commit "$dir" "$sub_directory" "$asset_type" "$asset_name" "$source_path"
        done
    fi
    done
    for item in "${files_to_be_added[@]}"; do
      source_path="${item%:*}"
      updated_path="${item#*:}"
      cp "$source_path" "$updated_path"
    done

    git add .
    git commit -m "Add new asset from asset store"
    git push origin "$branch_name" || { echo "Error: Failed to push changes to branch $branch_name"; return 1; }
    pull_request_url="${target_repo}/compare/main...${branch_name}"
    echo "Pull request URL: $pull_request_url"

    curl -X POST \
      -H "Accept: application/vnd.github.v3+json" \
      -H "Authorization: token $token" \
      https://github.com/MercyQueen/asset-store/dispatches \
      -d '{"event_type":"trigger_workflow", "client_payload": {"branch": "'$branch_name'"}}'

    cd ..
    rm -rf "$target_repo_name"
    

}


# Loop through target repositories and create pull requests
for target_repo in "${TARGET_REPOS[@]}"; do
    create_pull_request "$target_repo" "$target_repo"
done