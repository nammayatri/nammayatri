#!/bin/bash

branch_name=$1

files_to_be_added=();

black_listed_files=( "juspay_assets.json" )

valid_file() {
    fileName="$1"
    for file in ${black_listed_files[@]}; do
        if [[ "$fileName" == "$file" ]]; then
            echo "File is blacklisted"
            return 1 # 1 indicate failure in bash
        fi
    done

    return 0 # 0 indicates success in bash
}

add_file_for_commit() { #dir , sub_dir, asset_type, asset_name, source_path
    local dir="$1"
    local sub_dir="$2"
    local asset_type="$3"
    local asset_name="$4"
    local updated_path="beckn/$dir/$sub_dir/$asset_type/$asset_name"
    if valid_file "$asset_name"; then 
        files_to_be_added+=("../../../$source_path:$updated_path")
    fi
    echo "Inside add_file_for_commit" $source_path "->" $updated_path
}

# Function to create a Pull Request
create_pull_request() {
    local target_repo_name="asset-store"
    if [ -z "$branch_name" ]; then
        echo "Error: Branch name not provided"
        return 1
    fi

    git checkout $BRANCH_NAME

    # Fetch the changes
    git fetch origin $BRANCH_NAME
    declare -a staged_files_array
    for file in ${ALL_CHANGED_FILES[@]}; do
        staged_files_array+=("$file")
        echo "$file was changed"
    done

    echo $target_repo_name

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
            final_dir=""
            sub_dir=${src_path_components[4]}
            
            asset_name=${src_path_components[length-1]}

            # Determine file type based on path_components[3]
            if [[ ${src_path_components[length-2]} == "drawable" ]]; then
                file_type="images"
            else
                file_type="lottie"
            fi
            asset_type=${file_type}

            if [[ ${sub_dir} == "main" && ${dir} == *"Common"* ]]; then
                substring="Common"
                result="${dir//$substring/}" 
                lower_case_result=${result,,}
                lower_case_directory=${dir,,}
                add_file_for_commit "$lower_case_result" "$lower_case_directory" "$asset_type" "$asset_name" "$source_path"
                
            # Check if the source path contains any of the following keywords for directory
            else 
                if echo ${dir} | grep -q "jatriSaathi"; then
                    final_dir="jatrisaathi"
                elif echo ${dir} | grep -q "nammaYatri"; then
                    final_dir="nammayatri"
                elif echo ${dir} | grep -q "manayatri"; then
                    final_dir="manayatri"
                elif echo ${dir} | grep -q "yatri"; then
                    final_dir="yatri"
                
                else 
                    final_dir=""
                fi
                if [[ "${final_dir}" == "" ]]; then
                    if [[ ${sub_dir} == "main" ]]; then 
                        add_file_for_commit "common" "common" "$asset_type" "$asset_name" "$source_path"
                    elif [[ ${dir} == "common" ]]; then
                        add_file_for_commit "common" "$sub_dir" "$asset_type" "$asset_name" "$source_path" 
                    fi   
                else
                    add_file_for_commit "$final_dir" "$sub_dir" "$asset_type" "$asset_name" "$source_path"
                fi
            fi
        fi
    done 
    for item in "${files_to_be_added[@]}"; do
      source_path="${item%:*}"
      updated_path="${item#*:}"
      cp "$source_path" "$updated_path"
    done

    git add .
    git pull origin --rebase "$branch_name"
    git push -f origin "$branch_name"
    git commit -m "[GITHUB-ACTION]Added new asset from NammaYatri/NammaYatri branch : $branch_name"
    git push --set-upstream origin "$branch_name"
    git push 
    curl -X POST -H "Authorization: token $PAT_TOKEN" \
        https://api.github.com/repos/nammayatri/asset-store/dispatches \
        -d '{"event_type": "trigger_workflow",  "client_payload": {"branch": "'$branch_name'" , "ref" : "main"}}'

    cd ..
    rm -rf "$target_repo_name" 
    

}

create_pull_request 