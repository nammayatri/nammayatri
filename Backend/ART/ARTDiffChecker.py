import json
import re
import os
from datetime import datetime, timedelta
import time
import QueryDiffChecker as query_diff_checker

class colors:
    RESET = '\033[0m'
    RED = '\033[91m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    BLUE = '\033[94m'
    PURPLE = '\033[95m'
    CYAN = '\033[96m'




def getFilePath(file_name):
    current_file_path = os.path.abspath(__file__)
    current_directory = os.path.dirname(current_file_path)
    new_file_path = os.path.join(current_directory, file_name)
    return new_file_path

art_data_file_path = getFilePath("groupedRequestIdsForDiffChecker.log")
art_runner_data_file_path = getFilePath("APIdataArtRunner.log")


def is_valid_time_or_uuid(input_str):
    # return False, "Neither time nor UUID"
    # Regular expression pattern for UUID
    uuid_pattern = r'^[0-9a-f]{8}-?[0-9a-f]{4}-?[1-5][0-9a-f]{3}-?[89ab][0-9a-f]{3}-?[0-9a-f]{12}$'
    # Check if input matches UUID pattern
    if re.match(uuid_pattern, input_str):
        return True, "UUID"

    # Check if input matches any of the time formats
    time_pattern =  r'^\d{4}-\d{2}-\d{2}(T\d{2}:\d{2}:\d{2}(\.\d+)?Z?)?$'
    return bool(re.match(time_pattern, input_str)), "Time"


def get_response_from_list(response_list):
    for response in response_list:
        try:
            if "rawPathInfo" and "response" and "requestMethod" in response:
                response_data = json.loads(response)
                return json.loads(response_data["response"])
        except Exception as e:
            print(f'{colors.RED}Error in getting response from list{colors.RESET}')
            print(f"Error -> {e}")
    return None

def get_nested_diff_if_any(art_data_response, art_runner_data_response):
    try:
        #check if both are dict or list and diff and get diff recursively
        if art_data_response == art_runner_data_response:
            return None
        diff = {}
        if isinstance(art_data_response, dict):
            for key, value in art_data_response.items():
                if key not in art_runner_data_response:
                    diff[key] = {"art_response": value, "art_runner_response": None}
                else:
                    if value != art_runner_data_response[key]:
                        diff_data = get_nested_diff_if_any(value, art_runner_data_response[key])
                        if diff_data != None and diff_data != {} and diff_data != []:
                            diff[key] = diff_data
        elif isinstance(art_data_response, list):
            diff = []
            for index, value in enumerate(art_data_response):
                if index >= len(art_runner_data_response):
                    diff.append({"art_response": value, "art_runner_response": None})
                else:
                    if value != art_runner_data_response[index]:
                        diff_data = get_nested_diff_if_any(value, art_runner_data_response[index])
                        if diff_data != None and diff_data != {} and diff_data != []:
                            diff.append(diff_data)
        else:
            if art_data_response != art_runner_data_response:
                check, type = is_valid_time_or_uuid(str(art_data_response))
                if not check:
                    diff = {"art_response": art_data_response, "art_runner_response": art_runner_data_response}
                else:
                    print(f"{colors.YELLOW}Skiiping time/uuid check as type of {art_data_response} and {art_runner_data_response} is {type}{colors.RESET}")
                    diff = None
        if diff == {} or diff == []:
            return None
        return diff

    except Exception as e:
        print(f"{colors.RED}Error in getting nested diff{colors.RESET}")
        print(f"Error -> {e}")
        return "Error in getting nested diff"

def get_diff_art(art_data_response, art_runner_data_response):
    try:
        if art_data_response == None or art_runner_data_response == None:
            return {"art_data": art_data_response, "art_runner_data": art_runner_data_response, "diff": None}
        if art_data_response == art_runner_data_response:
            return {"art_data": art_data_response, "art_runner_data": art_runner_data_response, "diff": None}
        diff = {"art_data": art_data_response, "art_runner_data": art_runner_data_response, "diff": {}}
        for key, value in art_data_response.items():
            if key not in art_runner_data_response:
                diff["diff"][key] = {"art_response": value, "art_runner_response": None, "diff": {key: "Key not found in art_runner data"}}
            else:
                if value != art_runner_data_response[key]:
                    print(f"{colors.PURPLE}Diff found for key {key}{colors.RESET}")
                    diff_data = get_nested_diff_if_any(value, art_runner_data_response[key])
                    if diff_data != None:
                        diff["diff"][key] = diff_data
        if diff["diff"] == {}:
            diff["diff"] = None
        return diff
    except Exception as e:
        print(f"{colors.RED}Error in getting diff between art and art_runner data{colors.RESET}")
        print(f"Error -> {e}")
        return {"art_data": art_data_response, "art_runner_data": art_runner_data_response, "diff": "Error in getting diff"}

#-----------------------------------------------------------diff_checker-----------------------------------------------------------

def check_diff_between_art_and_art_runner():
    with open(art_data_file_path, 'r') as art_data_file:
        print(f"Reading -> {art_data_file_path}")
        art_data = json.load(art_data_file)
    with open(art_runner_data_file_path, 'r') as art_runner_data_file:
        print(f"Reading -> {art_runner_data_file_path}")
        art_runner_data = json.load(art_runner_data_file)

    # now iterate over one and check diff in another
    art_data_diff_result = {}
    print("Checking diff between art and art_runner data")
    for key, value in art_data.items():
        try:
            if key not in art_runner_data:
                print(f"{colors.YELLOW}Key {key} not found in art_runner_data{colors.RESET}")
                art_data_diff_result[key] = {"art_data": get_response_from_list(value), "art_runner_data": None , "diff": "No records found","calls_made":{"art":len(value),"art_runner":0}}
            else:
                art_data_response = get_response_from_list(value)
                art_runner_data_response = get_response_from_list(art_runner_data[key])
                if art_data_response == None:
                    print(f"{colors.YELLOW}Key {key} not found in art_data{colors.RESET}")
                    art_data_diff_result[key] = {"art_data": None, "art_runner_data": art_runner_data[key], "diff": "No response found","calls_made":{"art":len(value),"art_runner":len(art_runner_data[key])}}
                elif art_runner_data_response == None:
                    print(f"{colors.YELLOW}Key {key} not found in art_runner_data{colors.RESET}")
                    art_data_diff_result[key] = {"art_data": value, "art_runner_data": None, "diff": "No response found","calls_made":{"art":len(value),"art_runner":len(art_runner_data[key])}}
                else:
                    diff = get_diff_art(art_data_response, art_runner_data_response)
                    art_data_diff_result[key] = diff
                    art_data_diff_result[key]["calls_made"] = {"art":len(value),"art_runner":len(art_runner_data[key])}
        except Exception as e:
            print(f"Error in processing key {key}")
            print(f"Error -> {e}")
            art_data_diff_result[key] = {"art_data": value, "art_runner_data": art_runner_data[key], "diff": "Error in processing key","calls_made":{"art":len(value),"art_runner":len(art_runner_data[key])}}
    return art_data_diff_result


def compare_art_calls(calls_made_art, calls_made_art_runner):
    if calls_made_art == calls_made_art_runner:
        return True
    return False

def print_diff_result(key, result):
    diff = result["diff"]
    calls_made = result["calls_made"]
    if diff == None:
        print("\n"+colors.CYAN +"_"*100+colors.RESET,"\n")
        print(f"{colors.RESET}Result for API -> {key}{colors.RESET}")
        print(f"{colors.GREEN}Test-1 -> Api Response : Result : Passed ✅ {colors.RESET}")
        print(f"{colors.GREEN}No diff found in Response {colors.RESET}")
        print(f"{colors.RESET}Result for calls made for API -> {key}{colors.RESET}")
        if compare_art_calls(calls_made["art"], calls_made["art_runner"]):
            print(f"{colors.GREEN}Test-2 -> Calls made : Result : Passed ✅ {colors.RESET}")
        else:
            print(f"{colors.RED}Test-2 -> Calls made : Result : Failed ❌ {colors.RESET}")
            print(f"{colors.RESET}🚨 Art calls made -> {calls_made['art']} and Art_runner calls made -> {calls_made['art_runner']}{colors.RESET}")
        print("\n"+colors.CYAN +"_"*100+colors.RESET,"\n")
    else:
        print("\n"+colors.CYAN +"_"*100+colors.RESET,"\n")
        print(f"{colors.RESET}Result for API -> {key}{colors.RESET}")
        print(f"{colors.RED}Test-1 -> Api Response : Result : Failed ❌ {colors.RESET}")
        print(f"{colors.RED}🚨 Diff found for key {key} -> {json.dumps(diff, indent=4)}")
        print(f"{colors.RESET}Result for calls made for API -> {key}{colors.RESET} ")
        if calls_made["art"] == calls_made["art_runner"]:
            print(f"{colors.GREEN}Test-2 -> Calls made : Result : Passed ✅ {colors.RESET}")
        else:
            print(f"{colors.RED}Test-2 -> Calls made : Result : Failed ❌ {colors.RESET}")
            print(f"{colors.RESET}🚨 Art calls made -> {calls_made['art']} and Art_runner calls made -> {calls_made['art_runner']}{colors.RESET}")
        print("\n"+colors.CYAN +"_"*100+colors.RESET,"\n")
        print(f"Diff -> {diff}")
        print("\n")

def write_to_file(file_path, data):
    with open(file_path, 'w') as file:
        json.dump(data, file, indent=4)

#-----------------------------------------------------------main-----------------------------------------------------------

def main():
    try:
        result = check_diff_between_art_and_art_runner()
        file_path = getFilePath("ArtLogs/"+time.strftime("%Y-%m-%d--%H:%M")+".json")
        print(f"\nWriting diff result to file -> {file_path}\n")
        write_to_file(file_path, result)
        query_file_path_art = getFilePath("ArtLogs/"+time.strftime("%Y-%m-%d--%H:%M")+"_query_diff_art.json")
        print(f'{colors.GREEN}=> Successfully written ART and ART Runner diff to file -> {file_path}\n{colors.RESET}')
        query_file_path_art_replayer = getFilePath("ArtLogs/"+time.strftime("%Y-%m-%d--%H:%M")+"_query_diff_art_replayer.json")
        query_diff_result_art, query_diff_result_art_replayer = query_diff_checker.getDiff_for_art_queries(art_data_file_path, art_runner_data_file_path)
        print(f"Writing art query calls to file -> {query_file_path_art}")
        write_to_file(query_file_path_art, query_diff_result_art)
        print(f'{colors.GREEN}=> Successfully written ART Query Calls to file -> {query_file_path_art}\n{colors.RESET}')
        print(f"Writing art_replayer query calls to file -> {query_file_path_art_replayer}")
        write_to_file(query_file_path_art_replayer, query_diff_result_art_replayer)
        print(f'{colors.GREEN}=> Successfully written ART Replayer Query Calls to file -> {query_file_path_art_replayer}\n{colors.RESET}')
        print("\nWriting done\n")
    except Exception as e:
        print(f"Error in main")
        print(f"Error -> {e}")
    print("\nPrinting first passed and failed test cases\n")
    for key, value in result.items():
        print_diff_result(key, value)
        time.sleep(1)
        # if input("Do you want to continue (y/n) : ") == "n":
        #     break
        # print("\n Sleep for 1 second")