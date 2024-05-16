import json

def getDiff_for_art_queries(file1, file2):
    try:
        with open(file1, 'r') as file:
            file1_data = json.load(file)
        with open(file2, 'r') as file:
            file2_data = json.load(file)
        queryTypeCount1 = {}
        queryTypeCount2 = {}
        for key, value in file1_data.items():
            queryTypeCount1[key] = {}
            for line in value:
                if "queryData" in line:
                    parsed_line = json.loads(line)
                    queryType = (parsed_line["queryData"]["queryType"])
                    schemaName = (parsed_line["queryData"]["schemaName"])
                    table = (parsed_line["queryData"]["table"])
                    if schemaName in queryTypeCount1[key]:
                        if table in queryTypeCount1[key][schemaName]:
                            if queryType in queryTypeCount1[key][schemaName][table]:
                                queryTypeCount1[key][schemaName][table][queryType] += 1
                            else:
                                queryTypeCount1[key][schemaName][table][queryType] = 1
                        else:
                            queryTypeCount1[key][schemaName][table] = {}
                            queryTypeCount1[key][schemaName][table][queryType] = 1
                    else:
                        queryTypeCount1[key][schemaName] = {}
                        queryTypeCount1[key][schemaName][table] = {}
                        queryTypeCount1[key][schemaName][table][queryType] = 1
        for key, value in file2_data.items():
            queryTypeCount2[key] = {}
            for line in value:
                if "queryData" in line:
                    parsed_line = json.loads(line)
                    queryType = (parsed_line["queryData"]["queryType"])
                    schemaName = (parsed_line["queryData"]["schemaName"])
                    table = (parsed_line["queryData"]["table"])
                    if schemaName in queryTypeCount2[key]:
                        if table in queryTypeCount2[key][schemaName]:
                            if queryType in queryTypeCount2[key][schemaName][table]:
                                queryTypeCount2[key][schemaName][table][queryType] += 1
                            else:
                                queryTypeCount2[key][schemaName][table][queryType] = 1
                        else:
                            queryTypeCount2[key][schemaName][table] = {}
                            queryTypeCount2[key][schemaName][table][queryType] = 1
                    else:
                        queryTypeCount2[key][schemaName] = {}
                        queryTypeCount2[key][schemaName][table] = {}
                        queryTypeCount2[key][schemaName][table][queryType] = 1

        print("Processed Query Data for Diff Check")
        return queryTypeCount1, queryTypeCount2
    except Exception as e:
        print(f"Error in getting query diff between art and art_runner data")
        print(f"Error -> {e}")
        return {}, {}





