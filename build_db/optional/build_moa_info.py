
## Author: Junmin Wang
## Date: 05/23/25
## Note: To retrieve the mechanisms of action, one needs to provide the correct path to the input and output files.
## "GSE92742_Broad_LINCS_pert_info.txt.gz" and "GSE70138_Broad_LINCS_pert_info_2017-03-06.txt.gz" can be downloaded from https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE92742 and https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE70138
## User key can be obtained from clue.io.

# Import libraries
import pandas as pd
import requests
import json

# Load data
df1 = pd.read_csv('/path/to/GSE92742_Broad_LINCS_pert_info.txt.gz', compression = 'gzip', delimiter = '\t')
df2 = pd.read_csv('/path/to/GSE70138_Broad_LINCS_pert_info_2017-03-06.txt.gz', compression = 'gzip', delimiter = '\t')

# Filter rows where the column 'pert_type' is 'trt_cp'
df1_filtered = df1[df1['pert_type'] == 'trt_cp'][['pert_id', 'pert_iname']]
df2_filtered = df2[df2['pert_type'] == 'trt_cp'][['pert_id', 'pert_iname']]

# Combine the two filtered tables
df_combined = pd.concat([df1_filtered, df2_filtered], ignore_index = True)

# Find distinct rows (drop duplicate rows)
df_distinct = df_combined.drop_duplicates()

# Keep rows with distinct values of 'pert_id'
df_final = df_distinct.drop_duplicates(subset = 'pert_id', keep = 'first')

# List of BRD IDs to query
brd_ids = list(df_final["pert_id"])

# Function to chunk the list
def chunk_list(lst, size):
    for i in range(0, len(lst), size):
        yield lst[i:(i + size)]


## API setup
# Define the base URL and endpoint
url = "https://api.clue.io/api/perts"

# User key
user_key = ""  # update user key here

# Set headers
headers = {"user_key": user_key}

# Preallocate results
all_results = []

# Loop through chunks
for i, chunk in enumerate(chunk_list(brd_ids, 100)):
    # Create the filter dictionary
    filter_param = {"fields": ["pert_id", "pert_iname", "moa"],
                   "where": {
                       "pert_id": {
                           "inq": chunk
                   }}}
    # Generate response
    response = requests.get(
        url, 
        headers = headers,
        params = {"filter": json.dumps(filter_param)}
    )
    # Store results
    if response.status_code == 200:
        all_results.extend(response.json())
    else:
        print(f"Error: {response.status_code} - {response.text}")


# Convert to dataframe
all_results_df = pd.DataFrame(all_results)

# Keep only needed columns
all_results_df_copy = all_results_df[["pert_iname", "moa"]].copy()

# Split by ', ' and explode into separate rows
all_results_df_copy = all_results_df_copy.explode('moa')

# Drop NaNs
all_results_df_copy = all_results_df_copy.dropna(subset = ['moa'])

# Drop duplicates
all_results_df_copy = all_results_df_copy.drop_duplicates()

# Group by pert_iname; combine unique moas into one string
all_results_df_grouped = all_results_df_copy.groupby("pert_iname")["moa"].apply(lambda x: ', '.join(x)).reset_index()

# Save the grouped dataframe to a tab-delimited file
all_results_df_grouped.to_csv("/path/to/output.txt", sep = "\t", index = False)

