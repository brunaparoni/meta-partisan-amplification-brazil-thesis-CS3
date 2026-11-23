"""
00_upload_inputs_SRE.py
-----------------------------------------------------------------------------
Stages the three external input files in the SRE (private environment).
Run cell by cell in a Python notebook; each `# %%` marks a cell.

Upload flow in the SRE (per file):
  1. user.prompt_uploads_file(<file name>)  -> the SRE e-mails a one-time
     `curl --upload-file ...` command;
  2. run that command ON YOUR OWN COMPUTER, in the folder holding the file;
  3. user.list_database_file_mappings()     -> shows the uploaded file as
     "<file name>_<timestamp>" under "Unmapped Files", and your private
     database name ("private_user_<id>");
  4. fill MAPPING_IDS and DATABASE below, then run stage().

Files (the two MPAS files come from scripts/part1_local/08_prepare_sre_inputs.R):
  url_rid_year_cluster.csv     -> ./rawdata/urlrids_2018_2022_clusters.csv
  clusters_mpas_clean_353.csv  -> ./rawdata/clusters_mpas_clean.csv
  url_mpas_matched_only_v3.csv -> ./rawdata/URLs_info_mathed_only.csv
                                  (file name kept as expected by script 05b)
"""

# %% Setup (run first in every new Python notebook)
import os
from fbri.common.user_data_manager import UserDataManager
from fbri.private.sql.query import execute

user = UserDataManager.get_instance()

for d in ["rawdata", "data", "chunks_BR", "results", "figures"]:
    os.makedirs(d, exist_ok=True)

# Fill in from user.list_database_file_mappings()
DATABASE = "private_user_<YOUR_USER_ID>"
MAPPING_IDS = {
    "url_rid_year_cluster.csv":     "url_rid_year_cluster.csv_<TIMESTAMP>",
    "clusters_mpas_clean_353.csv":  "clusters_mpas_clean_353.csv_<TIMESTAMP>",
    "url_mpas_matched_only_v3.csv": "url_mpas_matched_only_v3.csv_<TIMESTAMP>",
}

FILES = [
    # local file name,               SRE table name,              output path,                                   expected rows
    ("url_rid_year_cluster.csv",     "urlrids_br_clusters",       "./rawdata/urlrids_2018_2022_clusters.csv",    222_486),
    ("clusters_mpas_clean_353.csv",  "clusters_info_br_353",      "./rawdata/clusters_mpas_clean.csv",           353),
    ("url_mpas_matched_only_v3.csv", "urls_mpas_matched_only_v3", "./rawdata/URLs_info_mathed_only.csv",         110_851),
]


def stage(local_name, table, out_path, expected_rows):
    """Map an uploaded file to a table, read it back and save it as CSV."""
    user.create_table(table, MAPPING_IDS[local_name])
    result = execute(f"SELECT * FROM {DATABASE}.{table}")
    print(f"{table}: {result.shape} (expected {expected_rows:,} rows)")
    result.to_csv(out_path, index=False)
    return result


# %% Step 1 - request the upload links (one e-mail per file)
for local_name, *_ in FILES:
    user.prompt_uploads_file(local_name)

# %% Step 2 - after running each e-mailed curl command locally
user.list_database_file_mappings()

# %% Step 3 - after filling DATABASE and MAPPING_IDS above
for local_name, table, out_path, n in FILES:
    stage(local_name, table, out_path, n)
