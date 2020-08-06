# UNCTAD Nowcast data update
All files relating to the UNCTAD nowcast data update. This repo only hosts the code, for datafiles see the onedrive link below.

## Links:
- [Git repo](https://github.com/dhopp1-UNCTAD/nowcast_data_update)
- [OneDrive](https://unitednations-my.sharepoint.com/personal/daniel_hopp_un_org/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Fdaniel%5Fhopp%5Fun%5Forg%2FDocuments%2Fnowcasts%2Fnowcast%5Fdata%5Fupdate)

## Setup instructions
- Download the entire `nowcast_data_update` folder from OneDrive to your computer.
- Install all necessary R libraries listed in the `requirements.txt` file. On linux you can run `bash install_packages.sh` from the terminal. On windows install manually with `install.packages("library_name")` in R.

## Update instructions
- Update `helper/Eikon.xlsx` from the source (or Nour will send), and place in the `helper` directory, replacing the old file.
- Fill in latest available data for `x_servs_world` in `helper/historical.csv` with data from UNCTAD. This is emailed to us. If you're not sure what this means disregard.
- Run `Rscript update_data.r` from the project directory to get info until this month. Run `Rscript update_data.r [groups]` where `[groups]` e.g. is `24:26` to reget data just for groups 24-26. This will run:
	- `src/get_data.r`: this will update data sources via API and web scraping. The console will output which group is being gotten, and at the end will display which groups failed to update. Refer to the `catalog.csv` in the `helper` directory for which data sources are updated. Outputs `output/YYYY-mm-dd_database.csv` and `output/YYYY-mm-dd_log.csv`, which tells which data sources were succesfully updated and how long they took.
	- `src/transform_data.r`: seasonally adjusts data where relevant, and transforms cumulative numbers to non-cumulative where relevant. Outputs `output/YYYY-mm-dd_database_sa.csv` and `output/YYYY-mm-dd_database_tf.csv`.
- When the update is complete, upload the `output` directory to OneDrive, replacing all that is there. You may have to delete the whole folder then reupload.
- Data update process complete, disregard below unless you're working with it.

**Compare data (revisions, new data, etc.)**
- `src/compare_data.r`: can be used to compare two databases (doesn't run by default), run `compare_dfs(olddf, newdf)` to get a dataframe with same structure as input, except with:
                - `NA` for no data in old or new file
                - `2` for data in new, but not in old
                - `3` for data in old, but not in new
                - `0` if the old data is within a percentage (default 2%) of the new data
                - `1` if the old data is more than a percentage (default 2%) different from the new data (i.e. retroactive revision)

## Testing
Run tests by running `Rscript run_tests.r`
