# UNCTAD Nowcasts
All files relating to the UNCTAD nowcasts. This repo only hosts the code, for datafiles see the onedrive link below.

## Links:
- [Git repo](https://github.com/dhopp1-UNCTAD/nowcasts)
- [OneDrive](https://unitednations-my.sharepoint.com/personal/daniel_hopp_un_org/_layouts/15/onedrive.aspx?id=/personal/daniel_hopp_un_org/Documents/nowcasts)

## Setup instructions
- Clone this repo, then copy the `helper` and `output` directories from OneDrive to the root directory of the repo.
- Run `bash install_packages.sh` to install necessary libraries. Any failures resolve specific to your system.

## Update instructions
- Update `helper/Eikon.xlsx` from the source and place in the `helper` directory
- Run `Rscript update_nowcasts.r YYYY-mm-dd` from the project directory to get info until that end date. Run `Rscript update_nowcasts.r 2020-05-01 [groups]` where `[groups]` e.g. is `24:26` to reget data just for groups 24-26. This will run:
	- `src/get_data.r`: this will update data sources via API and web scraping. The console will output which group is being gotten, and at the end will display which groups failed to update. Refer to the `catalog.csv` in the `helper` directory for which data sources are updated. Outputs `output/YYYY-mm-dd_database.csv` and `output/YYYY-mm-dd_log.csv`, which tells which data sources were succesfully updated and how long they took.
	- `src/compare_data.r`: compares newly created database file to `output/most_recent_database.csv`. Outputs `output/YYYY-mm-dd_comparison.csv`, which is a file mirroring the database file except with:
		- `NA` for no data in old or new file
		- `2` for data in new, but not in old
		- `3` for data in old, but not in new
		- `0` if the old data is within a percentage (default 2%) of the new data
		- `1` if the old data is more than a percentage (default 2%) different from the new data (i.e. retroactive revision)
	- `src/transform_data.r`: seasonally adjusts data where relevant, and transforms cumulative numbers to non-cumulative where relevant. Outputs `output/YYYY-mm-dd_database_sa.csv` and `output/YYYY-mm-dd_database_tf.csv`.
	- `src/prepare_data.r`: prepares data for input to Octave. Outputs `output/YYYY-mm-dd_octave_value.csv`, `output/YYYY-mm-dd_octave_volume.csv`, `output/YYYY-mm-dd_octave_services.csv`. Which variables are included for which output are in the `octave_value`, `octave_volume`, and `octave_services` columns of `helper/catalog.csv`.
- When the update is complete, upload the whole local repo to OneDrive for data persistance purposes.

## Testing
Run tests by running `Rscript run_tests.r`
