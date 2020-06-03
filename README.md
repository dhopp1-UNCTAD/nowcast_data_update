# UNCTAD Nowcasts
All files relating to the UNCTAD nowcasts. This repo only hosts the code, for datafiles see the onedrive link below.

## Links:
- [Git repo](https://github.com/dhopp1-UNCTAD/nowcasts)
- [OneDrive](https://unitednations-my.sharepoint.com/personal/daniel_hopp_un_org/_layouts/15/onedrive.aspx?id=/personal/daniel_hopp_un_org/Documents/nowcasts)

## Update instructions
- Clone this repo, then copy the `helper` and `output` directories from OneDrive to the root directory of the repo. The `Eikon.xlsx` file in the `helper` directory of OneDrive should be updated from the source.
- Run `bash install_packages.sh` to install necessary libraries. Any failures resolve specific to your system. 
- Run `Rscript update_nowcasts.r YYYY-mm-dd` from the project directory to get info until that end date. Run `Rscript update_nowcasts.r 2020-05-01 [groups]` where `[groups]` e.g. is `24:26` to reget data just for groups 24-26. This will run:
	- `src/get_data.r`
	- `src/compare_data.r`
	- `src/transform_data.r`
	- `src/prepare_data.r`
- When the update is complete, upload the whole local repo to OneDrive for data backup reasons.
