while IFS=" " read -r package version; 
do 
  Rscript -e "devtools::install_version('"$package"', version='"$version"', repos='http://cran.rstudio.com')"; 
done < "requirements.txt"
