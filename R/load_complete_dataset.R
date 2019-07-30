
#' Read the complete data set from the (compressed) CSV
#'
#' @return Returns a data from containing the entire data set,
#' with invalid rows (mobile users and non-consents) filtered out
#' @export
#' @import dplyr
#' @importFrom utils unzip
#' @importFrom tools file_path_sans_ext
#'
load_complete_dataset <- function(){

  zip_file_path <- system.file("extdata/qualtrics_31-5-2019_20.40.csv.zip",
                                 package = "MoreyHoekstra2019")
  csv_dir_path <- tempdir()
  csv_file_name <- tools::file_path_sans_ext(basename(zip_file_path))
  csv_file_path <- file.path(csv_dir_path, csv_file_name)

  md5_file_path <- system.file("md5/md5sum.txt",
                               package = "MoreyHoekstra2019")
  colnames_file_path <- system.file("extdata/colnames.csv",
                               package = "MoreyHoekstra2019")

  md5s <- read.table(md5_file_path, header = FALSE,
                     row.names = 1, stringsAsFactors = FALSE)

  # check md5 of zipped file
  if( md5s["Original_zip",1] != tools::md5sum( zip_file_path ) )
    stop( "Something is wrong; md5 of zip file does not check out! Aborting." )

  utils::unzip(zip_file_path, exdir = csv_dir_path)

  # check md5 of unzipped file
  if( md5s["Original",1] != tools::md5sum( csv_file_path ) )
    stop( "Something is wrong; md5 of csv file does not check out! Aborting." )

  # Read in data twice to get header; qualtrics is weird
  dat <- read.csv(csv_file_path, skip = 1, header = TRUE,
                  stringsAsFactors = FALSE)
  dat <- dat[-1,]
  header <- read.csv(csv_file_path,skip = 1, header = TRUE)
  colnames(dat) <- colnames(header)

  # Clean columns/initial filter

  # Rename columns for convenience and
  # delete unnecessary columns
  cnames <- read.csv(file = colnames_file_path,
                     header = TRUE, stringsAsFactors = FALSE)
  cnames0 <- cnames$names[cnames$keep]
  names(cnames0) <- cnames$new_names[cnames$keep]

  dat %>%
    select( !!!cnames0 ) %>%
    filter(mobile != "true") %>%
    filter(consent == "I understand the above and consent to participate in this study.") -> dat

}



