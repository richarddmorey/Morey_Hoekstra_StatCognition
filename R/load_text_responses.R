
#' Text responses  for each participant from the cleaned data
#'
#' @return A data frame containing the only text responses from the cleaned data set
#' @export
#' @import dplyr
#'
load_text_responses <- function(){

  data_list <- load_clean_dataset()

  dat <- data_list$clean_data
  samples <- data_list$samples

  colnames_file_path <- system.file("extdata/colnames.csv",
                                    package = "MoreyHoekstra2019")
  cnames <- read.csv(file = colnames_file_path,
                     header = TRUE, stringsAsFactors = FALSE)

  dat_names <- colnames(dat)

  has_text = ( dat_names %in% cnames$new_names[ cnames$free_text ] |
                 grepl("other", dat_names, fixed = TRUE) ) &
    !( dat_names %in% "sig_testing.prefer_other" )

  dat %>%
    select( c("id", dat_names[ has_text ] ) ) -> data_text_responses

  return(data_text_responses)

}
