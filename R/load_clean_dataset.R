
#' Load the data set and apply all cleaning steps
#'
#'
#' @return Returns a list of four objects:
#' \itemize{
#' \item clean_data: data frame with cleaned data, per participant
#' \item samples: Each click for each participant in the experimental task
#' \item eliminations: number of participants that fail to meet each criterion
#' \item original rows: the number of rows in the data set before cleaning
#' }
#' @export
#' @import dplyr
#' @importFrom purrr map_lgl pmap_lgl
#' @importFrom magrittr %<>%
#'
load_clean_dataset <- function(){

  data_list <- compile_complete_dataset()

  dat <- data_list$compiled_data
  samples <- data_list$samples

  original_rows <- nrow(dat)

  # Perform main data filtering ---------------------------------------------------

  # Perform data filtering by elimination criteria
  dat %<>%
    mutate(
      exclusion_firsttime = times != "No, this is my FIRST TIME doing this survey.",
      exclusion_engaged = (n_null == 0 | n_expt == 0),
      exclusion_isscience = is_science == "No",
      exclusion_education = education == "No university"
      )

  # Compute number of eliminations
  dat %>%
    select( matches("exclusion_") ) %>%
    mutate_all( function(z){
      z[is.na(z)] = TRUE
      z }
      ) %>%
    mutate(exclusion_any = pmap_lgl(., any )) -> excl_tbl

  eliminations <- colSums( excl_tbl, na.rm = TRUE )

  dat %<>%
    filter( !excl_tbl$exclusion_any ) %>%
    select( -matches("exclusion_") ) %>%
    merge(MoreyHoekstra2019::text_coding, by = "id")

  # Eliminate participants in record of sampling behaviour
  samples %<>% filter(id %in% dat$id )

  list( clean_data = dat,
        samples = samples,
        eliminations = eliminations,
        original_rows = original_rows )
}
