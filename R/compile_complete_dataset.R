
#' Complete data set after extracting samples and making some columns "neater"
#'
#'
#' @return List containing two data frames:
#' compiled_data (the participant summaries) and samples (participants'
#' clicks in the experimental task)
#' @export
#' @import dplyr
#' @importFrom urltools url_decode
#' @importFrom rjson fromJSON
#' @importFrom purrr map map_int
#'
compile_complete_dataset <- function() {

  dat <- load_complete_dataset()
  # Convert columns to appropriate types ----------------------------------------------------------

  dat$effect_size = as.numeric(dat$effect_size)
  dat$evidence_power = as.integer(dat$evidence_power)
  dat$formal_training = as.numeric(dat$formal_training)
  dat$initial_timestamp <- as.numeric(dat$initial_timestamp)
  dat$finished_practice_timestamp <-
    as.numeric(dat$finished_practice_timestamp)

  # Recode primary response ----------------------------------------------------------

  dat %>% mutate(
    response = case_when(
      response == "" ~ NA_character_,
      response == "...I will tell him that I conclude that the JINGLIES are faster when making ${e://Field/toy_name}.\"" ~ "jinglies",
      response == "...I will tell him that I conclude that the SPARKLIES are faster when making ${e://Field/toy_name}.\"" ~ "sparklies",
      response == "...I will tell him that I CANNOT DETECT A DIFFERENCE between the two teams when making ${e://Field/toy_name}.\"" ~ "no_detect",
      response == "...I will tell him that I conclude that BOTH TEAMS ARE THE SAME SPEED when making ${e://Field/toy_name}.\"" ~ "same",
      response == "...I will tell him that I got bored and want to leave.\"" ~ "bored"
    ),
    true_winner = case_when(
      sign(effect_size) == 0 ~ "null",
      sign(effect_size) == 1 ~ "jinglies",
      sign(effect_size) == -1 ~ "sparklies"
    ),
    true_null = case_when(true_winner == "null" ~ TRUE,
                          TRUE ~ FALSE),
    response_null = case_when(
      response %in% c("same", "no_detect") ~ TRUE,
      response %in% c("jinglies", "sparklies") ~ FALSE,
      response == "bored" ~ NA
    ),
    response_alt = !response_null,
    initial_timestamp = case_when(initial_timestamp == -1 ~ NA_real_,
                                  TRUE ~ initial_timestamp),
    finished_practice_timestamp = case_when(
      finished_practice_timestamp == -1 ~ NA_real_,
      TRUE ~ finished_practice_timestamp - initial_timestamp
    ),
  ) -> dat

  dat$response = factor(dat$response)

  # Extract experimental samples ----------------------------------------------------------

  urltools::url_decode(dat$expt_list) %>%
    map(rjson::fromJSON) %>%
    map(~ bind_rows(.)) -> expt_list

  names(expt_list) <- dat$id

  expt_list %>% map_int(nrow) -> dat$n_expt
  expt_list[dat$n_exp == 0] <- NULL
  expt_list <- bind_rows(expt_list, .id = "id")

  expt_list$initial_timestamp <-
    dat$initial_timestamp[match(expt_list$id, dat$id)]
  expt_list$type <- "expt"

  # Extract null samples ----------------------------------------------------------

  urltools::url_decode(dat$null_list) %>%
    map(rjson::fromJSON) %>%
    map(~ bind_rows(.)) -> null_list

  names(null_list) <- dat$id

  null_list %>% map_int(nrow) -> dat$n_null
  null_list[dat$n_null == 0] <- NULL
  null_list <- bind_rows(null_list, .id = "id")

  null_list$initial_timestamp <-
    dat$initial_timestamp[match(null_list$id, dat$id)]
  null_list$type <- "null"

  # Combine null and experimental samples ----------------------------------------------------------

  rbind(null_list, expt_list) %>% mutate(
    time = as.numeric(time) - initial_timestamp,
    initial_timestamp = 0,
    ev = as.numeric(ev),
    sgn = as.integer(sgn),
    n_idx = as.integer(n_idx),
    n = as.integer(n),
    d = as.numeric(d),
    toy_name = as.character(toy_name),
    initiated_timestamp = ifelse(type == "null", time, time - 1000*ceiling(n / 10))
  ) -> samples

  # set initial_timestamp for privacy
  dat$initial_timestamp = 0

  # Recode confidence ----------------------------------------------------------

  dat$confidence = factor(
    dat$confidence,
    levels = c(
      "Not confident at all",
      "Somewhat doubtful" ,
      "Somewhat confident",
      "Very confident"
    ),
    ordered = TRUE
  )

  # Recode is_science ----------------------------------------------------------

  dat$is_science[dat$is_science == ""] <- NA
  dat$is_science = factor(dat$is_science)
  dat$is_science_other[dat$is_science_other == ""] <- NA

  # Recode shuffle understanding ----------------------------------------------------------

  dat$shuffle_other[dat$shuffle_other == ""] <- NA
  dat$shuffle[dat$shuffle == ""] <- NA
  dat$shuffle <- factor(
    dat$shuffle,
    levels = c(
      "Other",
      "Definitely not",
      "Probably not",
      "Probably yes",
      "Definitely yes"
    ),
    ordered = TRUE
  )

  # Recode education ----------------------------------------------------------

  dat$education[dat$education == ""] <- NA
  dat$education <- factor(
    dat$education,
    levels = c(
      "I have no formal scientific education at the University level",
      "I have some education at the Bachelor level (or equivalent)",
      "I have completed a Bachelor's degree (or equivalent)",
      "I have some education above the Bachelor level (e.g., Master's, PhD, or equivalent)",
      "I have completed a PhD (or equivalent)"
    ),
    ordered = TRUE
  )
  levels(dat$education) = c(
    "No university",
    "Some undergraduate",
    "Bachelor's degree",
    "Some postgraduate",
    "PhD"
  )

  # Recode statistical use ----------------------------------------------------------

  opts <- c(
    "I use statistics in practice for the analysis of data",
    "I develop statistical methods",
    "I comment on the philosophy of statistics",
    "I comment on statistical practice, but do not develop them myself",
    "Statistics do not play a role in my work",
    "Other"
  )

  how.use <- sapply(opts, function(el) {
    grepl(pattern = el, x = dat$how_use)
  })
  how.use <- as.data.frame(how.use)
  colnames(how.use) <-
    c("analysis",
      "develop",
      "philosophy",
      "method_comment",
      "none",
      "other")
  how.use$other[how.use$other] <- dat$how_use_other[how.use$other]
  how.use$other[how.use$other == FALSE] <- NA

  data.frame(dat, how_use = how.use) %>%
    select(-how_use_other) -> dat


  # Recode field ----------------------------------------------------------

  opts <- c(
    "Biological sciences",
    "Physical sciences",
    "Social/Behavioral sciences",
    "Computer science / technology",
    "Medical sciences",
    "Other"
  )

  field <- sapply(opts, function(el) {
    grepl(pattern = el, x = dat$field)
  })
  field <- as.data.frame(field)
  colnames(field) <-
    c("bio", "phys", "soc_beh", "comp_tech", "med", "other")
  field$other[field$other] <- dat$field_other[field$other]
  field$other[field$other == FALSE] <- NA

  data.frame(dat, field = field) %>%
    select(-field_other) -> dat

  # Recode significance testing opinion ----------------------------------------------------------

  opts <- c(
    "I think significance testing is necessary for science",
    "I think significance testing is fine, but prefer other approaches to statistical inference",
    "I think significance testing is fine, but people misunderstand/misuse it",
    "I do not have a strong opinion for or against significance testing",
    "I think significance testing should be discontinued or performed very rarely",
    "I think the logic of significance testing is fatally flawed",
    "I do not understand the question",
    "Other"
  )
  sig_testing <- sapply(opts, function(el) {
    grepl(pattern = el, x = dat$sig_testing)
  })
  sig_testing <- as.data.frame(sig_testing)
  colnames(sig_testing) <-
    c(
      "necessary",
      "prefer_other",
      "misunderstood",
      "no_opinion",
      "discontinued",
      "fatally_flawed",
      "do_not_understand",
      "other"
    )
  sig_testing$other[sig_testing$other] <-
    dat$sig_testing_other[sig_testing$other]
  sig_testing$other[sig_testing$other == FALSE] <- NA

  data.frame(dat, sig_testing = sig_testing) %>%
    select(-sig_testing_other) -> dat

  # Recode preferred method ----------------------------------------------------------

  dat$preferred_other[dat$preferred_other == ""] = NA
  dat$preferred[dat$preferred == ""] <- NA
  dat$preferred <- factor(dat$preferred)
  levels(dat$preferred) <-
    c("bayes", "classical", "no_stats", "descriptive", "other")


  # Make text responses missing instead of blank ----------------------------

  dat %>%
    mutate_at(c("salient_factors",
                "expt_strategy",
                "shuffle_desc"),
              funs(ifelse(. == "", NA, .))) %>%
    select(-null_list,-expt_list) -> dat

  list( compiled_data = dat,
       samples = samples )

}
