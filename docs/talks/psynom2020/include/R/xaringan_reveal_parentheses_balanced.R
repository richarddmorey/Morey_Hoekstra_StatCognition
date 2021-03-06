# Emi Tanaka (@statsgen) and Garrick Aden-Buie (@grrrck) and Evangeline Reynolds (@EvaMaeRey)
# have contributed to this code

local_code <- # for testing w/o knitting
"cars %>%             # the data  #REVEAL
  filter(speed > 4) %>%  # subset 
  ggplot() +              # pipe to ggplot 
    aes(x = speed) +
    aes(y = dist) +  
    # Describing what follows
    geom_point(alpha = .3) + #REVEAL
    aes(color = 
          paste(\"speed\", 
          speed > 14) 
        ) %+%
    cars ->  
    my_plot "

library(tidyverse)

# We want to take just text (scalar), parse it and return a useful dataframe
parse_code <- function(code) {
  
  # code <- paste(knitr:::knit_code$get("the_code"), collapse = "\n")
  # code <- local_code
  
  raw_code <- tibble(raw_code = str_split(code, "\n")[[1]]) %>% 
    mutate(line = 1:n())
  
  sf <- srcfile(code)
  try(parse(text = code, srcfile = sf))
  getParseData(sf) %>%
    rename(line = line1) %>% 
    mutate(num_open_par = str_count(token, "\\(|\\{|\\[")) %>% # Counting open parentheses
    mutate(num_closed_par = str_count(token, "\\)|\\}|\\]"))  %>% # Counting closed parentheses
    group_by(line) %>%
    summarise(num_open_par = sum(num_open_par),
              num_closed_par = sum(num_closed_par),
              full_line = paste0(text, collapse = ""),
              comment = str_trim(paste0(ifelse(token == "COMMENT", text, ""),
                                        collapse = " "))) %>%
    left_join(raw_code) %>% 
    mutate(code = ifelse(comment != "", str_remove(raw_code, comment), raw_code)) %>%
    mutate(user_reveal = str_detect(comment, "#REVEAL")) %>%
    mutate(comment = str_remove(comment, "#REVEAL")) %>%
    mutate(connector = str_extract(str_trim(code), "%>%$|\\+$|->$|%\\+%")) %>%
    mutate(connector = replace_na(connector, "")) %>%
    mutate(code = str_remove(stringi::stri_trim_right(code), "%>%$|\\+$|->$|%\\+%")) %>%
    mutate(balanced_par = (cumsum(num_open_par) - cumsum(num_closed_par)) == 0 &
             code != "")
  
}


chunk_as_text <- function(chunk_name){
  
  paste(knitr:::knit_code$get(chunk_name), collapse = "\n")
  
}


# example
# parse_code(code = local_code)


parse_chunk <- function(chunk_name){
  
  code <- chunk_as_text(chunk_name)
  
  parse_code(code)
  
}


reveal_parsed <- function(parsed, upto = 3, highlight = 1:3){
  
  parsed %>% 
    mutate(reveal = 1:n() <= upto) %>% 
    filter(reveal) %>% 
    mutate(connector = case_when(1:n() == n() ~ "",
                                 1:n() != n() ~ connector)) %>% 
    mutate(highlight = ifelse(1:n() %in% highlight, "#<<", ""
    )) %>% 
    mutate(out = paste0(code, "", connector, "  ", comment, highlight)) %>% 
    select(out) ->
    up_to_result
  up_to_result$out  
  
}

reveal_code <- function(code, upto = 3, highlight = 1:3) {
  
  parsed <- parse_code(code = code) 
  
  reveal_parsed(parsed = parsed, upto = upto, highlight = highlight)
  
}

# example
# reveal_code(code = local_code)
# reveal_code(code = local_code, 5, 3:5)


reveal_chunk <- function(chunk_name, upto = 3, highlight = 1:3){

  content <- chunk_as_text(chunk_name)
  parsed <- parse_code(code = content) 
  
  reveal_parsed(parsed = parsed, upto = upto, highlight = highlight)

}



calc_highlight <- function(breaks) {
  
  highlighting <- list()

for (i in 1:length(breaks)) {
  if (i == 1) {  
    highlighting[[i]] <- 1:breaks[i]
  } else {
    highlighting[[i]] <- (breaks[i - 1] + 1):breaks[i]
  }
}
  
  return(highlighting)

}

# example
# calc_highlight(c(1,5,7,10))




# partial knit chunks

partially_knit_chunks <- function(chunk_name, user_reveal_defined = FALSE, show_code = TRUE, title = "") {
  # Create slide for lines 1:N for each line N in the given chunk

  parsed <- parse_chunk(chunk_name)
  
  if (user_reveal_defined == TRUE) { breaks <- parsed$line[parsed$user_reveal]  } else {
        breaks <- parsed$line[parsed$balanced_par] }

  highlighting <- calc_highlight(breaks = breaks)
  
  if (show_code == TRUE) {
    partial_knit_steps <- glue::glue(
      "class: split-40",
      "count: false",
      ".column[.content[",
      "```{r plot_{{chunk_name}}_{{breaks}}, eval=FALSE, code=reveal_chunk('{{chunk_name}}', {{breaks}}, {{highlighting}})}",
      "```",
      "]]",
      ".column[.content[",
      "```{r output_{{chunk_name}}_{{breaks}}, echo=FALSE, code=reveal_chunk('{{chunk_name}}', {{breaks}}, {{highlighting}})}",
      "```",
      "]]",
      .open = "{{", .close = "}}", .sep = "\n"
    )
  } else {
    
    partial_knit_steps <- glue::glue(title,"```{r output_{{chunk_name}}_{{breaks}}, echo=FALSE, code=reveal_chunk('{{chunk_name}}', {{breaks}}, {{highlighting}})}",
                                     "```",
                                     .open = "{{", .close = "}}", .sep = "\n"
    )
    
  }
  
  glue::glue_collapse(x = partial_knit_steps, sep = "\n---\n")
}



apply_reveal <- function(...){
  paste(knitr::knit(text = partially_knit_chunks(...)), collapse = "\n")
}



# For making visual table of contents / gallery

save_chunk_plot <- function(chunk_name, filename = chunk_name, path = "figures/", type = ".png"){
  get_what_save_what <- chunk_name
  eval(parse(text = paste(knitr:::knit_code$get(get_what_save_what), collapse = "")))
  ggsave(paste0(path, filename, type), dpi = 300)  
}

make_html_picture_link <- function(path, link){
  cat(paste0('<a href="', link, '"><img src="', path, '"width="150" height="150" title=' ,
             path, ' alt=', path,'></a>'))
}




# flipbook mini


ggplot_local_code <- # for testing w/o knitting
"ggplot(cars) +      
  aes(x = speed) +
  aes(y = dist) +  
  # Describing what follows
  geom_point() + #REVEAL
  aes(color = 
    paste(\"speed\", 
    speed > 15))"



# build partial plot

build_partial_chunk_plot <- function(chunk_name, upto = 3, highlight = 1:3){

writeLines(text = reveal_chunk(code = code, 
                               upto = upto, 
                               highlight = highlight), 
           con  = "tmp.R")
source("tmp.R")

}



build_partial_code_plot <- function(code, upto = 8, highlight = 1:8){
  
  writeLines(text = reveal_code(code = code, 
                                upto = upto, 
                                highlight = highlight), 
             con  = "tmp.R")
  source("tmp.R")

}

# example 
# print(build_partial_code_plot(code = ggplot_local_code, upto = 8)[[1]])


prep_code_to_plot_as_text <- function(code, upto = 3, highlight = 1:3){
  
  tibble(code_as_text = 
           reveal_code(code = code, 
                       upto = upto, 
                       highlight = highlight)) %>% 
    mutate(n = 1:n()) %>% 
    mutate(highlight = ifelse(str_detect(code_as_text, "#<<"), "yes", "no")) %>%
    mutate(highlight = factor(highlight, levels = c("yes", "no"))) %>% 
    mutate(code_as_text = str_remove(code_as_text, "#<<"))

}

# example
# prep_code_to_plot_as_text(ggplot_local_code)


build_partial_code_text_plot <- function(code, 
                                         upto = 8, 
                                         highlight = 1:8, 
                                         highlight_color = "plum4", 
                                         font_size = 4) {
  
  prepped <- prep_code_to_plot_as_text(code = code, 
                                       upto = upto, 
                                       highlight = highlight)
  
  ggplot(data = prepped) +
    aes(x = 1) +
    aes(y = -n) +
    scale_y_continuous(limits = c(-25,5), expand = c(0,0)) +
    scale_x_continuous(limits = c(-5,85), expand = c(0,0)) +
    coord_fixed(ratio = 3.5) +
    aes(label = code_as_text) +
    # grey background
    geom_rect(ymin = 0, ymax = -max(highlight) - 1, alpha = .2,
              xmin = -2.5, xmax = 80, fill = "grey") +
    # highlight background
    geom_rect(aes(ymin = -n + .5, ymax = -n - .5, alpha = highlight),
              xmin = -1, xmax = 80, fill = highlight_color) +
    labs(fill = NULL) +
    scale_alpha_discrete(range = c(.6,0), breaks = c("no", "yes"), guide = FALSE) +
    geom_text(hjust = 0, family = "mono", size = font_size) +
    theme_void()
  
}

# example
# build_partial_code_text_plot(code = ggplot_local_code, 
#                              upto = 8, 
#                              highlight = 1:8, 
#                              highlight_color = "plum4", 
#                              font_size = 4)


# putting plots together
bind_and_title_code_output_pngs <- 
  function(code, upto = NULL, highlight = NULL, 
           highlight_color = "plum4", font_size = 4, title = "Flipbook Mini"){

  num_lines_of_code <- length(str_split(code, "\n")[[1]])
    
  if (is.null(upto)) {upto <- num_lines_of_code}

  if (is.null(highlight)) {highlight <- 1:num_lines_of_code}  
    
  text_plot <- build_partial_code_text_plot(code = code, 
                                            upto = upto, 
                                            highlight = highlight, 
                                            highlight_color = highlight_color,
                                            font_size = font_size)
  
  the_plot <- build_partial_code_plot(code = code, upto = upto, highlight = highlight)[[1]]
  # the_plot <- build_partial_code_plot(code = local_code, upto = 8, highlight = 1:8)[[1]]
  
  
  # paste components together
  a_title <- cowplot::ggdraw() +
    cowplot::draw_label(label = title, fontface = 'bold')
  
  side_by_side <- cowplot::plot_grid(text_plot, the_plot, rel_widths = c(1, 1))
  
  cowplot::plot_grid(a_title, side_by_side, rel_heights = c(0.1, 1), ncol = 1)

}



# example
# bind_and_title_code_output_pngs(code = local_code, upto = 3, highlight = 1:3) -> g ; g
# bind_and_title_code_output_pngs(code = local_code) -> g ; g


create_frames <- function(code, name = "mini",  flipbook_dir = paste0("temp_", name), 
                           user_reveal_defined = FALSE, title = "A flipbook mini", 
                           font_size = 2.5, highlight_color = "plum4"){
  
  # flipbook_dir <- paste0("mini_", chunk_name)
  if (!dir.exists(flipbook_dir)) {dir.create(flipbook_dir)}
  unlink(list.files(flipbook_dir))
  
  parsed <- parse_code(code)
  
  if (user_reveal_defined == TRUE) { breaks <- parsed$line[parsed$user_reveal]  } else {
    breaks <- parsed$line[parsed$balanced_par] }
  
  highlighting <- calc_highlight(breaks = breaks)
  
  
  for (i in 1:length(breaks)) {
    
    g <- bind_and_title_code_output_pngs(code = code, upto = breaks[i], 
                                         highlight = if (i == 1) {1:breaks[i]}else{(breaks[i - 1] + 1):breaks[i]})
    cowplot::save_plot(g, 
                       filename = paste0(flipbook_dir,"/",  name, "_", i, ".png"))
    
  }
  
}


# example
# create_frames(code = ggplot_local_code)


pngs_to_gif <- function(path, file_out){

  files <- list.files(path = path, pattern = paste(".png"))
  files_path <- paste0(path, "/", files)
  
  files_path %>% 
    file.info() %>% 
    rownames_to_column(var = "file") %>% 
    arrange(mtime) %>% # sort them by time modified
    pull(file) %>% 
    purrr::map(magick::image_read) %>% # reads each path file
    magick::image_join() %>% # joins image
    magick::image_animate(fps = 1) %>% # animates
    magick::image_write(path = file_out)
  
}

# example
# pngs_to_gif(path = "temp_mini", 
#             file_out = "temp_mini/mini.gif")

create_frames_original <- function(chunk_name, chunk_dir = paste0("temp_", chunk_name), 
                          user_reveal_defined = FALSE, title = "A flipbook mini", 
                          font_size = 2.5, highlight_color = "plum4"){
  
  # chunk_dir <- paste0("mini_", chunk_name)
  if (!dir.exists(chunk_dir)) {dir.create(chunk_dir)}
  unlink(list.files(chunk_dir))
  
  parsed <- parse_chunk(chunk_name)
  
  if (user_reveal_defined == TRUE) { breaks <- parsed$line[parsed$user_reveal]  } else {
    breaks <- parsed$line[parsed$balanced_par] }
  
  highlighting <- calc_highlight(breaks = breaks)
  
  
  for (i in 1:length(breaks)) {
    
    # build partial plot
    writeLines(text = reveal_chunk(chunk_name, upto = breaks[i], highlight = highlighting[[i]]), con  = "tmp.R")
    source("tmp.R")
    the_plot <- last_plot()
    
    # write code to plot
    ggplot(tibble(label = 
                    reveal_chunk(chunk_name, 
                                 breaks[i], 
                                 highlighting[[i]])) %>% 
             mutate(n = 1:n()) %>% 
             mutate(highlight = ifelse(str_detect(label, "#<<"), "yes", "no")) %>%
             mutate(highlight = factor(highlight, levels = c("yes", "no"))) %>% 
             mutate(label = str_remove(label, "#<<"))
    ) +
      aes(x = 1) +
      aes(y = -n) +
      scale_y_continuous(limits = c(-25,0), expand = c(0,0)) +
      scale_x_continuous(limits = c(-4,75), expand = c(0,0)) +
      coord_fixed(ratio = 3.5) +
      aes(label = label) +
      geom_rect(aes(ymin = -n + .5, ymax = -n - .5, alpha = highlight),
                xmin = 0, xmax = 70, fill = highlight_color) +
      labs(fill = NULL) +
      scale_alpha_discrete(range = c(.6,0), breaks = c("no", "yes"), guide = FALSE) +
      geom_text(hjust = 0, family = "mono", size = font_size) +
      theme_void() +
      theme(plot.background = element_rect(fill = "gainsboro")) +
      theme(panel.background = element_rect(fill = "gainsboro")) -> 
      text
    
    # paste components together
    a_title <- ggplot() + 
      cowplot::draw_label(label = title, fontface = 'bold', color = "gainsboro") +
      theme(panel.background  = element_rect(fill = "darkgreen"))
    side_by_side <- cowplot::plot_grid(text, the_plot, rel_widths = c(1, 1))
    cowplot::save_plot(cowplot::plot_grid(a_title, side_by_side, 
                                          rel_heights = c(0.1, 1), ncol = 1), 
                       filename = paste0(chunk_dir,"/",  chunk_name, "_", i, ".png"))
    
    
  }
  
}


make_flipbook_mini <- function(chunk_name, chunk_dir = paste0("temp_", chunk_name), file_out, ...) {
  
  # Create slide for lines 1:N for each line N in the given chunk
  create_frames(chunk_name, chunk_dir, ...)
  
  # break_points <- seq_along(knitr:::knit_code$get(chunk_name)) # original code, breaks for each line
  pngs_to_gif(chunk_dir, file_out)
  
}

# #  andysw90
# last_expression <- function(n_recall = 1){
#   savehistory() #By default saves to ".Rhistory"
#   recalled_history <- tail(readLines(".Rhistory"),n_recall + 1)
#   return(head(recalled_history,-1))
# }



# RStudio overrides `history()`, `loadhistory()` and `savehistory()`, 
# so they work somewhat differently than on the console version of R.
# This saves the current history to a temporary file then reads the last
# line of it (or more depending on the `n` argument)
#
# It does not handle multi-line commands, which would be more tricky to handle...
# previous_commands <- function(n = 1) {
#   tf <- tempfile()
#   on.exit(unlink(tf))
#   savehistory(tf)
#   head(tail(readLines(tf), n = n + 1), n = n)
# }
# 
# # example 
# 4 + 5
# "hi"
# previous_commands(n = 2)
# 
# 
# previous_ggplot_code <- function(n = 20){
#   
#   previous_code <- previous_commands(n = n) 
#   
#   previous_code %>% 
#     str_detect("ggplot\\(") %>% 
#     which() %>% 
#     max() -> 
#     first_line_number
#   
#   previous_code[first_line_number:n] %>% 
#     paste0(collapse = "\n  ")
#   
#   
# }


# # example
# ggplot(data = gapminder::gapminder) +      
#   aes(x = gdpPercap) +
#   aes(y = lifeExp) +  
#   # Describing what follows
#   geom_point() + #REVEAL
#   aes(color = 
#         paste("continent", 
#               continent))
# previous_ggplot_code()



# parse_code(code = local_code)
# reveal(chu)
# knitr:::knit_code$get("the_code")
# parse_code(knitr:::knit_code$get("the_code")) %>% knitr::kable()

# parse_code(str_split(local_code, "\n")[[1]])

# proposed new reveal