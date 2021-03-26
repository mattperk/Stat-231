url <- "https://en.wikipedia.org/wiki/List_of_justices_of_the_Supreme_Court_of_the_United_States"
tables <- url %>% 
  read_html() %>% 
  html_nodes("table")
length(tables)
justices <- tables %>%
  purrr::pluck(2) %>%
  html_table(fill = TRUE)
justices
write_csv(justices, "justices.csv")

