quotes_funny_html <- read_html("https://www.brainyquote.com/topics/funny-quotes")

quotes_funny <- quotes_funny_html %>%
  html_nodes(".oncl_q") %>%
  html_text() 

person_funny <- quotes_funny_html %>%
  html_nodes(".oncl_a") %>%
  html_text()

# put in data frame with two variables (person and quote)
quotes_funny_dat <- data.frame(quote = quotes_funny, stringsAsFactors = FALSE) %>%
  filter(quote != "\n") %>%
  mutate(person = person_funny
         , together = paste('"', as.character(quote), '" --'
                            , as.character(person), sep=""))

write_csv(quotes_funny_dat, "quotes.csv")
