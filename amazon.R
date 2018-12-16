library(tidyverse)
library(rvest)
library(RSelenium)
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L)

url <- paste0("https://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&",
              "field-keywords=electronics&rh=i%3Aaps%2Ck%3Aelectronics&ajr=0")

suppressMessages(remDr$open())

remDr$navigate(url)

remDr$screenshot(display = TRUE)

get_product <- function(src) {
  items <- src %>% read_html() %>% html_nodes(".aok-relative")
  # get title and text
  text <- items %>% html_nodes(".p13n-sc-truncated") %>% html_text()
  title <- items %>% html_nodes(".p13n-sc-truncated") %>% html_attr("title")
  title[is.na(title)] <- text[is.na(title)]
  # get prouct short name and asin
  links <- items %>% html_nodes(".a-text-normal") %>% html_attr("href")
  p <- "^/(.+)/dp/(.+)/.+$"
  product <- gsub(p, "\\1", links)
  asin <- gsub(p, "\\2", links)
  price <- items %>% html_nodes(".p13n-sc-price") %>% html_text()
  rank <- items %>% html_nodes(".zg-badge-text") %>% html_text()
  rank <- gsub("^#", "", rank)
  tibble(
    rank = rank,
    product = product,
    asin = asin,
    price = price,
    title = title
  )
}

remDr$findElement("css selector", ".a-text-normal")$clickElement()

src <- remDr$getPageSource() %>% "[["(1)

page1 <- get_product(src)

remDr$findElement("css selector", ".a-last a")$clickElement()

src <- remDr$getPageSource() %>% "[["(1)

page2 <- get_product(src)

data <- bind_rows(page1, page2)

write_csv(data, "amazon.csv")
