  #setwd("C:/Users/Anna V/Desktop/R_Stuff/powerful_women")
  library(rvest)
  library(tools)
  
  fortune <- "http://fortune.com/most-powerful-women/"
  
  ### PART 1: Find all urls for all 51 webpages, extract name and rank from url
  
  ## define function
  get_url <- function(url, node, attr){
    # A function to get the hyperlinks to the profile pages
    # Args:
    #   url: a url that has the urls to the profile pages
    #   node: the node where to look for the hyperlink to the profile pages
    #   attr: which attribute to extract from the node
    # Returns:
    #   a vector that has all of the hyperlinks to the profile pages
    
    master <- data.frame()
    url_s <- read_html(url) %>% 
      html_nodes(node) %>% 
      xml_attr(attr)
  }
  
  # use the function to get the urls from the webpage
  links <- get_url(fortune, "a", "href")
  
  # put links into dataframe
  clean_links <- as.data.frame(links, stringsAsFactors = FALSE)
 
  # identify links to keep
  clean_links$bin <- regexpr(pattern = "/most-powerful-women/", text = clean_links$links)

  # keep only necessary links
  clean_links <- subset(x = clean_links, clean_links$bin == 1, drop = TRUE)  
  
  # keep only one of each necessary link
  clean_links <- unique(clean_links[ , c(1:2) ] )
  clean_links <- clean_links[1] 
  
  # check structure of dataframe
  str(clean_links)

  # make a complete url
  clean_links$links <- gsub("/most-powerful-women", "http://fortune.com/most-powerful-women", clean_links$links)

  # extract name
  clean_links$name <- clean_links$links
  clean_links$name <- gsub("http://fortune.com/most-powerful-women/", "", clean_links$name)
  clean_links$name <- gsub("/", "", clean_links$name)
  clean_links$name <- gsub("-", " ", clean_links$name)
  clean_links$name <- gsub("[[:digit:]]+", "", clean_links$name)
  clean_links$name <- toTitleCase(clean_links$name)
  
  #extract rank
  clean_links$rank <- substr(clean_links$links, nchar(clean_links$links)-2, nchar(clean_links$links))
  clean_links$rank <- gsub("/", "", clean_links$rank)
  clean_links$rank <- gsub("-", " ", clean_links$rank)
  clean_links$rank <- as.numeric(clean_links$rank)
  
  # re-order variables
  clean_links <- clean_links[,c(1,3,2)]

### PART 2: extract title, affiliation, and age from each webpage
  ### define function
  get_text <- function(url, node){
  # A function to get text from webpage
  # Args:
  #       url: a url to a webpage
  #       node: the node where to look for the text
  # Returns:
  #       a vector with desired text 
  
    url_s <- read_html(url) %>% 
      html_nodes(node) %>% 
      html_text()
  }
  
  urls <- clean_links$links
  titles <- c()
  for (i in urls){
    result <- get_text(i, ".franchise-person-title")
    print(i)
    titles <- rbind(titles, result)
  }
  rm(i)
  
  titles <- as.data.frame(titles, stringsAsFactors = FALSE)
  # trim whitespace
  titles$V1 <- trimws(titles$V1, which = c("both"))
  
  # clean text so that I can extract, title and affiliation separately
  titles$V1 <- gsub("Chairman, President, and CEO", "Chairman President and CEO", titles$V1)
  titles$V1 <- gsub("YouTube, Google, Alphabet", "YouTube Google Alphabet", titles$V1)
  titles$V1 <- gsub("EVP, U.S. Stores", "EVP U.S. Stores", titles$V1)
  titles$V1 <- gsub("President, Northern Division", "President Northern Division", titles$V1)
  titles$V1 <- gsub("President, Personal Investing", "President Personal Investing", titles$V1)
  titles$V1 <- gsub("EVP, Worldwide Chairman, Pharmaceuticals", "EVP Worldwide Chairman Pharmaceuticals", titles$V1)
  titles$V1 <- gsub("SVP Blockchain, Industry Platforms, Accounts, and Partnerships", "SVP Blockchain Industry Platforms Accounts and Partnerships", titles$V1)
  titles$V1 <- gsub("Group President, North America", "Group President North America", titles$V1)
  titles$V1 <- gsub("President, Americas and Asia Pacific Japan", "President Americas and Asia Pacific Japan", titles$V1)
  titles$V1 <- gsub("Google, Alphabet", "Google Alphabet", titles$V1)
  titles$V1 <- gsub("President and CEO, Defense, Space & Security, and EVP", "President and CEO Defense Space & Security and EVP", titles$V1)
  titles$V1 <- gsub("JPM Asset and Wealth Management, JPMorgan Chase", "JPM Asset and Wealth Management JPMorgan Chase", titles$V1)
  titles$V1 <- gsub("NBCUniversal Cable Entertainment and Cable Studio, Comcast", "NBCUniversal Cable Entertainment and Cable Studio Comcast", titles$V1)
  
  titles$V1 <- gsub("BofA Merrill Lynch Intl., Bank of America", "BofA Merrill Lynch Intl. Bank of America", titles$V1)
  
  # Extract title
  titles$title <- titles$V1
  titles$title <- sub('\\s*,.*','', titles$title)
  
  # Extract affiliation
  titles$affiliation <- titles$V1
  titles$affiliation <- sub(".*, *(.*?) *,.*", "\\1", titles$affiliation)
  
  # Extract age
  titles$age <- substr(titles$V1, nchar(titles$V1)-2, nchar(titles$V1))
  titles$age <- as.numeric(titles$age)
  
  # webpage for marry barra has class franchise-person-title (without the .)
  marry_barra <- c("Chairman and CEO, General Motors, 56", "Chairman and CEO", "General Motors", 56)
  
  # binding together and ordering properly
  titles <- rbind(marry_barra, titles)
  rm(marry_barra)
  
  titles <- titles[c(2, 1, 3:51),]
  
  # generate CEO variable
  titles$CEO <- as.integer(grepl("CEO", titles$title))
  # generate president variable
  titles$pres <- as.integer(grepl("President", titles$title))
  
  # generate chairman variables
  titles$chair <- as.integer(grepl("Chairman", titles$title))
  
### Part 3: Extract revenue, profit, and market value from tables in all 51 webpages
  ###  
  table_info <- function(url){
    # A function to scrape specific information from a table on a webpage
    # Args:
    #       url: a url to a webpage
    # Returns:
    #       a dataframe with revenue, profit, and market value from a webpage
    
    tb_info <- read_html(url) %>%
      html_nodes("table") %>%
      html_table()
  
  if(length(tb_info) != 0){
    comp_info <- tb_info[[3]]
      revenue <- comp_info[1,2]
      profit <- comp_info[2,2]
      market_value <- comp_info[3,2]
    list2 <- data.frame(revenue, profit, market_value, stringsAsFactors = FALSE)
    return(list2)    
    }    
  }
  
  # Loop to go through all 51 urls and extract revenue, profits, and market value
  table <- data.frame(profit = character(),
                      revenue = character(),
                      market_value = character())
  for (i in urls) {
    row <- table_info(i)
      print(i)
    table <- rbind(table, row)
  }
  rm(i, row)
  
  # check which rows are missing from table
  for (i in urls) {
    info2 <- read_html(i) %>%
      html_nodes("table") %>%
      html_table()
    
    if(length(info2) == 0){
      print(i)
    }
  }
  rm(i, info2)
  
  # add Marry Barra row back into table
  tb_marry_barra <- c("157311.0", "-3864.0", "49,522.2")
  table <- rbind(tb_marry_barra, table)
    rm(tb_marry_barra)
  table <- table[c(2,1,3:51),]
  
  # change variables to numeric
  table$profit <- as.numeric((table$profit))
  table$revenue <- as.numeric(table$revenue)
    table$market_value <- gsub(",", "", table$market_value)
  table$market_value <- as.numeric(table$market_value)
  
  #check structure of dataframe
  str(table)
  
  # note: profits, reveue, and market value are in millioins!
  
### Part 4: Bind dataframes into one dataset, organize, and write a .csv
  df <- cbind(clean_links, table, titles)  
  str(df)

  # re-ordering variables
  df <- df[ , c(1:6, 8:13)]
  df <- df[ , c(2:12, 1)]
  df <- df[ , c(1,2,8,6,7,3:5,9:12)]
  
  # rm(clean_links, titles, table)
  
  # write to csv
  write.csv(df, file = "fortune_most_powerful_women_2018.csv", row.names = FALSE)
  