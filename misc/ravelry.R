library(httr)
library(XML)
library(jsonlite)

if(!grepl("/misc", getwd())){setwd("./misc")}
# user_rav.txt contains API username and password 
credentials <- readLines("user_rav.txt")
names(credentials) <- c("user","access_key","secret_key")

OpenConnection <- function(credentials){
  # Args: login info for the Ravelry API
  # Returns oauth token
  # Open connection to Ravelry API and return token
  reqURL <- "https://www.ravelry.com/oauth/request_token"
  accessURL <- "https://www.ravelry.com/oauth/access_token"
  authURL <- "https://www.ravelry.com/oauth/authorize"
  
  ravelry.app <- oauth_app("ravelry", key=credentials["access_key"], 
                           secret=credentials["secret_key"])
  ravelry.urls <- oauth_endpoint(reqURL, authURL, accessURL)
  
  return(oauth1.0_token(ravelry.urls, ravelry.app))
}

# Quick test of API connection by getting connected user info
TestConnection <- function(ravelry.token) {
  # Arg: API token
  # Returns name of the user connected with this token
  test <- GET("https://api.ravelry.com/current_user.json", 
              config=config("token"=ravelry.token)) 
  print(content(test)$user$username)
}

ravelry.token <- OpenConnection(credentials)
TestConnection(ravelry.token)

## Build dataset from Ravelry API: pattern permalink, pattern category, pattern text description
# Get url to patterns of interest from API search
pat0 <- GET("https://api.ravelry.com/patterns/search.json?page_size=20&craft=knitting", config=config("token"=ravelry.token))
pat <- content(pat0)


color_fam<-GET("https://api.ravelry.com/color_families.json", config=config("token"=ravelry.token))
color_fam<-content(color_fam)
# colors<-sapply(color_fam$color_families, function(x) x$permalink)
colors<-unlist(map(color_fam$color_families, `[[`,2))

yarns0<-GET("https://api.ravelry.com/yarns/search.json?fiber-content=wool&page_size=20", config=config("token"=ravelry.token))
yarns_content<-content(yarns0)
# yarns_id<-sapply(yarns_content$yarns, function(x) x$id)
# yarns_links<-GET(paste0("https://api.ravelry.com/yarns.json", paste0(yarns_id, collapse="+")))
# yarns_links_content<-content(yarns_links)
yarns<-yarns_content$yarns




yarns_ratings<-sapply(yarns_content$yarns, function(x) x$rating_average)

permalinks <- sapply(pat$patterns, function(x) x$permalink)
permalinks_full <- sapply(permalinks, function(name) paste("http://www.ravelry.com/patterns/library/",name,sep="",collapse=""))
names(permalinks_full) <- permalinks

# Get top level pattern category and description text using web scraping 
pattern_info <- lapply(permalinks_full, htmlTreeParse, useInternalNodes = TRUE)

pattern_description_par <- lapply(pattern_info, getNodeSet, path="//p", fun=xmlValue)
pattern_description <- sapply(pattern_description_par, paste, collapse=" ")

pattern_cat <- lapply(pattern_info, getNodeSet, path="//div[@class='category']/a/span/text()", fun=xmlValue)
pattern_topcat <- simplify2array(sapply(pattern_cat, head, 1))

## Data: 3 columns with pattern permalink, text description, and toplevel category
data <- as.data.frame(cbind(permalinks, pattern_topcat, pattern_description),stringsAsFactors=F,row.names=F)
names(data) <- c("permalink", "category", "description")
data$category <- as.factor(data$category)

cat_freq <- table(data$category)
nbr_examples <- dim(data)[1]

# Remove from data the categories with too few examples 
data <- subset(data, subset=(cat_freq[category] > 2))
data$category <- factor(data$category)
