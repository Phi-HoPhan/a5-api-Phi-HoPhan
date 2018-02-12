install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("plyr")
install.packages("stringr")

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plyr)
library(stringr)

setwd("~/../Desktop/Info_201/a5-api-Phi-HoPhan/projectkeys.gitignore")
source('keys.R')

API_KEY <- google.key
propub.key <- propublica.key

get.google.api <- function(address){
  base.url <- "https://www.googleapis.com/civicinfo/v2/"
  path <- "representatives/"
  google.api.request <- paste0(base.url, path)
  query.param <- list(address=address, key=API_KEY)
  response <- GET(google.api.request, query = query.param)
  body <- content(response, "text")
  result <- fromJSON(body)
  officials <- flatten(result$officials)
  return(officials)
}
get.table <- function(){
  keyed.officials <- mutate(officials, key= c(row_number()-1))
  keyed.officials$key <- as.numeric(keyed.officials$key)
  
  offices <- flatten(result$offices)
  unlisted.offices <- unnest(offices, officialIndices)
  
  merged.office.officials <- full_join(keyed.officials, unlisted.offices, by=c("key" = "officialIndices"))
  merged.office.officials.info <- 
    select(merged.office.officials, name.y, name.x, party, emails, phones, photoUrl)
  merged.office.officials.info[is.na(merged.office.officials.info)] <- "-"
  merged.office.officials.info[merged.office.officials.info == "NULL"] <- "-"
  return(merged.office.officials.info)
}


## 3. transform the data into a well formatted table
##    I recommend you transform the data into markdown strings.  For instance,
##    to display a html link as a link in the markdown file, you may want to
##    embed it between "[](" and ")".
##    
##    You may want to consider improved table printing, look for details at the rmarkdown
##    page at
##    http://rmarkdown.rstudio.com/index.html
##    
get.first.table <- function(){
  base.url2 <- "https://api.propublica.org/congress/v1/"
  path2 <- "members/house/WA/current.json"
  propub.api.request <- paste0(base.url2, path2)
  response2 <- GET(propub.api.request, add_headers("X-API-Key" = propub.key))
  body2 <- content(response2, "text")
  result2 <- fromJSON(body2)
  representatives <- flatten(result2$results)
  
  party.spread <- count(representatives, party)
  colors <- c("blue", "red")
  barplot(party.spread$n, col = colors, names.arg = c("Democrats", "Republicans"), 
          density = 50, main = "Party Affiliation", xlab = "Number of representatives", 
          horiz = TRUE)
}

get.second.table <- function(){
  base.url2 <- "https://api.propublica.org/congress/v1/"
  path2 <- "members/house/WA/current.json"
  propub.api.request <- paste0(base.url2, path2)
  response2 <- GET(propub.api.request, add_headers("X-API-Key" = propub.key))
  body2 <- content(response2, "text")
  result2 <- fromJSON(body2)
  representatives <- flatten(result2$results)

  gender.spread <- count(representatives, gender)
  colors2 <- c("pink", "light blue")
  barplot(gender.spread$n, col = colors2, names.arg = c("Female", "Male"),
          density = 50, main = "Gender Spread", xlab = "Number of representatives", horiz = TRUE)
}

path3 <- "members/R000578.json"
propub.member.request <- paste0(base.url2, path3)
response3 <- GET(propub.member.request, add_headers("X-API-Key" = propub.key))
body3 <- content(response3, "text")
result3 <- fromJSON(body3)
member <- flatten(result3$results)

name <- paste(member$first_name, member$middle_name, member$last_name)
birthday <- as.Date(member$date_of_birth[1])
today <- as.Date(Sys.Date())
days.old <- today-birthday
years.old <- round(as.numeric(days.old)/365) - 1
party <- if(toString(member$current_party[1]) == "D"){
  "Democrat"
  } else{
    "Republican"
  }
twitter <- toString(member$twitter_account)

path4 <- "house/votes/recent.json"
propub.vote.request <- paste0(base.url2, path4)
response4 <- GET(propub.vote.request, add_headers("X-API-Key" = propub.key))
body4 <- content(response4, "text")
result4 <- fromJSON(body4)
votes <- flatten(result4$results$votes)

path5 <- "members/R000578/explanations/115/votes.json"
propub.member.vote.request <- paste0(base.url2, path5)
response5 <- GET(propub.member.vote.request, add_headers("X-API-Key" = propub.key))
body5 <- content(response5, "text")
result5 <- fromJSON(body5)