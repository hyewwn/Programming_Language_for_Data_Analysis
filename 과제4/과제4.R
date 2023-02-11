#Web scraping
install.packages("dplyr")
install.packages("stringr")
install.packages("httr")
install.packages("rvest")

library(dplyr)
library(stringr)
library(httr)
library(rvest)

url <- 'https://www.goodreads.com/search?utf8=%E2%9C%93&query=harry+potter'

start <- proc.time()
#Variable
title <- NULL
author <- NULL
rating <- NULL
rating_NUM <- NULL
review_NUM <- NULL
summary <- NULL

#find last page number
page_NUM <- read_html(url) %>% html_nodes('div.leftContainer > div > div > a[href^="/search?page="]') %>% html_text
page_NUM <- page_NUM[length(page_NUM)-1] %>% as.integer()

for(i in 1:page_NUM) {
  tmp_url <- paste("https://www.goodreads.com/search?page=", i, "&q=harry+potter&qid=POWOMrNsaG&search_type=books&tab=books&utf8=%E2%9C%93", sep="")
  tmp_url <- gsub('%2B','+',tmp_url)
  book_url <- read_html(tmp_url) %>% html_nodes('a.bookTitle') %>% html_attr('href')
  book_url <- paste0("https://www.goodreads.com",book_url)
  for(j in 1:length(book_url)) {
    tryCatch({
      tmp_book_url <- book_url[j]
      tmp_paragraph <- read_html(tmp_book_url)
    
      #title
      tmp_title <- tmp_paragraph %>% html_nodes('h1#bookTitle') %>% html_text
      tmp_title <- gsub('\\s+',' ',tmp_title) %>% str_trim
      title <- c(title, tmp_title)
    
      #author
      tmp_author <- tmp_paragraph %>% html_nodes('a.authorName > span') %>% html_text %>%
        str_trim
      tmp_author <- toString(tmp_author)
      author <- c(author, tmp_author)
      
      #rating
      tmp_rating <- tmp_paragraph %>% html_nodes('div#bookMeta > span')
      tmp_rating <- tmp_rating[2] %>% html_text
      tmp_rating <- gsub('\\s+',' ',tmp_rating) %>% str_trim %>% as.numeric()
      rating <- c(rating, tmp_rating)
      
      #Number of rating
      tmp_rating_NUM <- tmp_paragraph %>% html_nodes('a.gr-hyperlink > meta[itemprop^="ratingCount"]') %>%
        html_attr('content') %>% as.integer()
      rating_NUM <- c(rating_NUM, tmp_rating_NUM)
      
      #Number of review
      tmp_review_NUM <- tmp_paragraph %>% html_nodes('a.gr-hyperlink > meta[itemprop^="reviewCount"]') %>%
        html_attr('content') %>% as.integer()
      review_NUM <- c(review_NUM, tmp_review_NUM)
      
      #summary
      tmp_summary <- tmp_paragraph %>% html_nodes('div#description > span[id^="freeTextContainer"]')
      if(length(tmp_summary)==0){
        tmp_summary <- c(" ")
      }else{
        tmp_summary <- tmp_summary %>% html_text
        tmp_summary <- gsub('\\s+',' ', tmp_summary) %>% str_trim
      }
      summary <- c(summary, tmp_summary)
      
      #check rank
      cat("rank", j, "\n")
    }, error = function(e){print("An error occurs. Skip")})
  }
  #check page
  Sys.sleep(1)
  cat(i, "/", page_NUM, "\n")
}

#make CSV
books <- data.frame(title, author, rating, rating_NUM, review_NUM, summary)
end <- proc.time()
end - start

write.csv(books, file = "books.csv")

#-----------------------------------------------------------------------------------------------


#input keyword
input_keyword <- c("deep learning", "machine learning", "natural language processing")
search_keyword <- gsub(" ", "+", input_keyword)

#variable
keyword <- NULL
title <- NULL
author <- NULL
rating <- NULL
rating_NUM <- NULL
review_NUM <- NULL
summary <- NULL

start <- proc.time()
for(k in 1:length(search_keyword)) {
  #acess to url(keyword)
  url <- modify_url(url, query = list(query=search_keyword[[k]]))
  url <- gsub('%2B','+',url)
  
  #find the number of pages
  page_NUM <- read_html(url) %>% html_nodes('div.leftContainer > div > div > a[href^="/search?page="]') %>% html_text
  page_NUM <- page_NUM[length(page_NUM)-1] %>% as.integer()
  
  #paste code above
  for(i in 1:page_NUM) {
    tmp_url <- paste("https://www.goodreads.com/search?page=",i,"&q=",search_keyword[[k]],"&qid=8sIcE9JULu&search_type=books&tab=books&utf8=%E2%9C%93", sep="")
    tmp_url <- gsub('%2B','+',tmp_url)
    book_url <- read_html(tmp_url) %>% html_nodes('a.bookTitle') %>% html_attr('href')
    book_url <- paste0("https://www.goodreads.com",book_url)
    for(j in 1:length(book_url)) {
      tryCatch({
        tmp_book_url <- book_url[j]
        tmp_paragraph <- read_html(tmp_book_url)
        
        #keyword
        keyword <- c(keyword,input_keyword[[k]])
        
        #title
        tmp_title <- tmp_paragraph %>% html_nodes('h1#bookTitle') %>% html_text
        tmp_title <- gsub('\\s+',' ',tmp_title) %>% str_trim
        title <- c(title, tmp_title)
        
        #author
        tmp_author <- tmp_paragraph %>% html_nodes('a.authorName > span') %>% html_text %>%
          str_trim
        tmp_author <- toString(tmp_author)
        author <- c(author, tmp_author)
        
        #rating
        tmp_rating <- tmp_paragraph %>% html_nodes('div#bookMeta > span')
        tmp_rating <- tmp_rating[2] %>% html_text
        tmp_rating <- gsub('\\s+',' ',tmp_rating) %>% str_trim %>% as.numeric()
        rating <- c(rating, tmp_rating)
        
        #Number of rating
        tmp_rating_NUM <- tmp_paragraph %>% html_nodes('a.gr-hyperlink > meta[itemprop^="ratingCount"]') %>%
          html_attr('content') %>% as.integer()
        rating_NUM <- c(rating_NUM, tmp_rating_NUM)
        
        #Number of review
        tmp_review_NUM <- tmp_paragraph %>% html_nodes('a.gr-hyperlink > meta[itemprop^="reviewCount"]') %>%
          html_attr('content') %>% as.integer()
        review_NUM <- c(review_NUM, tmp_review_NUM)
        
        #summary
        tmp_summary <- tmp_paragraph %>% html_nodes('div#description > span[id^="freeTextContainer"]')
        if(length(tmp_summary)==0){
          tmp_summary <- c(" ")
        }else{
          tmp_summary <- tmp_summary %>% html_text
          tmp_summary <- gsub('\\s+',' ', tmp_summary) %>% str_trim
        }
        summary <- c(summary, tmp_summary)
        
        #check rank
        cat("rank", j, "\n")
      }, error = function(e){print("An error occurs. Skip")})
    }
    #check page
    Sys.sleep(2)
    cat(i, "/", page_NUM, "\n")
  }
}

#make CSV
multi_books <- data.frame(keyword, title, author, rating, rating_NUM, review_NUM, summary)
end <- proc.time()
end - start

write.csv(multi_books, file = "multi_books.csv")


