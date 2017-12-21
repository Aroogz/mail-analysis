#unzip(zipfile = "takeout-20161011T023344Z.zip", exdir = ".")

#convert_mbox_eml(mbox = "./Takeout/Mail/All mail Including Spam and Trash.mbox", "./Mails")



#mail_corpus <- VCorpus(DirSource("Mails"), readerControl = list(reader= readMail))



  



#Processing (probably implement a loop)
mail_corpus <- tm_map(mail_corpus, removeWords, stopwords("en"))
mail_corpus <- tm_map(mail_corpus, stemDocument)

#clear web addresses
for (i in seq(mail_corpus)){
  mail_corpus[[i]]$content <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", mail_corpus[[i]]$content)
  mail_corpus[[i]]$content <- mail_corpus[[i]]$content[mail_corpus[[i]]$content != ""]
}

mail_corpus <- tm_map(mail_corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
mail_corpus <- tm_map(mail_corpus, stripWhitespace)
mail_corpus[[1]]$content

#create a plain text copy
plain_text <- tm_map(mail_corpus, PlainTextDocument)

#getmail function
getmail <- function(string){
  mail <- regmatches(string, regexpr("[[:alnum:]]+\\@[[:alpha:]]+\\.com", string))
  
  return(mail)
}

#getdatetime function
getdatetime <- function(string){
  month.abb.pattern <- paste(month.abb,collapse="|")
  datestring <- substr(string, regexpr(month.abb.pattern, string), nchar(string))
  #datetime <- strptime(datestring, "%b %d %I:%M:%S %z %Y")
  datetime <- strptime(datestring, "%b %d, %Y")
  
  return(datetime)
}


for (i in seq(mail_corpus)){
  datestring <- mail_corpus[[i]]$meta$header[1]
  mailstring <- mail_corpus[[i]]$meta$author
  headingstring <- mail_corpus[[i]]$meta$heading
  
  #Assigning the meta data field for the plain text
  plain_text[[i]]$meta$datetimestamp <- getdatetime(string= datetime)
  plain_text[[i]]$meta$author <- getmail(string = mailstring)
  plain_text[[i]]$meta$heading <- headingstring
  plain_text[[i]]$meta$id <- getmail(string = mailstring)
}

#so I could write a sample function like:
#   (a==b & b==b & c==c)
#create dataframe of email address and datetimestamp


##i need to get the address from the mail content
##this is a big problem
##na 2 weeks things




