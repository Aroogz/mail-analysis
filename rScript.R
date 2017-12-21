#================================= INCUBATION ============================
#=========================================================================

#clear Workspace loading Packages
rm(list = ls())
library(dplyr)
library(ggplot2)
library(ggmap)
library(maptools)
library(maps)
library(tm)
library(tm.plugin.mail)


#Get Data
incubation <- read.csv("./incpreincideasubmissions/Incubation Application-report (Edited) (1).csv",
                       stringsAsFactors = FALSE)

# function to get state
getstate <- function(string){
  states <- c("abia", "abuja", "adamawa","akwa", "anambra", "bauchi", "bayelsa", "benue", "borno","cross river", "delta", "ebonyi", "edo", "ekiti", "enugu", 
             "imo", "jigawa", "kaduna", "kano", "katsina", "kebbi", "kogi", "kwara", "lagos", "nasarawa", "niger", "ogun", "ondo", "osun",
             "oyo", "plateau", "rivers", "sokoto", "taraba", "yobe", "zamfara", "ibadan")
  for(i in states){
    matchLog <- grepl(i, string, ignore.case = TRUE)
    if (matchLog == TRUE){
      if (i == "ibadan"){
        return("oyo")
      }
      return(i)
    }
  }
  return("unknown")

}

#mine education
getedu <- function(string){
  keywords <- c("university", "graduate", "graduated", 
                "studied", "studying", "study", "degree", 
                "msc", "bsc", "b.sc", "m.sc")
  for (i in keywords){
    match.log <- grepl(i, string, ignore.case = T)
    if (match.log == TRUE){
      return("graduate")
    }
  }
  return("unknown/non-graudate")
}





# ============================PRE-PROCESSING====================================== #

incubation_edit <- unique(incubation)
  
#function to get the equivalent willingness
getwilling <- function(code){
  if (code == 1){
    return("willing")
  }
  else{
    return("notwilling")
  }
}

incubation_edit <- mutate(incubation, willingness= lapply(incubation$Are.you.willing.to.move.to.Lagos.for.next.6.months., getwilling))

#convert dates
incubation_edit <- mutate(incubation_edit, 
                          applicationStart = as.Date(incubation_edit$Start.Date..UTC., "%m/%d/%Y %H:%M"),
                          applicationEnd = as.Date(incubation_edit$Submit.Date..UTC., "%m/%d/%Y %H:%M"))

#get the time taken to fill the form
formFilling <- vector()
for (i in seq(length(incubation_edit$Submit.Date..UTC.))){
  timediff <- difftime(incubation_edit$applicationEnd[i], incubation_edit$applicationStart[i], units = "days")
  timediff <- as.numeric(timediff)
  formFilling <- c(formFilling, timediff)
  
}
incubation_edit <- mutate(incubation_edit, formFillingTime= formFilling)

incubation_edit <- mutate(incubation_edit, submissionMonth = months(incubation_edit$applicationEnd, abbreviate = T))
incubation_edit <- mutate(incubation_edit, willingness= as.character(incubation_edit$willingness))

#improve on location
incubation_edit <- mutate(incubation_edit, 
                          location= paste(incubation_edit$Current.location, 
                                          "Nigeria"))
incubation_edit <- mutate(incubation_edit, state= lapply(incubation_edit$Current.location, getstate))

incubation_edit <- mutate(incubation_edit, state= as.character(state))
# =============================The Visuals====================================== #
#total submissions
# par(mar = c(0,0,0,0))
# plot(c(0, 1), c(0, 1), ann = F, 
#      bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
# 
# text(x=0.5, y=0.5, paste("TOTAL NUMBER OF UNIQUE SUBMISSIONS\n",
#                           as.character(nrow(incubation_edit))), 
#      cex = 1.6, col = "black")
# #state distribution
# states <- as.data.frame(table(incubation_edit$state, dnn = c("state")))
# totalStates <- sum(states$Freq)
# percent <-lapply(states$Freq*100/totalStates, FUN = round, 2)
# states <- mutate(states, percentage= paste(percent,"%", sep = ""))
# states <- arrange(states, desc(Freq))
# ggplot(states, aes(factor(state, levels = state), states$Freq))+
#   geom_bar(stat = "identity")+
#   labs(x= "States", y= "NUmber of Submissions")+
#   ggtitle("Number of Applications Across States")+
#   geom_text(aes(label= percentage), size=4, vjust= "bottom")+theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, size = 10))
# 
# #willingness to come
# will <- as.data.frame(table(incubation_edit$willingness, 
#                                    dnn = c("willingness")))
# 
# ggplot(will, aes(will$willingness, 
#                         will$Freq))+geom_bar(stat = "identity",
#                                                     col = "red",
#                                                     fill = "blue",
#                                                     alpha = 0.4)+labs(x =
#       "willingness to come to Lagos", y = "Number of Submissions")+ggtitle(
#         "The Willingness to Come To Lagos for 6 Months")+geom_text(
#           aes(label = Freq), size = 5, vjust= "middle")+theme_bw()

#Location of Applications


loc <- incubation_edit$location
loc.code <- read.csv("incubation_locations.csv", stringsAsFactors = F) #since I have already geocoded
#loc.code <- geocode(location = loc)
# map("world",regions = "Nigeria", fill=TRUE, col="white",bg= "grey", ylim=c(0, 15), mar = c(0,0,0,0))
# 
# points(loc.code$lon, loc.code$lat, pch= 16, alpha = 0.2, col= "blue")
# title(main = "Distribution of the Submissions across Nigeria")

# mp <- NULL
# mapWorld <- borders("world", regions = "Nigeria", colour="gray50", fill="gray50") # create a layer of borders
# mp <- ggplot() +   mapWorld
# 
# #Now Layer the cities on top
# mp <- mp+ geom_point(aes(x=loc.code$lon, y=loc.code$lat) ,color="blue", size=3, alpha= 0.3) 
# mp + coord_cartesian(ylim = c(0,15), xlim = c(0, 20))+labs(x= "", 
#                                                            y= "", title= "Distribution of the Submission Clusters Across Nigeria")+theme_bw()
incubation_edit <- cbind(incubation_edit, loc.code)



#writing to file
incubationToFile <- data.frame(lapply(incubation_edit, as.character), 
                               stringsAsFactors=FALSE)

write.csv(incubationToFile, "Processed/incubation.csv")


# ======================Pre-incubation code======================================= #
rm(list = ls())

library(dplyr)
library(tidyr)
preIncubation <- read.csv("incpreincideasubmissions/Pre-Incubation Application-report (Edited) (1).csv", 
                          stringsAsFactors = F)

copy <- preIncubation
copy <- copy[!duplicated(copy$Name.of.proposed.solution), ]
copy <- copy %>% gather(sector, presence, Agriculture:Other )

## ******** leave duplicates (due to gather) duplicate!!!!! *****

#presence code
getCode <- function(string){
  if (string == ""){
    return(0)
  }
  else{
    return(1)
  }
}

copy <- mutate(copy, presenceCode = as.character(
  sapply(copy$presence, getCode)))


copy <- subset(copy, presence != "")


write.csv(copy, "Processed/preIncProcessed.csv")

# ==================incubation text analysis ============#
library(tm)
library(SnowballC)

docs <- incubationToFile$What.market.challenge.are.you.addressing.

corp <- VCorpus(VectorSource(docs))

#preprocessing
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, removeWords, stopwords())
corp <- tm_map(corp, stemDocument)
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, PlainTextDocument)

#staging
dtm <- DocumentTermMatrix(corp)
tdm <- TermDocumentMatrix(corp)

#exploring
freq <- colSums(as.matrix(dtm))
ord <- order(freq)

dtm_s <- removeSparseTerms(dtm, 0.1) #play with this number


wordcloud(names(freq), freq, min.freq = 5)

#================================= the previous collections =============

prev_pre_inc_raw <- read.csv("C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years/wp_cformsdata (1).csv", 
         stringsAsFactors = F)
prev_pre_inc <- select(prev_pre_inc_raw, -X1)
prev_pre_inc <- spread(prev_pre_inc, page, X.pre.incubation.)
prev_pre_inc <- subset(prev_pre_inc, page %in% c("/pre-incubation/",
                                                 "/incubation/"))
#seperate inc and pre-inc
inc_prev <- subset(prev_pre_inc, page == "/incubation/")
pre_inc_prev <- subset(prev_pre_inc, page == "/pre-incubation/")

#dispose data sets
rm(prev_pre_inc_raw, prev_pre_inc)

#function remove useless columns
removeAllNAColumns <- function(dataframe){
  cols <- names(dataframe)
  col2remove <- c()
  for (column in cols){
    if (sum(is.na(dataframe[,column])) == nrow(dataframe)){
      col2remove <- c(col2remove, column)
    }
  }
  fresh_out_of_ply <- select(dataframe, -one_of(col2remove))
  return(fresh_out_of_ply)
}

#remove columns with nothing
inc_prev <- removeAllNAColumns(inc_prev)
inc_prev <- select(inc_prev, -(Fieldset1:FieldsetEnd8))
pre_inc_prev <- removeAllNAColumns(pre_inc_prev)
pre_inc_prev <- select(pre_inc_prev, -1, -(7:8))


#write the structured data to file
write.csv(inc_prev, "Processed/previous_incubation.csv")
write.csv(pre_inc_prev, "Processed/previous_PreIncubation.csv")



# ============== more cleaning for merging ============

# to be cleaned later
inc_prev <- read.csv("Processed/previous_incubation.csv", stringsAsFactors = F)
pre_inc_prev <- read.csv("Processed/previous_PreIncubation.csv", stringsAsFactors = F)
#========================================

#beefing up inc_prev
inc_prev <- mutate(inc_prev, location= paste(inc_prev$Current.location,
                                             "Nigeria"))
inc_prev <- mutate(inc_prev,  
                   state = as.character(sapply(inc_prev$location, getstate)))
#inc_prev_loc <- geocode(inc_prev$Current.location)
inc_prev_loc <- read.csv("inc_prev_loc.csv", stringsAsFactors = F)
#write.csv(inc_prev_loc, "inc_prev_loc.csv")
inc_prev <- cbind(inc_prev, inc_prev_loc)
inc_prev <- data.frame(inc_prev)
inc_prev <- select(inc_prev, -1,-2,-13)
inc_prev <- inc_prev[!duplicated(inc_prev),]

#beefing up pre_inc_prev
pre_inc_prev <- select(pre_inc_prev, -1)
pre_inc_prev <- pre_inc_prev[-1,]
row.names(pre_inc_prev) <- 1:nrow(pre_inc_prev)
pre_inc_prev <- pre_inc_prev[!duplicated(pre_inc_prev), ]

#remove duplicate submissions
#inc_prev <- inc_prev[!duplicated(inc_prev$`Venture Name`),]
#pre_inc_prev <- pre_inc_prev[!duplicated(pre_inc_prev$`Email Address`),]


#===================================================================

# ========================= from the mail ==========================

# ===== functions ================
getmail <- function(string){
  mail <- regmatches(string, regexpr("[[:alnum:]]+\\@[[:alpha:]]+\\.(com|net|co\\.uk)", string))
  
  return(mail)
}

getdatetime <- function(string){
  
  month.abb.pattern <- paste(month.abb, collapse = "|")
  datestring <- substr(string, regexpr(month.abb.pattern, string), nchar(string))
  #datetime <- strptime(datestring, "%b %d %I:%M:%S %z %Y")
  datetime <- strptime(datestring, "%b %d, %Y")
  
  if (is.na(datetime)){
    datestring <- substr(string, regexpr("[0-9]", string), nchar(string))
    datetime <- strptime(datestring, "%d/%m/%Y")
  }
  
  
  return(datetime)
}

cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix( n-nrow(x), ncol(x))))) 
}

#================================


#unzip(zipfile = "C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/Email Work/mail_archive.zip", 
#      exdir = "C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years")

#convert_mbox_eml(mbox = "C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years/Takeout/Mail/Inbox.mbox",
#                 "C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years/Takeout/Mail/all_mails")

#unzip(zipfile = "C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years/Takeout_inc/takeout-20161110T151930Z.zip",
#      exdir = "C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years/Takeout_inc")
#convert_mbox_eml(mbox = "C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years/Takeout_inc/Takeout/Mail/Inbox.mbox",
#                 "C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years/Takeout_inc/Takeout/all_mail_inc")

#mail_corpus <- VCorpus(DirSource("C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years/Takeout/Mail/all_mails"), 
#                       readerControl = list(reader= readMail))

mail_corpus_inc <- VCorpus(DirSource("C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years/Takeout_inc/Takeout/all_mail_inc"),
                           readerControl = list(reader= readMail))

## ============ FOR INCUBATION ##
mbox_list_inc <- list.files(path = "C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years/Takeout_inc/Takeout/Mail/")

dir_inc_prev <- "C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years/Takeout_inc/Takeout/Mail/"

for (i in seq(mbox_list_inc)){
   path <- file.path(dir_inc_prev, mbox_list_inc[i])
   convert_mbox_eml(mbox = path, dir = paste(path, "-", sep = ""))
 }
eml_list_inc <- list.dirs("C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/prev_years/Takeout_inc/Takeout/Mail/",
                          recursive = F)

mail_corpus_inc <- VCorpus(DirSource(eml_list_inc),
                           readerControl = list(reader= readMail))

test_String_inc <- mail_corpus_inc[[1]]$content[5]
datelist_inc <- NULL
email_address_list_inc <- NULL

for (i in seq(mail_corpus_inc)){
  try(
    if (mail_corpus_inc[[i]]$content[5] == test_String_inc){
      date_ <- as.character(getdatetime(mail_corpus_inc[[i]]$content[7]))
      datelist_inc <- c(datelist_inc, date_)
      
      email <- getmail(mail_corpus_inc[[i]]$content)[1]
      email_address_list_inc <- c(email_address_list_inc, email)
    }
  ,silent = T)
}
inc_prev_mail_time <- data.frame(mail = email_address_list_inc,
                                     dates= as.character(datelist_inc), stringsAsFactors = F)
inc_prev_mail_time <- subset(inc_prev_mail_time, 
                             !is.na(mail))
inc_prev_mail_time <- inc_prev_mail_time[!duplicated(inc_prev_mail_time$mail),]

# merge inc_prev with the date
full_inc_prev <- data.frame(cbind.fill(inc_prev, inc_prev_mail_time),
                            stringsAsFactors = F)
willingness_inc <- factor(full_inc_prev$Are.you.willing.to.move.to.Lagos.for.next.6.months.,
                      levels = c("Yes", "No"), labels = c("willing", "not willing"))
full_inc_prev <- mutate(full_inc_prev, 
                        willingness = as.character(willingness_inc))
full_inc_prev <- full_inc_prev[!duplicated(full_inc_prev), ]
write.csv(full_inc_prev, "Processed/full_inc_prev.csv")

## ====================== get the necessary mails (pre_inc) ===========

## pre_inc_prev
test_String <- mail_corpus[[16]]$content[5] #the string identifies pre-inc

inc_count <- 0


datelist <- NULL
email_address_list <- NULL


for (i in seq(mail_corpus)){
  try(if (mail_corpus[[i]]$content[5] == test_String){
      inc_count <- inc_count +1
    
      date_ <- as.character(getdatetime(mail_corpus[[i]]$content[7]))
      datelist <- c(datelist, date_)
      
      email <- getmail(mail_corpus[[i]]$content)[1]
      email_address_list <- c(email_address_list, email)
    
  }, silent = TRUE)
  
}

pre_inc_prev_mail_time <- data.frame(mail = email_address_list,
                                     dates= as.character(datelist), stringsAsFactors = F)

#clean up
pre_inc_prev_mail_time <- subset(pre_inc_prev_mail_time, 
                                 !is.na(mail))
pre_inc_prev_mail_time <- pre_inc_prev_mail_time[!duplicated(pre_inc_prev_mail_time$mail),]

#merging to give full pre_inc_prev
full_pre_inc_prev <- merge(pre_inc_prev, pre_inc_prev_mail_time, 
           by.x="Email.Address", by.y= "mail", all.x= TRUE)

rm(pre_inc_prev_mail_time)

full_pre_inc_prev <- full_pre_inc_prev[!duplicated(full_pre_inc_prev),]

write.csv(full_pre_inc_prev, "Processed/full_pre_inc_prev.csv")

##inc_prev


# ============== integratin prev and typescript (for pre_inc for now) ===============
inc <- read.csv("Processed/incubation.csv", stringsAsFactors = F)
pre_inc <- read.csv("Processed/preIncProcessed.csv", stringsAsFactors = F)

#combine the full_inc_prev and new inc
m_inc <- select(inc, -1,-2,-21,-22,-24,-25,-26,-27)
m_inc_prev <- select(full_inc_prev, -24,-21,-4)

#arrange m_inc_prev
m_inc_prev <- select(m_inc_prev, 15,16,10,11,14,4,7,12,13,6,8,9,1,5,17,3,2,22,23,18,19,20,21)
names(m_inc_prev) <- names(m_inc)

# bind inc
merged_inc <- rbind(m_inc_prev, m_inc)
write.csv(merged_inc, "Processed/merged_inc.csv")


# combine the full_pre_inc_prev and the new pre_inc
m_pre_inc <- select(pre_inc, -1,-2,-5,-11,-12,-14,-15)
m_pre_inc_prev <- select(full_pre_inc_prev, -1,-3,-6)

#seperate sector for m_pre_inc_prev
m_pre_inc_prev <- separate(m_pre_inc_prev, 
                           col = "What.sector.of.the.economy.does.it.address.", 
                           into = c("v1","v2","v3","v4"), sep = ",")

m_pre_inc_prev <- gather(m_pre_inc_prev, options, sector, v1:v4)
m_pre_inc_prev <- subset(m_pre_inc_prev, !sector == "")
m_pre_inc_prev <- select(m_pre_inc_prev, -options)

m_pre_inc_prev <- select(m_pre_inc_prev, 1,5,6,2,4,3,7,8)

names(m_pre_inc_prev) <- names(m_pre_inc)

merged_pre_inc <- rbind(m_pre_inc_prev, m_pre_inc)

write.csv(merged_pre_inc, "Processed/merged_pre_inc.csv")

# ====================== further cleaning ====================

## merged_inc

merged_inc <- read.csv("C:/Users/AROOGZ/Documents/My Works/WORK - CcHUB/Inc and Pre-Inc/Processed/merged_inc.csv",
                       stringsAsFactors = F)
merged_inc <- merged_inc[,-1]
merged_inc$willingness[merged_inc$willingness == "notwilling"] <- "not willing"

temp.1 <- merged_inc$Start.Date..UTC.

merged_inc$Start.Date..UTC. <- ifelse(grepl("-", temp.1), 
                                      yes = as.character(as.Date(temp.1)), 
                                      no = as.character(as.Date(temp.1, "%m/%d/%Y")))
rm(temp.1)

require(lubridate)
temp.2 <- merged_inc$Launch.Pilot.date
merged_inc$launch.date.clean <- as.Date(parse_date_time(temp.2, orders = c("dmy", "my","y")))

write.csv(merged_inc, "Processed/merged_inc_1.csv",row.names = F)

# creating various version to take care of NAs
merged_inc_start_date <- merged_inc[!is.na(merged_inc$Start.Date..UTC.), ]
merged_inc_launch_date <- merged_inc[!is.na(merged_inc$launch.date.clean), ]
merged_inc_completecases <- merged_inc[complete.cases(merged_inc),]

write.csv(merged_inc, "Processed/in Excel/merged_inc_1.csv",row.names = F)
write.csv(merged_inc_start_date,"Processed/in Excel/merged_inc_start_date.csv", row.names = F)
write.csv(merged_inc_launch_date, "Processed/in Excel/merged_inc_launchdate.csv", row.names = F)
write.csv(merged_inc_completecases, "Processed/in Excel/merged_inc_comp.csv", row.names = F)


## merged_pre_inc

merged_pre_inc <- merged_pre_inc[-1,]
merged_pre_inc <- merged_pre_inc[,-1]
merged_pre_inc$Start.Date..UTC. <- as.Date(merged_pre_inc$Start.Date..UTC.)
merged_pre_inc$sector[merged_pre_inc$sector == "Small.business.development"] <- "Small Business Development"
merged_pre_inc$sector[merged_pre_inc$sector == "Inclusive.technology"] <- "Inclusive Technology"
merged_pre_inc$sector[merged_pre_inc$sector == "Other"] <- "Others"

# creating various versions
merged_pre_inc_start_date <- merged_pre_inc[!is.na(merged_pre_inc$Start.Date..UTC.),]
merged_pre_inc_start_date <- merged_pre_inc_start_date[!duplicated(merged_pre_inc_start_date$Describe.the.problem.challenge.you.seek.to.address...100.Words.), ]

merged_pre_inc_completecases <- merged_pre_inc[complete.cases(merged_pre_inc), ]
merged_pre_inc_completecases <- merged_pre_inc_completecases[!duplicated(merged_pre_inc_completecases$Describe.the.problem.challenge.you.seek.to.address...100.Words.),]

merged_pre_inc_unique <- merged_pre_inc[!duplicated(merged_pre_inc$Describe.the.problem.challenge.you.seek.to.address...100.Words.),]

write.csv(merged_pre_inc, "Processed/in Excel/merged_pre_inc_1.csv", row.names = F)
write.csv(merged_pre_inc_start_date, "Processed/in Excel/merged_pre_inc_start_date.csv", row.names = F)
write.csv(merged_pre_inc_completecases, "Processed/in Excel/merged_pre_inc_comp.csv", row.names = F)
write.csv(merged_pre_inc_unique, "Processed/in Excel/merged_pre_inc_uniq.csv", row.names = F)

## ============ Mining ===========
# pre_inc
grad <- sapply(merged_pre_inc_unique$Tell.us.about.yourself.and.others.working.on.the.idea..100.words., getedu)
pre_inc_grad <- as.data.frame(table(grad))
write.csv(pre_inc_grad, "Processed/in Excel/merged_pre_inc_grad.csv", row.names = F)

