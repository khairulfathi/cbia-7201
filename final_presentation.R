# load required libraries
library(plyr)
library(psych)
library(plotly)
library(summarytools)

# start to read both CSV files
csv_surv <- read.csv('Student_Survey.csv')
csv_test <- read.csv('Speedtest_Result.csv')

# rename variables name for easier reference
tmpstud <- plyr::rename(csv_surv, c(Academic.Programme = "Programme", Internet.Connection.Method = "SSID", Main.Usage.of.Internet = "Usage", On.overall.basis..how.satisfied.are.you.with.wireless.internet.connection.provided.by.IIUM. = "Satisfaction", Propose.a.solution.to.improve.IIUM.WIFI.services. = "Comment", Average.Duration.of.Daily.Use.of.Internet = "Duration", How.many.times.have.your.Internet.connection.been.suddenly.disconnected.or.face.intermittent.connection.in.past.3.days. = "Disconnected", Is.WiFi.coverage.adequate.and.within.acceptable.signal.strength. = "Coverage", How.satisfied.are.you.with.the.network.speed.to.achieve.your.main.usages. = "Speed"))
tmptest <- plyr::rename(csv_test, c(Mahallah.Dept = "Mahallah", Download.Mbps = "Download", Upload.Mbps = "Upload", Ping.ms = "Ping", Jitter.ms = "Jitter", Loss.. = "Loss"))

# filter test records and removed unwanted variables
tmpstud <- tmpstud[which(tmpstud$Gender == ''), ]
tmpstud$Timestamp <- tmpstud$Gender <- tmpstud$Comment <- NULL
tmpstud$Usage[7] <- "Study and assignment, Topic and subject research, Casual browsing"
tmpstud$Usage <- strsplit(as.character(tmpstud$Usage), ",")
tmpstud$Satisfaction <- ifelse(tmpstud$Satisfaction == 1, "Very Dissatisfied", ifelse(tmpstud$Satisfaction == 2, "Dissatisfied", ifelse(tmpstud$Satisfaction == 3, "Satisfied", "Very Satisfied")))
tmpstud$Speed <- ifelse(tmpstud$Speed == 1, "Very Dissatisfied", ifelse(tmpstud$Speed == 2, "Dissatisfied", ifelse(tmpstud$Speed == 3, "Satisfied", "Very Satisfied")))

tmptest$Loss <- 0

# start to process final data frame
k <- 1

Respondent <- character()
Programme <- character()
SSID <- character()
Mahallah <- character()
Usage <- character()
Satisfaction <- character()
Duration <- character()
Speed <- character()
Disconnected <- character()
Coverage <- character()

# iterate each row to split Usage into separate line in data frame
for(i in 1:nrow(tmpstud))
{
  for(j in 1:3)
  {
    Respondent[k] <- i # to identify unique number of respondent
    Programme[k] <- as.character(tmpstud$Programme[i])
    SSID[k] <- as.character(tmpstud$SSID[i])
    Mahallah[k] <- as.character(tmpstud$Mahallah[i])
    Usage[k] <- trimws(tmpstud$Usage[[i]][j]) # trim leading and trailing whitespace
    Satisfaction[k] <- as.character(tmpstud$Satisfaction[i])
    Duration[k] <- as.character(tmpstud$Duration[i])
    Speed[k] <- as.character(tmpstud$Speed[i])
    Disconnected[k] <- as.character(tmpstud$Disconnected[i])
    Coverage[k] <- as.character(tmpstud$Coverage[i])
    
    k <- k + 1
  }
}

# finalized data frame
dfstudentB <- data.frame(Respondent, Programme, SSID, Mahallah, Usage, Satisfaction, Duration, Speed, Disconnected, Coverage, stringsAsFactors = TRUE)
dfstudentA <- unique(dfstudentB[, -5]) # minus Usage and retrieve only unique records

dftest <- tmptest

# High level summary of Student Survey responses
dfSummary(dfstudentA[, -1], max.distinct.values = 15, plain.ascii = FALSE, style = "grid", graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "./img")

# pie - Primary Usage of Internet
dfusage <- count(dfstudentB$Usage)
plot_ly(dfusage, labels = dfusage$x,values = ~dfusage$freq, type = 'pie') %>% 
  layout(title = 'Primary Usage of Internet', xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# stacked bar - Level of Satisfaction by Mahallah
dfsatisfaction <- aggregate(dfstudentA[ , c(4,5)], by = list(dfstudentA$Mahallah, dfstudentA$Satisfaction), FUN = length, simplify = TRUE)
dfsatisfaction <- plyr::rename(dfsatisfaction, c(Mahallah = "Count", Group.1 = "Mahallah", Group.2 = "Level"))
dfsatisfaction$Satisfaction <- NULL
dfsatisfaction <- reshape(dfsatisfaction, timevar = "Level", idvar = "Mahallah", direction = "wide")
dfsatisfaction <- plyr::rename(dfsatisfaction, c("Count.Very Dissatisfied" = "VeryDissatisfied", "Count.Dissatisfied" = "Dissatisfied", "Count.Satisfied" = "Satisfied", "Count.Very Satisfied" = "VerySatisfied"))

plot_ly(dfsatisfaction2, x = ~Mahallah, y = ~VeryDissatisfied, type = 'bar', name = 'Very Dissatisfied') %>%
  add_trace(y = ~Dissatisfied, name = 'Dissatisfied') %>%
  add_trace(y = ~Satisfied, name = 'Satisfied') %>%
  add_trace(y = ~VerySatisfied, name = 'Very Satisfied') %>%
  layout(title = 'Level of Satisfaction by Mahallah', yaxis = list(title = 'Count'), barmode = 'stack')

# summary - speedtest
dfsummary <- dftest %>% 
  group_by(Mahallah) %>%
  summarise(Download = mean(Download), Upload = mean(Upload), Ping = mean(Ping), Jitter = mean(Jitter))

dfsummary <- dfsummary[order(-dfsummary$Download), ]
View(dfsummary)
