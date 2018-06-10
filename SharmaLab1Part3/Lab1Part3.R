install.packages("twitteR")
install.packages("mapdata")
library(twitteR)
library(ggmap)
library(maps)
library(mapdata)
library(ggplot2)
library(sp)
library(maptools)

consumer_key <- "{YOUR CONSUMER KEY}"
consumer_secret <- "{YOUR CONSUMER SECRET}"
access_token <- "{YOUR ACCESS TOKEN}"
access_secret <- "{ACCESS SECRET}"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

result <- searchTwitter('flu', n=20000)
length(result)

#Reference: https://www.rdocumentation.org/packages/twitteR/versions/1.1.9/topics/twListToDF
dataframe.df <- twListToDF(result)

View(dataframe.df)

#Get ScreenNames of all users from tweets results fethched through 'searchTwitter()'
usernames <- dataframe.df$screenName

#Get User information using 'lookupUsers()'
#Reference: https://stackoverflow.com/questions/40721031/twitter-package-how-to-get-users-informations-from-an-ids-followers-list
users.df <- twListToDF(lookupUsers(usernames))

View(users.df)

#Filtering out empty locations
UsersLocation <- users.df$location[(users.df$location != "")]

View(UsersLocation)
length(UsersLocation)

#Filtering Non-US data from all the locations extracted
RemIndia <- UsersLocation[!grepl(' India', UsersLocation, ignore.case = TRUE)]
RemIndia <- RemIndia[!grepl(',India', RemIndia, ignore.case = TRUE)]
RemEngland  <- RemIndia[!grepl('England', RemIndia, ignore.case = TRUE)]
RemCanada <- RemEngland[!grepl('Canada', RemEngland, ignore.case = TRUE)]
RemSharp <- RemCanada[!grepl('#', RemCanada, ignore.case = TRUE)]
RemLondon <- RemSharp[!grepl('london', RemSharp, ignore.case = TRUE)]
RemUK <- RemLondon[!grepl('united kingdom', RemLondon, ignore.case = TRUE)]
RemEurope <- RemUK[!grepl('united kingdom', RemUK, ignore.case = TRUE)]
RemEurope <- RemEurope[!grepl('uk', RemEurope, ignore.case = TRUE)]
RemArab <- RemEurope[!grepl('arab', RemEurope, ignore.case = TRUE)]
RemOtt <- RemArab[!grepl('otta', RemArab, ignore.case = TRUE)]
RemVancou <- RemOtt[!grepl('vanco', RemOtt, ignore.case = TRUE)]
RemPak <- RemVancou[!grepl('pakist', RemVancou, ignore.case = TRUE)]
RemFrance <- RemPak[!grepl('france', RemPak, ignore.case = TRUE)]
RemEgypt <- RemFrance[!grepl('egypt', RemFrance, ignore.case = TRUE)]
RemItaly <- RemEgypt[!grepl('italy', RemEgypt, ignore.case = TRUE)]
RemAustralia <- RemItaly[!grepl('austra', RemItaly, ignore.case = TRUE)]
RemAlberta <- RemAustralia[!grepl('austra', RemAustralia, ignore.case = TRUE)]
RemGermany <- RemAlberta[!grepl('germany', RemAlberta, ignore.case = TRUE)]
RemRussia <- RemGermany[!grepl('russia', RemGermany, ignore.case = TRUE)]
RemIndoanesia <- RemRussia[!grepl('indones', RemRussia, ignore.case = TRUE)]
RemSweden <- RemIndoanesia[!grepl('sweden', RemIndoanesia, ignore.case = TRUE)]
RemTokyo <- RemSweden[!grepl('tokyo', RemSweden, ignore.case = TRUE)]
RemJapan <- RemTokyo[!grepl('japan', RemTokyo, ignore.case = TRUE)]
RemToronto <- RemJapan[!grepl('toronto', RemJapan, ignore.case = TRUE)]
RemOntario <- RemToronto[!grepl('ontario', RemToronto, ignore.case = TRUE)]
RemTrinidad <- RemOntario[!grepl('trinidad', RemOntario, ignore.case = TRUE)]
RemGod <- RemTrinidad[!grepl('god', RemTrinidad, ignore.case = TRUE)]
RemSnap <- RemGod[!grepl('snap', RemGod, ignore.case = TRUE)]
RemScot <- RemSnap[!grepl('scot', RemSnap, ignore.case = TRUE)]
RemAfrica <- RemScot[!grepl('africa', RemScot, ignore.case = TRUE)]
RemIntern <- RemAfrica[!grepl('intern', RemAfrica, ignore.case = TRUE)]
RemNether <- RemIntern[!grepl('nether', RemIntern, ignore.case = TRUE)]
RemDenver <- RemNether[!grepl('denv', RemNether, ignore.case = TRUE)]
RemEurope <- RemDenver[!grepl('eu', RemDenver, ignore.case = TRUE)]
RemWWW <- RemEurope[!grepl('www', RemEurope, ignore.case = TRUE)]
RemHTTP <- RemWWW[!grepl('http', RemWWW, ignore.case = TRUE)]
RemHell <- RemHTTP[!grepl('hell', RemHTTP, ignore.case = TRUE)]
RemIreland <- RemHell[!grepl('ireland', RemHell, ignore.case = TRUE)]
RemMexico <- RemIreland[!grepl('mexic', RemIreland, ignore.case = TRUE)]
RemPoland <- RemMexico[!grepl('poland', RemMexico, ignore.case = TRUE)]
RemThree <- RemPoland[!grepl('3', RemPoland, ignore.case = TRUE)]
RemAt <- RemThree[!grepl('@', RemThree, ignore.case = TRUE)]
RemPortugal <- RemAt[!grepl('portugal', RemAt, ignore.case = TRUE)]
RemOne <- RemPortugal[!grepl('1', RemPortugal, ignore.case = TRUE)]
RemTwo <- RemOne[!grepl('2', RemOne, ignore.case = TRUE)]
RemFour <- RemTwo[!grepl('4', RemTwo, ignore.case = TRUE)]
RemFive <- RemFour[!grepl('5', RemFour, ignore.case = TRUE)]
RemSix <- RemFive[!grepl('6', RemFive, ignore.case = TRUE)]
RemSeven <- RemSix[!grepl('7', RemSix, ignore.case = TRUE)]
RemEight <- RemSeven[!grepl('8', RemSeven, ignore.case = TRUE)]
RemNine <- RemEight[!grepl('9', RemEight, ignore.case = TRUE)]
RemZero <- RemNine[!grepl('0', RemNine, ignore.case = TRUE)]
RemUp <-  RemZero[!grepl('up ', RemZero, ignore.case = TRUE)]
RemWorld <- RemUp[!grepl('World', RemUp, ignore.case = TRUE)]
RemS <- RemWorld[!grepl('~', RemWorld, ignore.case = TRUE)]
RemGlobal <- RemS[!grepl('global', RemS, ignore.case = TRUE)]
RemHappy <- RemGlobal[!grepl('happy', RemGlobal, ignore.case = TRUE)]
RemLove <- RemHappy[!grepl('love', RemHappy, ignore.case = TRUE)]
RemYou <- RemLove[!grepl('you', RemLove, ignore.case = TRUE)]
RemSpe <- RemYou[!grepl('단', RemYou, ignore.case = TRUE)]
RemSpe <- RemYou[!grepl('市', RemYou, ignore.case = TRUE)]
RemSpe <- RemYou[!grepl(')', RemYou, ignore.case = TRUE)]
RemSpe <- RemYou[!grepl('جد', RemYou, ignore.case = TRUE)]
RemSpe <- RemYou[!grepl('生', RemYou, ignore.case = TRUE)]
RemSpe <- RemYou[!grepl('덕', RemYou, ignore.case = TRUE)]
RemSpe <- RemYou[!grepl('☀', RemYou, ignore.case = TRUE)]
RemSpe <- RemYou[!grepl('ά', RemYou, ignore.case = TRUE)]
RemSpe <- RemYou[!grepl('年', RemYou, ignore.case = TRUE)]
RemSpe <- RemYou[!grepl('<', RemYou, ignore.case = TRUE)]
RemSpe <- RemYou[!grepl('my ', RemYou, ignore.case = TRUE)]
RemSpe <- RemYou[!grepl('earth', RemYou, ignore.case = TRUE)]


length(RemSpe)
View(RemSpe)

write.csv(RemSpe, "myData.csv")

#Manually Filtering Data from myData.csv to remove Characters that were not removed from above filtering
#Diving the file in parts (of less than 2500 rows to allow geocode to process data) - myData3.csv and myData4.csv

#Read data from myData3.csv and myData4.csv
MyData3 <- read.csv(file="myData3.csv", header=TRUE, sep = ",")
MyData4 <- read.csv(file="myData4.csv", header=TRUE, sep = ",")

#Get geocode (Longitude and Latitude values) results for the given locations
result3 <- geocode(as.character(MyData3$x))
dim(result3)
write.csv(result3, "result3.csv")

result4 <- geocode(as.character(MyData4$x))
dim(result4)

#Removing rows with 'NA' values 
#Reference: https://stackoverflow.com/questions/4862178/remove-rows-with-nas-missing-values-in-data-frame
final_result3 <- result3[complete.cases(result3), ]
final_result4 <- result4[complete.cases(result4), ]

dim(final_result3)

#Storing results in final_result3.csv and result4.csv
write.csv(final_result3, "final_result3.csv")
write.csv(final_result3, "result4.csv")

#Manually merging the data of the two CSVs (final_result3.csv and result4.csv)
#by appending data from result4.csv to final_result3.csv
#Reading total data from final_result3.csv into 'final_result'
final_result <- read.csv(file="final_result3.csv", header=TRUE, sep = ",")

dim(final_result)

#Plotting USA map

#Reference https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
#Getting state names from Latitute and Logitude values
latlong2state <- function(final_result) {
  all_states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(all_states$names, ":"), function(x) x[1])
  SpatialStates <- map2SpatialPolygons(all_states, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  SP_points <- SpatialPoints(final_result, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  index <- over(SP_points, SpatialStates)
  state_Names <- sapply(SpatialStates@polygons, function(x) x@ID)
  state_Names[index]
}

testPoints <- data.frame(final_result$long, final_result$lat)

stateList <- latlong2state(testPoints)
length(stateList)
stateList

#Get Flu-frequency for each state
# Reference: https://stackoverflow.com/questions/13841599/calculate-frequency-of-occurrence-in-an-array-using-r
region <- table(stateList)

write.csv(region, "stateFreq.csv")
freqList <- read.csv("stateFreq.csv", header=TRUE, sep = ",") 

#Rename the columnName from 'stateList' to 'region'
#Reference : https://stackoverflow.com/questions/7531868/how-to-rename-a-single-column-in-a-data-frame
colnames(freqList)[2] <- "region"

#Merfging 'states' and 'FreqList' into dataframe map.df
states <- map_data("state")
map.df <- merge(freqList, states, by="region", all.x=T)

#Plotting map using ggplot and peom_polygon
#Reference - https://plot.ly/ggplot2/geom_polygon/
p <- ggplot()
p <- p + geom_polygon( data=map.df, aes(x=long, y=lat, group = group, fill= map.df$Freq),colour="blue" ) + labs(fill="Flu Frequency", title="Twitter Data" )

print(p)


