# Storm Data :Course Project 2
Anjali Singh  
3 September 2017  


## An analysis of Impact of Weather Events on Public Health and Economy in the United States
### Synopsis
The project involves exploring the storm data provided as a part of the course project 2. The data comes from U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. The database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.
The goal in this project is analyse the impact of different weather events can have on public health and economy. The analysis will involve downloading,reading and per processing the data.The data base has 902297 record spreading across 1950 -2011.In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years have more complete records
The data used for analysis will be subset of the data provided as not every variable will used in the analysis. We had two basic question to look for

1)Which event caused the most deaths/injuries in US

A- Tornado was a major killer

2)Which event that caused maximum property and crop loss. 

A- Floods and Droughts caused major Damage here

###3 Data Processing and Libraries
####3.1 Load the data and remove necessary data

```r
library(stringr)
library(ggplot2)
require(gridExtra)
#This will setup the directory for the assignent, It will create directory, download the files from the web
# and will unzip the files to be use for the analysis
mainDir<-getwd()
subDir<-"Course5Assignment2"
if (file.exists(subDir)){
  setwd(file.path(mainDir, subDir))
} else {
  dir.create(file.path(mainDir, subDir))
  setwd(file.path(mainDir, subDir))
  
}
#download the file and unzip into created folder
desc<-"StormData.bzip2"
mDir<-paste(getwd(),"/",desc,sep = "")
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if (!file.exists(mDir)){
  download.file(url, dest=desc, mode="wb") 
}

StormData<-read.csv(file="StormData.bzip2",header = TRUE,na.strings = "NA",sep = ",")

colnames<-c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
smallStormData<-StormData[,colnames]
```
####3.2 Clean the data and make a tidy data set

```r
temp<-smallStormData
evet<-temp$EVTYPE
evet<-str_trim(evet,side="both") 

#Following are the series of R commands to process the data and tag them along the Eventtype

evet<-gsub("^TSTM.*", "Thunderstrom Wind",evet,ignore.case = TRUE)
evet<-gsub("^Thu.*", "Thunderstrom Wind",evet,ignore.case = TRUE)
evet<-gsub("^TUNDERSTORM WIND.*", "Thunderstrom Wind",evet,ignore.case = TRUE)

evet<-gsub("^vol.*", "Volcanic Ash",evet,ignore.case = TRUE)
evet<-gsub("^WILD.*", "wildfire",evet,ignore.case = TRUE)
evet<-gsub("^WATER.*", "Waterspout",evet,ignore.case = TRUE)
evet<-gsub("^WINTER STORM.*", "Winter Storm",evet,ignore.case = TRUE)
evet<-gsub("^WINTER WEATHER.*", "Winter Weather",evet,ignore.case = TRUE)
evet<-gsub("^Wintry mix.*", "Wintry mix",evet,ignore.case = TRUE)
evet<-gsub("^WINTER MIX.*", "Wintry mix",evet,ignore.case = TRUE)
evet<-gsub("^WINTERY MIX.*", "Wintry mix",evet,ignore.case = TRUE)
evet<-gsub("^WINTERY MIX.*", "Wintry mix",evet,ignore.case = TRUE)
evet<-gsub("^HIGH WIND.*", "High Wind ",evet,ignore.case = TRUE)
evet<-gsub("^WAYTERSPOUT.*", "Waterspout",evet,ignore.case = TRUE)
evet<-gsub("^Saharan Dus.*", "Dust Devil",evet,ignore.case = TRUE)
evet<-gsub("^LIGNTNING.*", "Lightning",evet,ignore.case = TRUE)



c<-grep("ASTRONOMICAL LOW TIDE*",evet,ignore.case = TRUE)
evet[c]<-"ALT"
c<-grep("flash",evet,ignore.case = TRUE)
evet[c]<-"Flash"
c<-grep("^Coastal",evet,ignore.case = TRUE)
evet[c]<-"Coastal"
c<-grep("Lakeshore",evet,ignore.case = TRUE)
evet[c]<-"Lk"
c<-grep("LAKE FLOOD",evet,ignore.case = TRUE)
evet[c]<-"Lk"
c<-grep("flOOD",evet,ignore.case = TRUE)
evet[c]<-"Flood"
c<-grep("flash",evet,ignore.case = TRUE)
evet[c]<-"Flash Flood"
c<-grep("^Coastal",evet,ignore.case = TRUE)
evet[c]<-"Coastal Flood"
c<-grep("Frost",evet,ignore.case = TRUE)
evet[c]<-"Frost/Freeze"
c<-grep("Blizzard",evet,ignore.case = TRUE)
evet[c]<-"Blizzard"
c<-grep("^Cold",evet,ignore.case = TRUE)
evet[c]<-"Cold/Wind Chill"
c<-grep("Extreme",evet,ignore.case = TRUE)
evet[c]<-"Extreme"

c<-grep("DROUGHT",evet,ignore.case = TRUE)
evet[c]<-"Drought"
c<-grep("Heat",evet,ignore.case = TRUE)
evet[c]<-"Heat"
c<-grep("Debris",evet,ignore.case = TRUE)
evet[c]<-"Debris Flow"
c<-grep("TORNADO",evet,ignore.case = TRUE)
evet[c]<-"Tornado"
evet<-gsub("^TORNDAO*", "Tornado ",evet,ignore.case = TRUE)
c<-grep("Strong",evet,ignore.case = TRUE)
evet[c]<-"Strong"
c<-grep("^Wind",evet,ignore.case = TRUE)
evet[c]<-"Strong"
evet<-gsub("^WND*", "Strong ",evet,ignore.case = TRUE)
c<-grep("Summary",evet,ignore.case = TRUE)
evet[c]<-"Summary"
c<-grep("sURGE",evet,ignore.case = TRUE)
evet[c]<-"Storm Surge/Tide"
c<-grep("^Hail",evet,ignore.case = TRUE)
evet[c]<-"Hail"
c<-grep("Sleet",evet,ignore.case = TRUE)
evet[c]<-"Sleet"
c<-grep("COLD",evet,ignore.case = TRUE)
evet[c]<-"Cold/Wind Chill"
c<-grep("Rain",evet,ignore.case = TRUE)
evet[c]<-"Heavy Rain"
c<-grep("Snow",evet,ignore.case = TRUE)
evet[c]<-"Heavy Snow"
c<-grep("Surf",evet,ignore.case = TRUE)
evet[c]<-"Heavy Surf"
c<-grep("HIGH WINDS",evet,ignore.case = TRUE)
evet[c]<-"High Wind"
c<-grep("Hurricane",evet,ignore.case = TRUE)
evet[c]<-"Hurricane (Typhoon)"
c<-grep("Typhoon",evet,ignore.case = TRUE)
evet[c]<-"Hurricane (Typhoon)"
c<-grep("^Ice",evet,ignore.case = TRUE)
evet[c]<-"Ice Storm"
c<-grep("Lake.*",evet,ignore.case = TRUE)
evet[c]<-"Lake-Effect Snow"
c<-grep("Lightning.*",evet,ignore.case = TRUE)
evet[c]<-"Lightning"
c<-grep("MARINE TSTM WIND",evet,ignore.case = TRUE)
evet[c]<-"Marine Thunderstorm Wind"
c<-grep("MARINE HAIL",evet,ignore.case = TRUE)
evet[c]<-"Marine Hail"
c<-grep("MARINE HIGH WIND",evet,ignore.case = TRUE)
evet[c]<-"Marine High Wind"
c<-grep("MARINE STRONG WIND",evet,ignore.case = TRUE)
evet[c]<-"Marine Strong Wind"
c<-grep("MARINE THUNDERSTORM WIND",evet,ignore.case = TRUE)
evet[c]<-"Mtw"
c<-grep("Rip Current,*",evet,ignore.case = TRUE)
evet[c]<-"Rip Current"
c<-grep("Seiche,*",evet,ignore.case = TRUE)
evet[c]<-"Seiche"
c<-grep("Tropical Storm.*",evet,ignore.case = TRUE)
evet[c]<-"Tropical Storm"
c<-grep("Tropical Depression.*",evet,ignore.case = TRUE)
evet[c]<-"Tropical Depression"
c<-grep("Tsunami.*",evet,ignore.case = TRUE)
evet[c]<-"Tsunami"
c<-grep("Fog.*",evet,ignore.case = TRUE)
evet[c]<-"Dense Fog"
c<-grep("Smoke*",evet,ignore.case = TRUE)
evet[c]<-"Dense Smoke"
c<-grep("DEVIL*",evet,ignore.case = TRUE)
evet[c]<-"Dust Devil"
c<-grep("DUST STORM.*",evet,ignore.case = TRUE)
evet[c]<-"Dust Storm"
c<-grep("DUSTSTORM.*",evet,ignore.case = TRUE)
evet[c]<-"Dust Storm"
c<-grep("Cloud*",evet,ignore.case = TRUE)
evet[c]<-"Funnel Cloud"
c<-grep("SEVERE.*",evet,ignore.case = TRUE)
evet[c]<-"Thunderstrom Wind"
c<-grep("DRY.*",evet,ignore.case = TRUE)
evet[c]<-"Heat"
c<-grep("Wet.*",evet,ignore.case = TRUE)
evet[c]<-"Heavy Rain"
c<-grep("HOT.*",evet,ignore.case = TRUE)
evet[c]<-"Heat"
c<-grep("warm.*",evet,ignore.case = TRUE)
evet[c]<-"Heat"
c<-grep("TIDE*",evet,ignore.case = TRUE)
evet[c]<-"Storm Surge/Tide"
c<-grep("WIND CHILL",evet,ignore.case = TRUE)
evet[c]<-"Extreme Cold/Wind Chil"
c<-grep("Marine Hail",evet,ignore.case = TRUE)
evet[c]<-"MH"
c<-grep("Hail",evet,ignore.case = TRUE)
evet[c]<-"Hail"
c<-grep("DUST",evet,ignore.case = FALSE)
evet[c]<-"Dust Devil"
c<-grep("STREAM",evet,ignore.case = TRUE)
evet[c]<-"Stream"
c<-grep("FREEZE",evet,ignore.case = TRUE)
evet[c]<-"Frost/Freeze"

c<-grep("PRECIPITATION*",evet,ignore.case = TRUE)
evet[c]<-"Heavy Snow"
c<-grep("Avalanc.*",evet,ignore.case = TRUE)
evet[c]<-"Avalanche"
c<-grep("FUNNELS.*",evet,ignore.case = TRUE)
evet[c]<-"Funnel Cloud"
c<-grep("FUNNEL*",evet,ignore.case = TRUE)
evet[c]<-"Funnel Cloud"
c<-grep("slides*",evet,ignore.case = TRUE)
evet[c]<-"Landslide"
c<-grep("ICE.*",evet,ignore.case = TRUE)
evet[c]<-"Ice Storm"
c<-grep("Freezing.*",evet,ignore.case = TRUE)
evet[c]<-"Frost/Freeze"
c<-grep("UNSEA*",evet,ignore.case = TRUE)
evet[c]<-"Cold/Wind Chill"
c<-grep("swell.*",evet,ignore.case = TRUE)
evet[c]<-"High Surf"
c<-grep("WAVES*",evet,ignore.case = TRUE)
evet[c]<-"Hifh Surf"
c<-grep("HIGH WATER*",evet,ignore.case = TRUE)
evet[c]<-"High Surf"
c<-grep("MICROBURST.*",evet,ignore.case = TRUE)
evet[c]<-"Microburst"
c<-grep("URBAN*",evet,ignore.case = TRUE)
evet[c]<-"Stream"
c<-grep("SEAs*",evet,ignore.case = TRUE)
evet[c]<-"Hifh Surf"
c<-grep("TEMPERATURE*",evet,ignore.case = TRUE)
evet[c]<-"Excessive Heat"
c<-grep("RECORD HIGH*",evet,ignore.case = TRUE)
evet[c]<-"Excessive Heat"
c<-grep("HIGH  WINDS*",evet,ignore.case = TRUE)
evet[c]<-"High Wind"
c<-grep("Gusty Wind*",evet,ignore.case = TRUE)
evet[c]<-"Thunderstorm Wind"
c<-grep("Gusty*",evet,ignore.case = TRUE)
evet[c]<-"Thunderstorm Wind"
c<-grep("ICY.*",evet,ignore.case = TRUE)
evet[c]<-"Ice Storm"
c<-grep("SHOWER*",evet,ignore.case = TRUE)
evet[c]<-"Heavy Rain"
c<-grep("COOL*",evet,ignore.case = TRUE)
evet[c]<-"Cold/Wind Chill Rain"
c<-grep("HYPo.*",evet,ignore.case = TRUE)
evet[c]<-"Extreme Cold/Wind Chill"
c<-grep("gradient wind*",evet,ignore.case = TRUE)
evet[c]<-"Gradient wind"

c<-grep("STORM FORCE WINDS*",evet,ignore.case = TRUE)
evet[c]<-"Storm Surge/Tide"
c<-grep("Metro Storm, May 26*",evet,ignore.case = TRUE)
evet[c]<-"Storm Surge/Tide"
c<-grep("Whirlwind*",evet,ignore.case = TRUE)
evet[c]<-"Tornado"
c<-grep("RAPIDLY RISING WATER*",evet,ignore.case = TRUE)
evet[c]<-"Flash Flood"
c<-grep("DAM*",evet,ignore.case = TRUE)
evet[c]<-"Flash Flood"
c<-grep("DOWNBURST*",evet,ignore.case = TRUE)
evet[c]<-"Thunderstorm Wind"
c<-grep("Surf.*",evet,ignore.case = TRUE)
evet[c]<-"High Surf"
c<-grep("FIRE.*",evet,ignore.case = TRUE)
evet[c]<-"Fires"
c<-grep("Beach*",evet,ignore.case = TRUE)
evet[c]<-"Beach Erosion"
c<-grep("Glaze*",evet,ignore.case = TRUE)
evet[c]<-"Frost/Freeze"
c<-grep("^High Wind *",evet,ignore.case = TRUE)
evet[c]<-"High Wind"
c<-grep("^Other *",evet,ignore.case = TRUE)
evet[c]<-"Other"
c<-grep("ALT",evet,ignore.case = TRUE)
evet[c]<-"Astronimical Low Tide"
c<-grep("MH",evet,ignore.case = TRUE)
evet[c]<-"Marine Hail"
c<-grep("Mtw",evet,ignore.case = TRUE)
evet[c]<-"Marine Thunderstorm Wind"
c<-grep("^Lk*",evet,ignore.case = TRUE)
evet[c]<-"Lakeshore Flood"
c<-grep("^Strong",evet,ignore.case = TRUE)
evet[c]<-"Strong Winds"
c<-grep("^Extreme",evet,ignore.case = TRUE)
evet[c]<-"Extreme Cold/Wind Chill"
evet<-str_trim(evet,side="both") 
temp$EVTYPE<-evet
smallStormData$EVTYPE<-temp$EVTYPE
```
####3.3 Impact on Public Health
In this section, we will be checking the number of fatalities and injuries that are caused by the severe weather events. We will using the first 10 events that caused major deaths across US.

#####3.3.1  Organize data so that it can be combined and aggregated

```r
harmfulevent<-aggregate(smallStormData$FATALITIES,by=list(smallStormData$EVTYPE),sum,na.rm=TRUE)
injury<-aggregate(smallStormData$INJURIES,by=list(smallStormData$EVTYPE),sum,na.rm=TRUE)
names(harmfulevent)<-c("EVTYPE","TotalFatalities")
names(injury)<-c("EVTYPE","TotalInjuries")
harm_10<-harmfulevent[order(-harmfulevent$TotalFatalities),][1:10,]
injury_10<-injury[order(-injury$TotalInjuries),][1:10,]
```

#####3.3.2 Prepare the plots

```r
#PLOT for Deaths
g1<-ggplot(harm_10,aes(x=reorder(EVTYPE,-TotalFatalities),y=TotalFatalities))
g1<-g1+geom_bar(stat = "identity",fill = "blue")+ labs(y="Total Death",x="Event Type")
#beautifying the plot
g1<-g1+theme_bw() +ggtitle("Total Deaths due to event types")+theme(
  plot.title = element_text(color="red", size=12, face="bold.italic"),
  axis.title.x = element_text(color="#993333", size=10, face="bold", hjust = 1),
  axis.title.y = element_text(color="#993333", size=10, face="bold"),
  axis.text = element_text(angle = 90)
)
g1<-g1+ expand_limits(x=c(0,15), y=c(0, 6000))

#PLOT for Injuries
g2<-ggplot(injury_10,aes(x=reorder(EVTYPE,-TotalInjuries),y=TotalInjuries))
g2<-g2+geom_bar(stat = "identity",fill = "green")+ labs(y="Total Injuries",x="Event Type")
#beautifying the plot
g2<-g2+theme_bw() +ggtitle("Total Injuries due to event types")+theme(
  plot.title = element_text(color="red", size=12, face="bold.italic"),
  axis.title.x = element_text(color="#993333", size=10, face="bold", hjust = 1),
  axis.title.y = element_text(color="#993333", size=10, face="bold"),
  axis.text = element_text(angle = 90)
)
g2<-g2+ expand_limits(x=c(0,15), y=c(0, 100000))
```


####3.4 Impact on Economy
In this section, we will be checking the total property and crop loss caused by the severe weather events. We will first convert the property damage and crop damage data into comparable numerical forms using the definition of the units described in the code book (Storm Events). 
We will using the first 10 events that caused major property and crop loss across US.

#####3.4.1 Converting the cost for Property and Crop Damage

```r
#The dataset does not have the numbers for calculating the loss, therefore we used the variable list to construct and convert the values to appropriate numbers in 100s
smallStormData$Propval[smallStormData$PROPDMGEXP == "K"] <- 1000
smallStormData$Propval[smallStormData$PROPDMGEXP == "M"] <- 1e+06
smallStormData$Propval[smallStormData$PROPDMGEXP == ""] <- 1
smallStormData$Propval[smallStormData$PROPDMGEXP == "B"] <- 1e+09
smallStormData$Propval[smallStormData$PROPDMGEXP == "m"] <- 1e+06
smallStormData$Propval[smallStormData$PROPDMGEXP == "0"] <- 1
smallStormData$Propval[smallStormData$PROPDMGEXP == "5"] <- 1e+05
smallStormData$Propval[smallStormData$PROPDMGEXP == "6"] <- 1e+06
smallStormData$Propval[smallStormData$PROPDMGEXP == "4"] <- 10000
smallStormData$Propval[smallStormData$PROPDMGEXP == "2"] <- 100
smallStormData$Propval[smallStormData$PROPDMGEXP == "3"] <- 1000
smallStormData$Propval[smallStormData$PROPDMGEXP == "h"] <- 100
smallStormData$Propval[smallStormData$PROPDMGEXP == "7"] <- 1e+07
smallStormData$Propval[smallStormData$PROPDMGEXP == "H"] <- 100
smallStormData$Propval[smallStormData$PROPDMGEXP == "1"] <- 10
smallStormData$Propval[smallStormData$PROPDMGEXP == "8"] <- 1e+08

smallStormData$Propval[smallStormData$PROPDMGEXP == "+"] <- 0
smallStormData$Propval[smallStormData$PROPDMGEXP == "-"] <- 0
smallStormData$Propval[smallStormData$PROPDMGEXP == "?"] <- 0

#This is give us the totol property loss for each type of event
smallStormData$Propval<-smallStormData$Propval*smallStormData$PROPDMG

#This is give us the totol property loss for each type of event
smallStormData$CROPVAL[smallStormData$CROPDMGEXP == "K"] <- 1000
smallStormData$CROPVAL[smallStormData$CROPDMGEXP == "M"] <- 1e+06
smallStormData$CROPVAL[smallStormData$CROPDMGEXP == ""] <- 1
smallStormData$CROPVAL[smallStormData$CROPDMGEXP == "B"] <- 1e+09
smallStormData$CROPVAL[smallStormData$CROPDMGEXP == "m"] <- 1e+06
smallStormData$CROPVAL[smallStormData$CROPDMGEXP == "0"] <- 1
smallStormData$CROPVAL[smallStormData$CROPDMGEXP == "2"] <- 100
smallStormData$CROPVAL[smallStormData$CROPDMGEXP == "k"] <- 1000
smallStormData$CROPVAL[smallStormData$PROPDMGEXP == "?"] <- 0

smallStormData$CROPVAL<-smallStormData$CROPVAL*smallStormData$CROPDMG
```
#####3.4.2 Organize data so that it can be combined and aggregated

```r
#Gtetting the total count across event type and take the top 10 
propdamage<-aggregate(smallStormData$Propval,by=list(smallStormData$EVTYPE),sum,na.rm=TRUE)
cropdamage<-aggregate(smallStormData$CROPVAL,by=list(smallStormData$EVTYPE),sum,na.rm=TRUE)
names(propdamage)<-c("EVTYPE","TotalLoss")
names(cropdamage)<-c("EVTYPE","TotalLoss")
propdamage_10<-propdamage[order(-propdamage$TotalLoss),][1:10,]
#scaling it with 10^9
propdamage_10$TotalLoss<-propdamage_10$TotalLoss/10^9
cropdamage_10<-cropdamage[order(-cropdamage$TotalLoss),][1:10,]
#scaling it with 10^9
cropdamage_10$TotalLoss<-cropdamage_10$TotalLoss/10^8
```

#####3.4.3 Prepare the plot

```r
propdam<-ggplot(propdamage_10,aes(x=reorder(EVTYPE,-TotalLoss),y=TotalLoss))
propdam<-propdam+geom_bar(stat = "identity",fill = "blue")+ labs(y="Total Loss(Billions $)",x="Event Type")
#beautifying the plot
propdam<-propdam+theme_bw() +ggtitle("Total Property Loss due to event types")+theme(
  plot.title = element_text(color="red", size=12, face="bold.italic"),
  axis.title.x = element_text(color="#993333", size=10, face="bold", hjust = 1),
  axis.title.y = element_text(color="#993333", size=10, face="bold"),
  axis.text = element_text(angle = 90)
)
propdam<-propdam+ expand_limits(x=c(0,15), y=c(0, 200))


cropdam<-ggplot(cropdamage_10,aes(x=reorder(EVTYPE,-TotalLoss),y=TotalLoss))
cropdam<-cropdam+geom_bar(stat = "identity",fill = "green")+ labs(y="Total Loss(Millions $)",x="Event Type")
#beautifying the plot
cropdam<-cropdam+theme_bw() +ggtitle("Total Crop Loss due to event types")+theme(
  plot.title = element_text(color="red", size=12, face="bold.italic"),
  axis.title.x = element_text(color="#993333", size=10, face="bold", hjust = 1),
  axis.title.y = element_text(color="#993333", size=10, face="bold"),
  axis.text = element_text(angle = 90)
)
cropdam<-cropdam+ expand_limits(x=c(0,15), y=c(0, 200))
```
###4 Results
####4.1 Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

```r
grid.arrange(g1,g2,ncol=2)
```

![](Storm_Data_-_Course_Project_2_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
harm_10
```

```
##                     EVTYPE TotalFatalities
## 66                 Tornado            5659
## 28                    Heat            3079
## 22             Flash Flood            1036
## 39         Lakeshore Flood             861
## 65       Thunderstrom Wind             712
## 20 Extreme Cold/Wind Chill             572
## 56             Rip Current             572
## 23                   Flood             484
## 35               High Wind             295
## 4                Avalanche             225
```

```r
injury_10
```

```
##               EVTYPE TotalInjuries
## 66           Tornado         91364
## 65 Thunderstrom Wind          9516
## 28              Heat          9102
## 23             Flood          6795
## 39   Lakeshore Flood          5287
## 38         Ice Storm          2183
## 22       Flash Flood          1802
## 21             Fires          1608
## 35         High Wind          1471
## 27              Hail          1371
```

Tornado caused most Deaths and Injuries

####4.2`Across the United States, which types of events have the greatest economic consequences?
![](Storm_Data_-_Course_Project_2_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```
## [1] "Property Loss"
```

```
##               Eventype Loss in Billions $
## 23               Flood         150.182243
## 36 Hurricane (Typhoon)          85.256410
## 66             Tornado          58.552164
## 60    Storm Surge/Tide          47.974349
## 22         Flash Flood          17.590814
## 27                Hail          15.977560
## 65   Thunderstrom Wind          11.179194
## 21               Fires           8.496628
## 68      Tropical Storm           7.714391
## 74        Winter Storm           6.748997
```

```
## [1] "Crop Loss"
```

```
##                   Eventype Loss in Millions $
## 14                 Drought         139.726218
## 23                   Flood         108.478259
## 36     Hurricane (Typhoon)          55.061178
## 38               Ice Storm          50.221143
## 27                    Hail          30.468876
## 24            Frost/Freeze          19.970610
## 22             Flash Flood          15.321972
## 20 Extreme Cold/Wind Chill          14.317656
## 65       Thunderstrom Wind          12.716640
## 31              Heavy Rain           9.531528
```

Floods and Drought caused major damages to both property and crop
