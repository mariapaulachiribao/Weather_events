# Health and economic impact of weather events in the United States

## Synopsis

In this report I aim to describe wich weather events has the greatest impact in the economy of the US and in the population health.  
To investigate that I obtained a database from the Reproducible Research Course web site. This database tracks characteristics of major storms and weather events in the United States for the period 1950-2011.  
From this data, I found that the event that has caused more deaths and injuries to the population are the Tornados.
In terms of economic impact, the event with worst consequences has been the Floods. In this last analysis if I separate the information between Properties damage and Crops damage I found that the Floods has been the event with more impact in properties but Drought has been the event with the worst impact in crops. I found these last conclusion as the most interesting of my analysis.

## Data Processing

[From the Reproducible Research course web site](https://www.coursera.org/learn/reproducible-research/peer/OMZ37/course-project-2) I obtained the storm database of the NOAA. This database tracks characteristics of major storms and weather events in the United States for the period 1950-2011.


```r
StormData <- read.csv("repdata_data_StormData.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'repdata_data_StormData.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

### The first question I want to answer is which type of event is most harmful with respect to population health. 

In order to do this first I need only the columns "EVTYPE", "FATALITIES", and "INJURIES"


```r
library(dplyr)
```


```r
harmbyevent <- StormData %>%
        select(EVTYPE, FATALITIES, INJURIES) %>%
        group_by(EVTYPE)
```

```
## Error in eval(lhs, parent, parent): object 'StormData' not found
```

I inserted a new column with the sum of fatalities and injuries.


```r
harmbyevent$TOTAL <- harmbyevent$FATALITIES + harmbyevent$INJURIES
```

```
## Error in eval(expr, envir, enclos): object 'harmbyevent' not found
```

I saw the differents types of events, in the column "EVTYPE":


```r
unique(harmbyevent$EVTYPE)
```

```
## Error in unique(harmbyevent$EVTYPE): object 'harmbyevent' not found
```

I could see that the information of the type of event is confusing, with events that seem to be the same but are written slightly differently.

I started to clarify this column:

First I changed all the events to a "uppercase" text and then I removed the leading and trailing whitespaces. Doing that the types of event decreased from 985 to 890.


```r
harmbyevent$EVTYPE <- trimws(toupper(harmbyevent$EVTYPE))
```

```
## Error in toupper(harmbyevent$EVTYPE): object 'harmbyevent' not found
```

I summarized the data by total of harm per event.

```r
totalharmbyevent <- harmbyevent %>%
                summarize(TOTAL=sum(TOTAL)) %>%
                arrange(desc(TOTAL))%>%
                print
```

```
## Error in eval(lhs, parent, parent): object 'harmbyevent' not found
```

I selected only the 10 events most harmful.


```r
totalharmbyevent <- totalharmbyevent[1:10,]
```

```
## Error in eval(expr, envir, enclos): object 'totalharmbyevent' not found
```

```r
totalharmbyevent
```

```
## Error in eval(expr, envir, enclos): object 'totalharmbyevent' not found
```

Some events appears to be the same, for example "EXCESSIVE HEAT/HEAT" and "TSTM WIND/THUNDERSTORM WIND".
I ordered the column EVTYPE and I saw that some events were repeated. 


```r
typesofevent <- sort(as.character(unique(harmbyevent$EVTYPE)))
```

```
## Error in unique(harmbyevent$EVTYPE): object 'harmbyevent' not found
```

```r
typesofevent
```

```
## Error in eval(expr, envir, enclos): object 'typesofevent' not found
```

I adjusted some event's name that I could find that were repeated. Doing that the types of events decreased from 890 to 682.


```r
library(mgsub)
```


```r
harmbyevent$EVTYPE <- mgsub(string=harmbyevent$EVTYPE,
                            pattern=c(".*TORNADO|TORNADOES|TORNDAO.*",".*HEAT.*",
                                      ".*TSTM WIND|TSTM WND|TSTMW|THUNDERSTORM WIND.*",
                                      ".*FLOOD.*", ".*LIGHTNING|LIGNTNING.*",
                                      ".*ICE STORM.*", ".*WINTER STORM.*", ".*HURRICANE.*",
                                      ".*STORM SURGE.*", ".*TROPICAL STORM.*"),
                            replacement = c("TORNADO", "HEAT", "TSTM WIND", "FLOOD",
                                            "LIGHTNING", "ICE STORM", "WINTER STORM",
                                            "HURRICANE", "STORM SURGE", "TROPICAL STORM"))
```

```
## Error in mgsub(string = harmbyevent$EVTYPE, pattern = c(".*TORNADO|TORNADOES|TORNDAO.*", : object 'harmbyevent' not found
```

```r
str(unique(harmbyevent$EVTYPE))
```

```
## Error in unique(harmbyevent$EVTYPE): object 'harmbyevent' not found
```

I summarized the data again by total of harm per event, and then I selected only the most 5 harmful events.


```r
finalharmtable <- harmbyevent %>%
        group_by(EVTYPE) %>%
        summarize(TOTAL=sum(TOTAL))%>%
        arrange(desc(TOTAL))
```

```
## Error in eval(lhs, parent, parent): object 'harmbyevent' not found
```

```r
finalharmtable <- finalharmtable[1:5,]
```

```
## Error in eval(expr, envir, enclos): object 'finalharmtable' not found
```

```r
finalharmtable
```

```
## Error in eval(expr, envir, enclos): object 'finalharmtable' not found
```

In order to complete the analysis, I created two new tables to see if the same event is also the event with more fatalities and more injuries if we analysed it separately, and not only as a sum of fatalities and injuries.


```r
totalfatalities <- harmbyevent %>%
        group_by(EVTYPE) %>%
        summarize(FATALITIES = sum(FATALITIES)) %>%
        arrange(desc(FATALITIES))
```

```
## Error in eval(lhs, parent, parent): object 'harmbyevent' not found
```


```r
totalinjuries <- harmbyevent %>%
        group_by(EVTYPE) %>%
        summarize(INJURIES=sum(INJURIES)) %>%
        arrange(desc(INJURIES))
```

```
## Error in eval(lhs, parent, parent): object 'harmbyevent' not found
```

### The second question I want to answer is wich types of events have the greatest economic consequences?

In order to do this first I need only the columns "EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG" and "CROPDMGEXP"


```r
economicimpact <- StormData %>%
        select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
        print
```

```
## Error in eval(lhs, parent, parent): object 'StormData' not found
```

I did the same adjustments to the column EVTYPE that I did for question 1 in order to solve some duplicates and spelling mistakes.


```r
economicimpact$EVTYPE <- trimws(toupper(economicimpact$EVTYPE))
```

```
## Error in toupper(economicimpact$EVTYPE): object 'economicimpact' not found
```

```r
economicimpact$EVTYPE <- mgsub(string=economicimpact$EVTYPE,
                            pattern=c(".*TORNADO|TORNADOES|TORNDAO.*",".*HEAT.*",
                                      ".*TSTM WIND|TSTM WND|TSTMW|THUNDERSTORM WIND.*",
                                      ".*FLOOD.*", ".*LIGHTNING|LIGNTNING.*",
                                      ".*ICE STORM.*", ".*WINTER STORM.*", ".*HURRICANE.*",
                                      ".*STORM SURGE.*", ".*TROPICAL STORM.*"),
                            replacement = c("TORNADO", "HEAT", "TSTM WIND", "FLOOD",
                                            "LIGHTNING", "ICE STORM", "WINTER STORM",
                                            "HURRICANE", "STORM SURGE", "TROPICAL STORM"))
```

```
## Error in mgsub(string = economicimpact$EVTYPE, pattern = c(".*TORNADO|TORNADOES|TORNDAO.*", : object 'economicimpact' not found
```

I analysed what is included in the columns PROPDMGEXP and CROPDMGEXP:


```r
unique(economicimpact$PROPDMGEXP)
```

```
## Error in unique(economicimpact$PROPDMGEXP): object 'economicimpact' not found
```


```r
unique(economicimpact$CROPDMGEXP)
```

```
## Error in unique(economicimpact$CROPDMGEXP): object 'economicimpact' not found
```

These columns include the exponent for the number in the columns PROPDMG and CROPDMG. I substituted this code for the number that corresponds in each case:


```r
economicimpact$PROPDMGEXP <- mgsub(string=as.character(economicimpact$PROPDMGEXP),
                                   pattern = c(".*-|?|+|0.*","", "1", "2|h|H", "3|k|K",
                                               "4", "5", "6|M|m", "7", "8", "B"),
                                   replacement = c("1","1", "10", "100", "1000", "10000",
                                                   "100000", "1000000", "10000000",
                                                   "100000000", "1000000000"))
```

```
## Error in mgsub(string = as.character(economicimpact$PROPDMGEXP), pattern = c(".*-|?|+|0.*", : object 'economicimpact' not found
```


```r
economicimpact$CROPDMGEXP <- mgsub(string=as.character(economicimpact$CROPDMGEXP),
                                   pattern = c("?|0","", "2", "B", "k|K", "M|m"),
                                   replacement = c("1","1", "100", "1000000000", "1000", 
                                                   "1000000"))
```

```
## Error in mgsub(string = as.character(economicimpact$CROPDMGEXP), pattern = c("?|0", : object 'economicimpact' not found
```

I created 3 new columns:  
- Total of property damage  
- Total of crop damages  
- Total damage (sum of property and crop)  


```r
economicimpact$PROPERTYDAMAGE <- economicimpact$PROPDMG * as.numeric(economicimpact$PROPDMGEXP)
```

```
## Error in eval(expr, envir, enclos): object 'economicimpact' not found
```

```r
economicimpact$CROPDAMAGE <- economicimpact$CROPDMG * as.numeric(economicimpact$CROPDMGEXP)
```

```
## Error in eval(expr, envir, enclos): object 'economicimpact' not found
```

```r
economicimpact$TOTALDAMAGE <- economicimpact$PROPERTYDAMAGE + economicimpact$CROPDAMAGE
```

```
## Error in eval(expr, envir, enclos): object 'economicimpact' not found
```

I summarized the data by total of economic impact, and then I selected the 5 events with more impact.
I show the information in Billions of dollars to make it more readable.


```r
totalimpact <- economicimpact %>%
        group_by(EVTYPE) %>%
        summarize(TOTALDAMAGE=sum(TOTALDAMAGE, na.rm = TRUE))%>%
        arrange(desc(TOTALDAMAGE))%>%
        mutate(TOTALDAMAGE = TOTALDAMAGE/1000000000)%>%
        rename(DAMAGE_IN_BUSD=TOTALDAMAGE)
```

```
## Error in eval(lhs, parent, parent): object 'economicimpact' not found
```

```r
totalimpact <- totalimpact [1:5,]
```

```
## Error in eval(expr, envir, enclos): object 'totalimpact' not found
```

```r
totalimpact
```

```
## Error in eval(expr, envir, enclos): object 'totalimpact' not found
```

In order to complete the analysis, I created two new tables to see if the same event is also the event with more property damage and more crop damage if we analysed it separately, and not only as a sum of property and crop.


```r
totalpropdamage <- economicimpact %>%
        group_by(EVTYPE) %>%
        summarize(PROPERTYDAMAGE_MUSD = sum(PROPERTYDAMAGE, na.rm = TRUE)/1000000) %>%
        arrange(desc(PROPERTYDAMAGE_MUSD))
```

```
## Error in eval(lhs, parent, parent): object 'economicimpact' not found
```


```r
totalcropdamage <- economicimpact %>%
        group_by(EVTYPE) %>%
        summarize(CROPDAMAGE_MUSD = sum(CROPDAMAGE, na.rm = TRUE)/1000000) %>%
        arrange(desc(CROPDAMAGE_MUSD))
```

```
## Error in eval(lhs, parent, parent): object 'economicimpact' not found
```

## Results

### Which type of event is most harmful with respect to population health?


```r
par(mar=c(11, 7, 4, 4))
with(finalharmtable, barplot(height = TOTAL, 
                               col= "blue", 
                               main = "Event type most harmful",
                               yaxt="n",
                               xlab="", ylab="",
                               ylim= c(0,100000),
                               names.arg = EVTYPE,
                               las = 2))
```

```
## Error in with(finalharmtable, barplot(height = TOTAL, col = "blue", main = "Event type most harmful", : object 'finalharmtable' not found
```

```r
mtext(text="Event Type", side = 1, line = 9)
```

```
## Error in mtext(text = "Event Type", side = 1, line = 9): plot.new has not been called yet
```

```r
mtext(text="Quantity of Fatalities and Injuries", side = 2, line = 4)
```

```
## Error in mtext(text = "Quantity of Fatalities and Injuries", side = 2, : plot.new has not been called yet
```

```r
axis(2, at=pretty(finalharmtable$TOTAL),
     labels=format(pretty(finalharmtable$TOTAL), scientific=FALSE), las=1)
```

```
## Error in pretty(finalharmtable$TOTAL): object 'finalharmtable' not found
```

The most harmful event is the Tornado. If we analyze Injuries and Fatalities separately, the most harmful event is also the Tornado.


```r
head(totalfatalities, 5)
```

```
## Error in head(totalfatalities, 5): object 'totalfatalities' not found
```

```r
head(totalinjuries, 5)
```

```
## Error in head(totalinjuries, 5): object 'totalinjuries' not found
```

### Which type of event has the greatest economic consequences?


```r
par(mar=c(9, 7, 4, 4))
with(totalimpact, barplot(height = DAMAGE_IN_BUSD, 
                               col= "blue", 
                               main = "Economic consequences by type of event",
                               yaxt="n",
                               xlab="", ylab="",
                               ylim= c(0,160),
                               names.arg = EVTYPE,
                               las = 2))
```

```
## Error in with(totalimpact, barplot(height = DAMAGE_IN_BUSD, col = "blue", : object 'totalimpact' not found
```

```r
mtext(text="Event Type", side = 1, line = 9)
```

```
## Error in mtext(text = "Event Type", side = 1, line = 9): plot.new has not been called yet
```

```r
mtext(text="Damage in Billions of USD", side = 2, line = 4)
```

```
## Error in mtext(text = "Damage in Billions of USD", side = 2, line = 4): plot.new has not been called yet
```

```r
axis(2, at=pretty(totalimpact$DAMAGE_IN_BUSD),
     labels = format(pretty(totalimpact$DAMAGE_IN_BUSD), scientific=FALSE), las=1)
```

```
## Error in pretty(totalimpact$DAMAGE_IN_BUSD): object 'totalimpact' not found
```

The event with the worst economic consequences is Floods. 
If we analyzed the information separately (property damage and crop damage), the result is very interesting. 
In the case of properties the event with the worst economic consequences is flood but in the case of crops is drought.


```r
head(totalpropdamage, 5)
```

```
## Error in head(totalpropdamage, 5): object 'totalpropdamage' not found
```

```r
head(totalcropdamage, 5)
```

```
## Error in head(totalcropdamage, 5): object 'totalcropdamage' not found
```

