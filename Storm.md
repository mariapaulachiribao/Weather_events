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

I inserted a new column with the sum of fatalities and injuries.


```r
harmbyevent$TOTAL <- harmbyevent$FATALITIES + harmbyevent$INJURIES
```

I saw the differents types of events, in the column "EVTYPE":


```r
unique(harmbyevent$EVTYPE)
```

```
##   [1] TORNADO                        TSTM WIND                      HAIL                          
##   [4] FREEZING RAIN                  SNOW                           ICE STORM/FLASH FLOOD         
##   [7] SNOW/ICE                       WINTER STORM                   HURRICANE OPAL/HIGH WINDS     
##  [10] THUNDERSTORM WINDS             RECORD COLD                    HURRICANE ERIN                
##  [13] HURRICANE OPAL                 HEAVY RAIN                     LIGHTNING                     
##  [16] THUNDERSTORM WIND              DENSE FOG                      RIP CURRENT                   
##  [19] THUNDERSTORM WINS              FLASH FLOOD                    FLASH FLOODING                
##  [22] HIGH WINDS                     FUNNEL CLOUD                   TORNADO F0                    
##  [25] THUNDERSTORM WINDS LIGHTNING   THUNDERSTORM WINDS/HAIL        HEAT                          
##  [28] WIND                           LIGHTING                       HEAVY RAINS                   
##  [31] LIGHTNING AND HEAVY RAIN       FUNNEL                         WALL CLOUD                    
##  [34] FLOODING                       THUNDERSTORM WINDS HAIL        FLOOD                         
##  [37] COLD                           HEAVY RAIN/LIGHTNING           FLASH FLOODING/THUNDERSTORM WI
##  [40] WALL CLOUD/FUNNEL CLOUD        THUNDERSTORM                   WATERSPOUT                    
##  [43] EXTREME COLD                   HAIL 1.75)                     LIGHTNING/HEAVY RAIN          
##  [46] HIGH WIND                      BLIZZARD                       BLIZZARD WEATHER              
##  [49] WIND CHILL                     BREAKUP FLOODING               HIGH WIND/BLIZZARD            
##  [52] RIVER FLOOD                    HEAVY SNOW                     FREEZE                        
##  [55] COASTAL FLOOD                  HIGH WIND AND HIGH TIDES       HIGH WIND/BLIZZARD/FREEZING RA
##  [58] HIGH TIDES                     HIGH WIND AND HEAVY SNOW       RECORD COLD AND HIGH WIND     
##  [61] RECORD HIGH TEMPERATURE        RECORD HIGH                    HIGH WINDS HEAVY RAINS        
##  [64] HIGH WIND/ BLIZZARD            ICE STORM                      BLIZZARD/HIGH WIND            
##  [67] HIGH WIND/LOW WIND CHILL       HEAVY SNOW/HIGH                RECORD LOW                    
##  [70] HIGH WINDS AND WIND CHILL      HEAVY SNOW/HIGH WINDS/FREEZING LOW TEMPERATURE RECORD        
##  [73] AVALANCHE                      MARINE MISHAP                  WIND CHILL/HIGH WIND          
##  [76] HIGH WIND/WIND CHILL/BLIZZARD  HIGH WIND/WIND CHILL           HIGH WIND/HEAVY SNOW          
##  [79] HIGH TEMPERATURE RECORD        FLOOD WATCH/                   RECORD HIGH TEMPERATURES      
##  [82] HIGH WIND/SEAS                 HIGH WINDS/HEAVY RAIN          HIGH SEAS                     
##  [85] SEVERE TURBULENCE              RECORD RAINFALL                RECORD SNOWFALL               
##  [88] RECORD WARMTH                  HEAVY SNOW/WIND                EXTREME HEAT                  
##  [91] WIND DAMAGE                    DUST STORM                     APACHE COUNTY                 
##  [94] SLEET                          HAIL STORM                     FUNNEL CLOUDS                 
##  [97] FLASH FLOODS                   DUST DEVIL                     EXCESSIVE HEAT                
## [100] THUNDERSTORM WINDS/FUNNEL CLOU WINTER STORM/HIGH WIND         WINTER STORM/HIGH WINDS       
## [103] GUSTY WINDS                    STRONG WINDS                   FLOODING/HEAVY RAIN           
## [106] SNOW AND WIND                  HEAVY SURF COASTAL FLOODING    HEAVY SURF                    
## [109] HEAVY PRECIPATATION            URBAN FLOODING                 HIGH SURF                     
## [112] BLOWING DUST                   URBAN/SMALL                    WILD FIRES                    
## [115] HIGH                           URBAN/SMALL FLOODING           WATER SPOUT                   
## [118] HIGH WINDS DUST STORM          WINTER STORM HIGH WINDS        LOCAL FLOOD                   
## [121] WINTER STORMS                  MUDSLIDES                      RAINSTORM                     
## [124] SEVERE THUNDERSTORM            SEVERE THUNDERSTORMS           SEVERE THUNDERSTORM WINDS     
## [127] THUNDERSTORMS WINDS            DRY MICROBURST                 FLOOD/FLASH FLOOD             
## [130] FLOOD/RAIN/WINDS               WINDS                          DRY MICROBURST 61             
## [133] THUNDERSTORMS                  FLASH FLOOD WINDS              URBAN/SMALL STREAM FLOODING   
## [136] MICROBURST                     STRONG WIND                    HIGH WIND DAMAGE              
## [139] STREAM FLOODING                URBAN AND SMALL                HEAVY SNOWPACK                
## [142] ICE                            FLASH FLOOD/                   DOWNBURST                     
## [145] GUSTNADO AND                   FLOOD/RAIN/WIND                WET MICROBURST                
## [148] DOWNBURST WINDS                DRY MICROBURST WINDS           DRY MIRCOBURST WINDS          
## [151] DRY MICROBURST 53              SMALL STREAM URBAN FLOOD       MICROBURST WINDS              
## [154] HIGH WINDS 57                  DRY MICROBURST 50              HIGH WINDS 66                 
## [157] HIGH WINDS 76                  HIGH WINDS 63                  HIGH WINDS 67                 
## [160] BLIZZARD/HEAVY SNOW            HEAVY SNOW/HIGH WINDS          BLOWING SNOW                  
## [163] HIGH WINDS 82                  HIGH WINDS 80                  HIGH WINDS 58                 
## [166] FREEZING DRIZZLE               LIGHTNING THUNDERSTORM WINDSS  DRY MICROBURST 58             
## [169] HAIL 75                        HIGH WINDS 73                  HIGH WINDS 55                 
## [172] LIGHT SNOW AND SLEET           URBAN FLOOD                    DRY MICROBURST 84             
## [175] THUNDERSTORM WINDS 60          HEAVY RAIN/FLOODING            THUNDERSTORM WINDSS           
## [178] TORNADOS                       GLAZE                          RECORD HEAT                   
## [181] COASTAL FLOODING               HEAT WAVE                      FIRST SNOW                    
## [184] FREEZING RAIN AND SLEET        UNSEASONABLY DRY               UNSEASONABLY WET              
## [187] WINTRY MIX                     WINTER WEATHER                 UNSEASONABLY COLD             
## [190] EXTREME/RECORD COLD            RIP CURRENTS HEAVY SURF        SLEET/RAIN/SNOW               
## [193] UNSEASONABLY WARM              DROUGHT                        NORMAL PRECIPITATION          
## [196] HIGH WINDS/FLOODING            DRY                            RAIN/SNOW                     
## [199] SNOW/RAIN/SLEET                WATERSPOUT/TORNADO             WATERSPOUTS                   
## [202] WATERSPOUT TORNADO             URBAN/SMALL STREAM FLOOD       STORM SURGE                   
## [205] WATERSPOUT-TORNADO             WATERSPOUT-                    TORNADOES, TSTM WIND, HAIL    
## [208] TROPICAL STORM ALBERTO         TROPICAL STORM                 TROPICAL STORM GORDON         
## [211] TROPICAL STORM JERRY           LIGHTNING THUNDERSTORM WINDS   WAYTERSPOUT                   
## [214] MINOR FLOODING                 LIGHTNING INJURY               URBAN/SMALL STREAM  FLOOD     
## [217] LIGHTNING AND THUNDERSTORM WIN THUNDERSTORM WINDS53           URBAN AND SMALL STREAM FLOOD  
## [220] URBAN AND SMALL STREAM         WILDFIRE                       DAMAGING FREEZE               
## [223] THUNDERSTORM WINDS 13          SMALL HAIL                     HEAVY SNOW/HIGH WIND          
## [226] HURRICANE                      WILD/FOREST FIRE               SMALL STREAM FLOODING         
## [229] MUD SLIDE                      LIGNTNING                      FROST                         
## [232] FREEZING RAIN/SNOW             HIGH WINDS/                    THUNDERSNOW                   
## [235] FLOODS                         EXTREME WIND CHILLS            COOL AND WET                  
## [238] HEAVY RAIN/SNOW                SMALL STREAM AND URBAN FLOODIN SMALL STREAM/URBAN FLOOD      
## [241] SNOW/SLEET/FREEZING RAIN       SEVERE COLD                    GLAZE ICE                     
## [244] COLD WAVE                      EARLY SNOW                     SMALL STREAM AND URBAN FLOOD  
## [247] HIGH  WINDS                    RURAL FLOOD                    SMALL STREAM AND              
## [250] MUD SLIDES                     HAIL 80                        EXTREME WIND CHILL            
## [253] COLD AND WET CONDITIONS        EXCESSIVE WETNESS              GRADIENT WINDS                
## [256] HEAVY SNOW/BLOWING SNOW        SLEET/ICE STORM                THUNDERSTORM WINDS URBAN FLOOD
## [259] THUNDERSTORM WINDS SMALL STREA ROTATING WALL CLOUD            LARGE WALL CLOUD              
## [262] COLD AIR FUNNEL                GUSTNADO                       COLD AIR FUNNELS              
## [265] BLOWING SNOW- EXTREME WIND CHI SNOW AND HEAVY SNOW            GROUND BLIZZARD               
## [268] MAJOR FLOOD                    SNOW/HEAVY SNOW                FREEZING RAIN/SLEET           
## [271] ICE JAM FLOODING               SNOW- HIGH WIND- WIND CHILL    STREET FLOOD                  
## [274] COLD AIR TORNADO               SMALL STREAM FLOOD             FOG                           
## [277] THUNDERSTORM WINDS 2           FUNNEL CLOUD/HAIL              ICE/SNOW                      
## [280] TSTM WIND 51                   TSTM WIND 50                   TSTM WIND 52                  
## [283] TSTM WIND 55                   HEAVY SNOW/BLIZZARD            THUNDERSTORM WINDS 61         
## [286] HAIL 0.75                      THUNDERSTORM DAMAGE            THUNDERTORM WINDS             
## [289] HAIL 1.00                      HAIL/WINDS                     SNOW AND ICE                  
## [292] WIND STORM                     SNOWSTORM                      GRASS FIRES                   
## [295] LAKE FLOOD                     PROLONG COLD                   HAIL/WIND                     
## [298] HAIL 1.75                      THUNDERSTORMW 50               WIND/HAIL                     
## [301] SNOW AND ICE STORM             URBAN AND SMALL STREAM FLOODIN THUNDERSTORMS WIND            
## [304] THUNDERSTORM  WINDS            HEAVY SNOW/SLEET               AGRICULTURAL FREEZE           
## [307] DROUGHT/EXCESSIVE HEAT         TUNDERSTORM WIND               TROPICAL STORM DEAN           
## [310] THUNDERTSORM WIND              THUNDERSTORM WINDS/ HAIL       THUNDERSTORM WIND/LIGHTNING   
## [313] HEAVY RAIN/SEVERE WEATHER      THUNDESTORM WINDS              WATERSPOUT/ TORNADO           
## [316] LIGHTNING.                     WARM DRY CONDITIONS            HURRICANE-GENERATED SWELLS    
## [319] HEAVY SNOW/ICE STORM           RIVER AND STREAM FLOOD         HIGH WIND 63                  
## [322] COASTAL SURGE                  HEAVY SNOW AND ICE STORM       MINOR FLOOD                   
## [325] HIGH WINDS/COASTAL FLOOD       RAIN                           RIVER FLOODING                
## [328] SNOW/RAIN                      ICE FLOES                      HIGH WAVES                    
## [331] SNOW SQUALLS                   SNOW SQUALL                    THUNDERSTORM WIND G50         
## [334] LIGHTNING FIRE                 BLIZZARD/FREEZING RAIN         HEAVY LAKE SNOW               
## [337] HEAVY SNOW/FREEZING RAIN       LAKE EFFECT SNOW               HEAVY WET SNOW                
## [340] DUST DEVIL WATERSPOUT          THUNDERSTORM WINDS/HEAVY RAIN  THUNDERSTROM WINDS            
## [343] THUNDERSTORM WINDS      LE CEN HAIL 225                       BLIZZARD AND HEAVY SNOW       
## [346] HEAVY SNOW AND ICE             ICE STORM AND SNOW             HEAVY SNOW ANDBLOWING SNOW    
## [349] HEAVY SNOW/ICE                 BLIZZARD AND EXTREME WIND CHIL LOW WIND CHILL                
## [352] BLOWING SNOW & EXTREME WIND CH WATERSPOUT/                    URBAN/SMALL STREAM            
## [355] TORNADO F3                     FUNNEL CLOUD.                  TORNDAO                       
## [358] HAIL 0.88                      FLOOD/RIVER FLOOD              MUD SLIDES URBAN FLOODING     
## [361] TORNADO F1                     THUNDERSTORM WINDS G           DEEP HAIL                     
## [364] GLAZE/ICE STORM                HEAVY SNOW/WINTER STORM        AVALANCE                      
## [367] BLIZZARD/WINTER STORM          DUST STORM/HIGH WINDS          ICE JAM                       
## [370] FOREST FIRES                   THUNDERSTORM WIND G60          FROST\\FREEZE                 
## [373] THUNDERSTORM WINDS.            HAIL 88                        HAIL 175                      
## [376] HVY RAIN                       HAIL 100                       HAIL 150                      
## [379] HAIL 075                       THUNDERSTORM WIND G55          HAIL 125                      
## [382] THUNDERSTORM WINDS G60         HARD FREEZE                    HAIL 200                      
## [385] THUNDERSTORM WINDS FUNNEL CLOU THUNDERSTORM WINDS 62          WILDFIRES                     
## [388] RECORD HEAT WAVE               HEAVY SNOW AND HIGH WINDS      HEAVY SNOW/HIGH WINDS & FLOOD 
## [391] HAIL FLOODING                  THUNDERSTORM WINDS/FLASH FLOOD HIGH WIND 70                  
## [394] WET SNOW                       HEAVY RAIN AND FLOOD           LOCAL FLASH FLOOD             
## [397] THUNDERSTORM WINDS 53          FLOOD/FLASH FLOODING           TORNADO/WATERSPOUT            
## [400] RAIN AND WIND                  THUNDERSTORM WIND 59           THUNDERSTORM WIND 52          
## [403] COASTAL/TIDAL FLOOD            SNOW/ICE STORM                 BELOW NORMAL PRECIPITATION    
## [406] RIP CURRENTS/HEAVY SURF        FLASH FLOOD/FLOOD              EXCESSIVE RAIN                
## [409] RECORD/EXCESSIVE HEAT          HEAT WAVES                     LIGHT SNOW                    
## [412] THUNDERSTORM WIND 69           HAIL DAMAGE                    LIGHTNING DAMAGE              
## [415] RECORD TEMPERATURES            LIGHTNING AND WINDS            FOG AND COLD TEMPERATURES     
## [418] OTHER                          RECORD SNOW                    SNOW/COLD                     
## [421] FLASH FLOOD FROM ICE JAMS      TSTM WIND G58                  MUDSLIDE                      
## [424] HEAVY SNOW SQUALLS             HEAVY SNOW/SQUALLS             HEAVY SNOW-SQUALLS            
## [427] ICY ROADS                      HEAVY MIX                      SNOW FREEZING RAIN            
## [430] LACK OF SNOW                   SNOW/SLEET                     SNOW/FREEZING RAIN            
## [433] SNOW DROUGHT                   THUNDERSTORMW WINDS            THUNDERSTORM WIND 60 MPH      
## [436] THUNDERSTORM WIND 65MPH        THUNDERSTORM WIND/ TREES       THUNDERSTORM WIND/AWNING      
## [439] THUNDERSTORM WIND 98 MPH       THUNDERSTORM WIND TREES        TORRENTIAL RAIN               
## [442] TORNADO F2                     RIP CURRENTS                   HURRICANE EMILY               
## [445] HURRICANE GORDON               HURRICANE FELIX                THUNDERSTORM WIND 59 MPH      
## [448] THUNDERSTORM WINDS 63 MPH      THUNDERSTORM WIND/ TREE        THUNDERSTORM DAMAGE TO        
## [451] THUNDERSTORM WIND 65 MPH       FLASH FLOOD - HEAVY RAIN       THUNDERSTORM WIND.            
## [454] FLASH FLOOD/ STREET            THUNDERSTORM WIND 59 MPH.      HEAVY SNOW   FREEZING RAIN    
## [457] DAM FAILURE                    THUNDERSTORM HAIL              HAIL 088                      
## [460] THUNDERSTORM WINDSHAIL         LIGHTNING  WAUSEON             THUDERSTORM WINDS             
## [463] ICE AND SNOW                   RECORD COLD/FROST              STORM FORCE WINDS             
## [466] FREEZING RAIN AND SNOW         FREEZING RAIN SLEET AND        SOUTHEAST                     
## [469] HEAVY SNOW & ICE               FREEZING DRIZZLE AND FREEZING  THUNDERSTORM WINDS AND        
## [472] HAIL/ICY ROADS                 FLASH FLOOD/HEAVY RAIN         HEAVY RAIN; URBAN FLOOD WINDS;
## [475] HEAVY PRECIPITATION            TSTM WIND DAMAGE               HIGH WATER                    
## [478] FLOOD FLASH                    RAIN/WIND                      THUNDERSTORM WINDS 50         
## [481] THUNDERSTORM WIND G52          FLOOD FLOOD/FLASH              THUNDERSTORM WINDS 52         
## [484] SNOW SHOWERS                   THUNDERSTORM WIND G51          HEAT WAVE DROUGHT             
## [487] HEAVY SNOW/BLIZZARD/AVALANCHE  RECORD SNOW/COLD               WET WEATHER                   
## [490] UNSEASONABLY WARM AND DRY      FREEZING RAIN SLEET AND LIGHT  RECORD/EXCESSIVE RAINFALL     
## [493] TIDAL FLOOD                    BEACH EROSIN                   THUNDERSTORM WIND G61         
## [496] FLOOD/FLASH                    LOW TEMPERATURE                SLEET & FREEZING RAIN         
## [499] HEAVY RAINS/FLOODING           THUNDERESTORM WINDS            THUNDERSTORM WINDS/FLOODING   
## [502] THUNDEERSTORM WINDS            HIGHWAY FLOODING               THUNDERSTORM W INDS           
## [505] HYPOTHERMIA                    FLASH FLOOD/ FLOOD             THUNDERSTORM WIND 50          
## [508] THUNERSTORM WINDS              HEAVY RAIN/MUDSLIDES/FLOOD     MUD/ROCK SLIDE                
## [511] HIGH WINDS/COLD                BEACH EROSION/COASTAL FLOOD    COLD/WINDS                    
## [514] SNOW/ BITTER COLD              THUNDERSTORM WIND 56           SNOW SLEET                    
## [517] DRY HOT WEATHER                COLD WEATHER                   RAPIDLY RISING WATER          
## [520] HAIL ALOFT                     EARLY FREEZE                   ICE/STRONG WINDS              
## [523] EXTREME WIND CHILL/BLOWING SNO SNOW/HIGH WINDS                HIGH WINDS/SNOW               
## [526] EARLY FROST                    SNOWMELT FLOODING              HEAVY SNOW AND STRONG WINDS   
## [529] SNOW ACCUMULATION              BLOWING SNOW/EXTREME WIND CHIL SNOW/ ICE                     
## [532] SNOW/BLOWING SNOW              TORNADOES                      THUNDERSTORM WIND/HAIL        
## [535] FLASH FLOODING/FLOOD           HAIL 275                       HAIL 450                      
## [538] FLASH FLOOODING                EXCESSIVE RAINFALL             THUNDERSTORMW                 
## [541] HAILSTORM                      TSTM WINDS                     BEACH FLOOD                   
## [544] HAILSTORMS                     TSTMW                          FUNNELS                       
## [547] TSTM WIND 65)                  THUNDERSTORM WINDS/ FLOOD      HEAVY RAINFALL                
## [550] HEAT/DROUGHT                   HEAT DROUGHT                   NEAR RECORD SNOW              
## [553] LANDSLIDE                      HIGH WIND AND SEAS             THUNDERSTORMWINDS             
## [556] THUNDERSTORM WINDS HEAVY RAIN  SLEET/SNOW                     EXCESSIVE                     
## [559] SNOW/SLEET/RAIN                WILD/FOREST FIRES              HEAVY SEAS                    
## [562] DUSTSTORM                      FLOOD & HEAVY RAIN             ?                             
## [565] THUNDERSTROM WIND              FLOOD/FLASHFLOOD               SNOW AND COLD                 
## [568] HOT PATTERN                    PROLONG COLD/SNOW              BRUSH FIRES                   
## [571] SNOW\\COLD                     WINTER MIX                     EXCESSIVE PRECIPITATION       
## [574] SNOWFALL RECORD                HOT/DRY PATTERN                DRY PATTERN                   
## [577] MILD/DRY PATTERN               MILD PATTERN                   LANDSLIDES                    
## [580] HEAVY SHOWERS                  HEAVY SNOW AND                 HIGH WIND 48                  
## [583] LAKE-EFFECT SNOW               BRUSH FIRE                     WATERSPOUT FUNNEL CLOUD       
## [586] URBAN SMALL STREAM FLOOD       SAHARAN DUST                   HEAVY SHOWER                  
## [589] URBAN FLOOD LANDSLIDE          HEAVY SWELLS                   URBAN SMALL                   
## [592] URBAN FLOODS                   SMALL STREAM                   HEAVY RAIN/URBAN FLOOD        
## [595] FLASH FLOOD/LANDSLIDE          LANDSLIDE/URBAN FLOOD          HEAVY RAIN/SMALL STREAM URBAN 
## [598] FLASH FLOOD LANDSLIDES         EXTREME WINDCHILL              URBAN/SML STREAM FLD          
## [601] TSTM WIND/HAIL                 Other                          Record dry month              
## [604] Temperature record             Minor Flooding                 Ice jam flood (minor          
## [607] High Wind                      Tstm Wind                      ROUGH SURF                    
## [610] Wind                           Heavy Surf                     Dust Devil                    
## [613] Wind Damage                    Marine Accident                Snow                          
## [616] Freeze                         Snow Squalls                   Coastal Flooding              
## [619] Heavy Rain                     Strong Wind                    COASTAL STORM                 
## [622] COASTALFLOOD                   Erosion/Cstl Flood             Heavy Rain and Wind           
## [625] Light Snow/Flurries            Wet Month                      Wet Year                      
## [628] Tidal Flooding                 River Flooding                 Damaging Freeze               
## [631] Beach Erosion                  Hot and Dry                    Flood/Flash Flood             
## [634] Icy Roads                      High Surf                      Heavy Rain/High Surf          
## [637] Thunderstorm Wind              Rain Damage                    Unseasonable Cold             
## [640] Early Frost                    Wintry Mix                     blowing snow                  
## [643] STREET FLOODING                Record Cold                    Extreme Cold                  
## [646] Ice Fog                        Excessive Cold                 Torrential Rainfall           
## [649] Freezing Rain                  Landslump                      Late-season Snowfall          
## [652] Hurricane Edouard              Coastal Storm                  Flood                         
## [655] HEAVY RAIN/WIND                TIDAL FLOODING                 Winter Weather                
## [658] Snow squalls                   Strong Winds                   Strong winds                  
## [661] RECORD WARM TEMPS.             Ice/Snow                       Mudslide                      
## [664] Glaze                          Extended Cold                  Snow Accumulation             
## [667] Freezing Fog                   Drifting Snow                  Whirlwind                     
## [670] Heavy snow shower              Heavy rain                     LATE SNOW                     
## [673] Record May Snow                Record Winter Snow             Heavy Precipitation           
## [676]  COASTAL FLOOD                 Record temperature             Light snow                    
## [679] Late Season Snowfall           Gusty Wind                     small hail                    
## [682] Light Snow                     MIXED PRECIP                   Black Ice                     
## [685] Mudslides                      Gradient wind                  Snow and Ice                  
## [688] Freezing Spray                 Summary Jan 17                 Summary of March 14           
## [691] Summary of March 23            Summary of March 24            Summary of April 3rd          
## [694] Summary of April 12            Summary of April 13            Summary of April 21           
## [697] Summary August 11              Summary of April 27            Summary of May 9-10           
## [700] Summary of May 10              Summary of May 13              Summary of May 14             
## [703] Summary of May 22 am           Summary of May 22 pm           Heatburst                     
## [706] Summary of May 26 am           Summary of May 26 pm           Metro Storm, May 26           
## [709] Summary of May 31 am           Summary of May 31 pm           Summary of June 3             
## [712] Summary of June 4              Summary June 5-6               Summary June 6                
## [715] Summary of June 11             Summary of June 12             Summary of June 13            
## [718] Summary of June 15             Summary of June 16             Summary June 18-19            
## [721] Summary of June 23             Summary of June 24             Summary of June 30            
## [724] Summary of July 2              Summary of July 3              Summary of July 11            
## [727] Summary of July 22             Summary July 23-24             Summary of July 26            
## [730] Summary of July 29             Summary of August 1            Summary August 2-3            
## [733] Summary August 7               Summary August 9               Summary August 10             
## [736] Summary August 17              Summary August 21              Summary August 28             
## [739] Summary September 4            Summary September 20           Summary September 23          
## [742] Summary Sept. 25-26            Summary: Oct. 20-21            Summary: October 31           
## [745] Summary: Nov. 6-7              Summary: Nov. 16               Microburst                    
## [748] wet micoburst                  Hail(0.75)                     Funnel Cloud                  
## [751] Urban Flooding                 No Severe Weather              Urban flood                   
## [754] Urban Flood                    Cold                           Summary of May 22             
## [757] Summary of June 6              Summary August 4               Summary of June 10            
## [760] Summary of June 18             Summary September 3            Summary: Sept. 18             
## [763] Coastal Flood                  coastal flooding               Small Hail                    
## [766] Record Temperatures            Light Snowfall                 Freezing Drizzle              
## [769] Gusty wind/rain                GUSTY WIND/HVY RAIN            Blowing Snow                  
## [772] Early snowfall                 Monthly Snowfall               Record Heat                   
## [775] Seasonal Snowfall              Monthly Rainfall               Cold Temperature              
## [778] Sml Stream Fld                 Heat Wave                      MUDSLIDE/LANDSLIDE            
## [781] Saharan Dust                   Volcanic Ash                   Volcanic Ash Plume            
## [784] Thundersnow shower             NONE                           COLD AND SNOW                 
## [787] DAM BREAK                      TSTM WIND (G45)                SLEET/FREEZING RAIN           
## [790] BLACK ICE                      BLOW-OUT TIDES                 UNSEASONABLY COOL             
## [793] TSTM HEAVY RAIN                Gusty Winds                    GUSTY WIND                    
## [796] TSTM WIND 40                   TSTM WIND 45                   TSTM WIND (41)                
## [799] TSTM WIND (G40)                TSTM WND                       Wintry mix                    
## [802]  TSTM WIND                     Frost                          Frost/Freeze                  
## [805] RAIN (HEAVY)                   Record Warmth                  Prolong Cold                  
## [808] Cold and Frost                 URBAN/SML STREAM FLDG          STRONG WIND GUST              
## [811] LATE FREEZE                    BLOW-OUT TIDE                  Hypothermia/Exposure          
## [814] HYPOTHERMIA/EXPOSURE           Lake Effect Snow               Mixed Precipitation           
## [817] Record High                    COASTALSTORM                   Snow and sleet                
## [820] Freezing rain                  Gusty winds                    Blizzard Summary              
## [823] SUMMARY OF MARCH 24-25         SUMMARY OF MARCH 27            SUMMARY OF MARCH 29           
## [826] GRADIENT WIND                  Icestorm/Blizzard              Flood/Strong Wind             
## [829] TSTM WIND AND LIGHTNING        gradient wind                  Freezing drizzle              
## [832] Mountain Snows                 URBAN/SMALL STRM FLDG          Heavy surf and wind           
## [835] Mild and Dry Pattern           COLD AND FROST                 TYPHOON                       
## [838] HIGH SWELLS                    HIGH  SWELLS                   VOLCANIC ASH                  
## [841] DRY SPELL                       LIGHTNING                     BEACH EROSION                 
## [844] UNSEASONAL RAIN                EARLY RAIN                     PROLONGED RAIN                
## [847] WINTERY MIX                    COASTAL FLOODING/EROSION       HOT SPELL                     
## [850] UNSEASONABLY HOT                TSTM WIND (G45)               TSTM WIND  (G45)              
## [853] HIGH WIND (G40)                TSTM WIND (G35)                DRY WEATHER                   
## [856] ABNORMAL WARMTH                UNUSUAL WARMTH                 WAKE LOW WIND                 
## [859] MONTHLY RAINFALL               COLD TEMPERATURES              COLD WIND CHILL TEMPERATURES  
## [862] MODERATE SNOW                  MODERATE SNOWFALL              URBAN/STREET FLOODING         
## [865] COASTAL EROSION                UNUSUAL/RECORD WARMTH          BITTER WIND CHILL             
## [868] BITTER WIND CHILL TEMPERATURES SEICHE                         TSTM                          
## [871] COASTAL  FLOODING/EROSION      UNSEASONABLY WARM YEAR         HYPERTHERMIA/EXPOSURE         
## [874] ROCK SLIDE                     ICE PELLETS                    PATCHY DENSE FOG              
## [877] RECORD COOL                    RECORD WARM                    HOT WEATHER                   
## [880] RECORD TEMPERATURE             TROPICAL DEPRESSION            VOLCANIC ERUPTION             
## [883] COOL SPELL                     WIND ADVISORY                  GUSTY WIND/HAIL               
## [886] RED FLAG FIRE WX               FIRST FROST                    EXCESSIVELY DRY               
## [889] SNOW AND SLEET                 LIGHT SNOW/FREEZING PRECIP     VOG                           
## [892] MONTHLY PRECIPITATION          MONTHLY TEMPERATURE            RECORD DRYNESS                
## [895] EXTREME WINDCHILL TEMPERATURES MIXED PRECIPITATION            DRY CONDITIONS                
## [898] REMNANTS OF FLOYD              EARLY SNOWFALL                 FREEZING FOG                  
## [901] LANDSPOUT                      DRIEST MONTH                   RECORD  COLD                  
## [904] LATE SEASON HAIL               EXCESSIVE SNOW                 DRYNESS                       
## [907] FLOOD/FLASH/FLOOD              WIND AND WAVE                  LIGHT FREEZING RAIN           
## [910]  WIND                          MONTHLY SNOWFALL               RECORD PRECIPITATION          
## [913] ICE ROADS                      ROUGH SEAS                     UNSEASONABLY WARM/WET         
## [916] UNSEASONABLY COOL & WET        UNUSUALLY WARM                 TSTM WIND G45                 
## [919] NON SEVERE HAIL                NON-SEVERE WIND DAMAGE         UNUSUALLY COLD                
## [922] WARM WEATHER                   LANDSLUMP                      THUNDERSTORM WIND (G40)       
## [925] UNSEASONABLY WARM & WET         FLASH FLOOD                   LOCALLY HEAVY RAIN            
## [928] WIND GUSTS                     UNSEASONAL LOW TEMP            HIGH SURF ADVISORY            
## [931] LATE SEASON SNOW               GUSTY LAKE WIND                ABNORMALLY DRY                
## [934] WINTER WEATHER MIX             RED FLAG CRITERIA              WND                           
## [937] CSTL FLOODING/EROSION          SMOKE                           WATERSPOUT                   
## [940] SNOW ADVISORY                  EXTREMELY WET                  UNUSUALLY LATE SNOW           
## [943] VERY DRY                       RECORD LOW RAINFALL            ROGUE WAVE                    
## [946] PROLONG WARMTH                 ACCUMULATED SNOWFALL           FALLING SNOW/ICE              
## [949] DUST DEVEL                     NON-TSTM WIND                  NON TSTM WIND                 
## [952] GUSTY THUNDERSTORM WINDS       PATCHY ICE                     HEAVY RAIN EFFECTS            
## [955] EXCESSIVE HEAT/DROUGHT         NORTHERN LIGHTS                MARINE TSTM WIND              
## [958]    HIGH SURF ADVISORY          HAZARDOUS SURF                 FROST/FREEZE                  
## [961] WINTER WEATHER/MIX             ASTRONOMICAL HIGH TIDE         WHIRLWIND                     
## [964] VERY WARM                      ABNORMALLY WET                 TORNADO DEBRIS                
## [967] EXTREME COLD/WIND CHILL        ICE ON ROAD                    DROWNING                      
## [970] GUSTY THUNDERSTORM WIND        MARINE HAIL                    HIGH SURF ADVISORIES          
## [973] HURRICANE/TYPHOON              HEAVY SURF/HIGH SURF           SLEET STORM                   
## [976] STORM SURGE/TIDE               COLD/WIND CHILL                MARINE HIGH WIND              
## [979] TSUNAMI                        DENSE SMOKE                    LAKESHORE FLOOD               
## [982] MARINE THUNDERSTORM WIND       MARINE STRONG WIND             ASTRONOMICAL LOW TIDE         
## [985] VOLCANIC ASHFALL              
## 985 Levels:    HIGH SURF ADVISORY  COASTAL FLOOD  FLASH FLOOD  LIGHTNING  TSTM WIND  TSTM WIND (G45)  WATERSPOUT  WIND ... WND
```

I could see that the information of the type of event is confusing, with events that seem to be the same but are written slightly differently.

I started to clarify this column:

First I changed all the events to a "uppercase" text and then I removed the leading and trailing whitespaces. Doing that the types of event decreased from 985 to 890.


```r
harmbyevent$EVTYPE <- trimws(toupper(harmbyevent$EVTYPE))
```

I summarized the data by total of harm per event.

```r
totalharmbyevent <- harmbyevent %>%
                summarize(TOTAL=sum(TOTAL)) %>%
                arrange(desc(TOTAL))%>%
                print
```

I selected only the 10 events most harmful.


```r
totalharmbyevent <- totalharmbyevent[1:10,]
totalharmbyevent
```

```
## # A tibble: 10 x 2
##    EVTYPE            TOTAL
##    <fct>             <dbl>
##  1 TORNADO           96979
##  2 EXCESSIVE HEAT     8428
##  3 TSTM WIND          7461
##  4 FLOOD              7259
##  5 LIGHTNING          6046
##  6 HEAT               3037
##  7 FLASH FLOOD        2755
##  8 ICE STORM          2064
##  9 THUNDERSTORM WIND  1621
## 10 WINTER STORM       1527
```

Some events appears to be the same, for example "EXCESSIVE HEAT/HEAT" and "TSTM WIND/THUNDERSTORM WIND".
I ordered the column EVTYPE and I saw that some events were repeated. 


```r
typesofevent <- sort(as.character(unique(harmbyevent$EVTYPE)))
typesofevent
```

```
##   [1] "?"                              "ABNORMAL WARMTH"                "ABNORMALLY DRY"                
##   [4] "ABNORMALLY WET"                 "ACCUMULATED SNOWFALL"           "AGRICULTURAL FREEZE"           
##   [7] "APACHE COUNTY"                  "ASTRONOMICAL HIGH TIDE"         "ASTRONOMICAL LOW TIDE"         
##  [10] "AVALANCE"                       "AVALANCHE"                      "BEACH EROSIN"                  
##  [13] "BEACH EROSION"                  "BEACH EROSION/COASTAL FLOOD"    "BEACH FLOOD"                   
##  [16] "BELOW NORMAL PRECIPITATION"     "BITTER WIND CHILL"              "BITTER WIND CHILL TEMPERATURES"
##  [19] "BLACK ICE"                      "BLIZZARD"                       "BLIZZARD AND EXTREME WIND CHIL"
##  [22] "BLIZZARD AND HEAVY SNOW"        "BLIZZARD SUMMARY"               "BLIZZARD WEATHER"              
##  [25] "BLIZZARD/FREEZING RAIN"         "BLIZZARD/HEAVY SNOW"            "BLIZZARD/HIGH WIND"            
##  [28] "BLIZZARD/WINTER STORM"          "BLOW-OUT TIDE"                  "BLOW-OUT TIDES"                
##  [31] "BLOWING DUST"                   "BLOWING SNOW"                   "BLOWING SNOW- EXTREME WIND CHI"
##  [34] "BLOWING SNOW & EXTREME WIND CH" "BLOWING SNOW/EXTREME WIND CHIL" "BREAKUP FLOODING"              
##  [37] "BRUSH FIRE"                     "BRUSH FIRES"                    "COASTAL  FLOODING/EROSION"     
##  [40] "COASTAL EROSION"                "COASTAL FLOOD"                  "COASTAL FLOODING"              
##  [43] "COASTAL FLOODING/EROSION"       "COASTAL STORM"                  "COASTAL SURGE"                 
##  [46] "COASTAL/TIDAL FLOOD"            "COASTALFLOOD"                   "COASTALSTORM"                  
##  [49] "COLD"                           "COLD AIR FUNNEL"                "COLD AIR FUNNELS"              
##  [52] "COLD AIR TORNADO"               "COLD AND FROST"                 "COLD AND SNOW"                 
##  [55] "COLD AND WET CONDITIONS"        "COLD TEMPERATURE"               "COLD TEMPERATURES"             
##  [58] "COLD WAVE"                      "COLD WEATHER"                   "COLD WIND CHILL TEMPERATURES"  
##  [61] "COLD/WIND CHILL"                "COLD/WINDS"                     "COOL AND WET"                  
##  [64] "COOL SPELL"                     "CSTL FLOODING/EROSION"          "DAM BREAK"                     
##  [67] "DAM FAILURE"                    "DAMAGING FREEZE"                "DEEP HAIL"                     
##  [70] "DENSE FOG"                      "DENSE SMOKE"                    "DOWNBURST"                     
##  [73] "DOWNBURST WINDS"                "DRIEST MONTH"                   "DRIFTING SNOW"                 
##  [76] "DROUGHT"                        "DROUGHT/EXCESSIVE HEAT"         "DROWNING"                      
##  [79] "DRY"                            "DRY CONDITIONS"                 "DRY HOT WEATHER"               
##  [82] "DRY MICROBURST"                 "DRY MICROBURST 50"              "DRY MICROBURST 53"             
##  [85] "DRY MICROBURST 58"              "DRY MICROBURST 61"              "DRY MICROBURST 84"             
##  [88] "DRY MICROBURST WINDS"           "DRY MIRCOBURST WINDS"           "DRY PATTERN"                   
##  [91] "DRY SPELL"                      "DRY WEATHER"                    "DRYNESS"                       
##  [94] "DUST DEVEL"                     "DUST DEVIL"                     "DUST DEVIL WATERSPOUT"         
##  [97] "DUST STORM"                     "DUST STORM/HIGH WINDS"          "DUSTSTORM"                     
## [100] "EARLY FREEZE"                   "EARLY FROST"                    "EARLY RAIN"                    
## [103] "EARLY SNOW"                     "EARLY SNOWFALL"                 "EROSION/CSTL FLOOD"            
## [106] "EXCESSIVE"                      "EXCESSIVE COLD"                 "EXCESSIVE HEAT"                
## [109] "EXCESSIVE HEAT/DROUGHT"         "EXCESSIVE PRECIPITATION"        "EXCESSIVE RAIN"                
## [112] "EXCESSIVE RAINFALL"             "EXCESSIVE SNOW"                 "EXCESSIVE WETNESS"             
## [115] "EXCESSIVELY DRY"                "EXTENDED COLD"                  "EXTREME COLD"                  
## [118] "EXTREME COLD/WIND CHILL"        "EXTREME HEAT"                   "EXTREME WIND CHILL"            
## [121] "EXTREME WIND CHILL/BLOWING SNO" "EXTREME WIND CHILLS"            "EXTREME WINDCHILL"             
## [124] "EXTREME WINDCHILL TEMPERATURES" "EXTREME/RECORD COLD"            "EXTREMELY WET"                 
## [127] "FALLING SNOW/ICE"               "FIRST FROST"                    "FIRST SNOW"                    
## [130] "FLASH FLOOD"                    "FLASH FLOOD - HEAVY RAIN"       "FLASH FLOOD FROM ICE JAMS"     
## [133] "FLASH FLOOD LANDSLIDES"         "FLASH FLOOD WINDS"              "FLASH FLOOD/"                  
## [136] "FLASH FLOOD/ FLOOD"             "FLASH FLOOD/ STREET"            "FLASH FLOOD/FLOOD"             
## [139] "FLASH FLOOD/HEAVY RAIN"         "FLASH FLOOD/LANDSLIDE"          "FLASH FLOODING"                
## [142] "FLASH FLOODING/FLOOD"           "FLASH FLOODING/THUNDERSTORM WI" "FLASH FLOODS"                  
## [145] "FLASH FLOOODING"                "FLOOD"                          "FLOOD & HEAVY RAIN"            
## [148] "FLOOD FLASH"                    "FLOOD FLOOD/FLASH"              "FLOOD WATCH/"                  
## [151] "FLOOD/FLASH"                    "FLOOD/FLASH FLOOD"              "FLOOD/FLASH FLOODING"          
## [154] "FLOOD/FLASH/FLOOD"              "FLOOD/FLASHFLOOD"               "FLOOD/RAIN/WIND"               
## [157] "FLOOD/RAIN/WINDS"               "FLOOD/RIVER FLOOD"              "FLOOD/STRONG WIND"             
## [160] "FLOODING"                       "FLOODING/HEAVY RAIN"            "FLOODS"                        
## [163] "FOG"                            "FOG AND COLD TEMPERATURES"      "FOREST FIRES"                  
## [166] "FREEZE"                         "FREEZING DRIZZLE"               "FREEZING DRIZZLE AND FREEZING" 
## [169] "FREEZING FOG"                   "FREEZING RAIN"                  "FREEZING RAIN AND SLEET"       
## [172] "FREEZING RAIN AND SNOW"         "FREEZING RAIN SLEET AND"        "FREEZING RAIN SLEET AND LIGHT" 
## [175] "FREEZING RAIN/SLEET"            "FREEZING RAIN/SNOW"             "FREEZING SPRAY"                
## [178] "FROST"                          "FROST/FREEZE"                   "FROST\\FREEZE"                 
## [181] "FUNNEL"                         "FUNNEL CLOUD"                   "FUNNEL CLOUD."                 
## [184] "FUNNEL CLOUD/HAIL"              "FUNNEL CLOUDS"                  "FUNNELS"                       
## [187] "GLAZE"                          "GLAZE ICE"                      "GLAZE/ICE STORM"               
## [190] "GRADIENT WIND"                  "GRADIENT WINDS"                 "GRASS FIRES"                   
## [193] "GROUND BLIZZARD"                "GUSTNADO"                       "GUSTNADO AND"                  
## [196] "GUSTY LAKE WIND"                "GUSTY THUNDERSTORM WIND"        "GUSTY THUNDERSTORM WINDS"      
## [199] "GUSTY WIND"                     "GUSTY WIND/HAIL"                "GUSTY WIND/HVY RAIN"           
## [202] "GUSTY WIND/RAIN"                "GUSTY WINDS"                    "HAIL"                          
## [205] "HAIL 0.75"                      "HAIL 0.88"                      "HAIL 075"                      
## [208] "HAIL 088"                       "HAIL 1.00"                      "HAIL 1.75"                     
## [211] "HAIL 1.75)"                     "HAIL 100"                       "HAIL 125"                      
## [214] "HAIL 150"                       "HAIL 175"                       "HAIL 200"                      
## [217] "HAIL 225"                       "HAIL 275"                       "HAIL 450"                      
## [220] "HAIL 75"                        "HAIL 80"                        "HAIL 88"                       
## [223] "HAIL ALOFT"                     "HAIL DAMAGE"                    "HAIL FLOODING"                 
## [226] "HAIL STORM"                     "HAIL(0.75)"                     "HAIL/ICY ROADS"                
## [229] "HAIL/WIND"                      "HAIL/WINDS"                     "HAILSTORM"                     
## [232] "HAILSTORMS"                     "HARD FREEZE"                    "HAZARDOUS SURF"                
## [235] "HEAT"                           "HEAT DROUGHT"                   "HEAT WAVE"                     
## [238] "HEAT WAVE DROUGHT"              "HEAT WAVES"                     "HEAT/DROUGHT"                  
## [241] "HEATBURST"                      "HEAVY LAKE SNOW"                "HEAVY MIX"                     
## [244] "HEAVY PRECIPATATION"            "HEAVY PRECIPITATION"            "HEAVY RAIN"                    
## [247] "HEAVY RAIN AND FLOOD"           "HEAVY RAIN AND WIND"            "HEAVY RAIN EFFECTS"            
## [250] "HEAVY RAIN/FLOODING"            "HEAVY RAIN/HIGH SURF"           "HEAVY RAIN/LIGHTNING"          
## [253] "HEAVY RAIN/MUDSLIDES/FLOOD"     "HEAVY RAIN/SEVERE WEATHER"      "HEAVY RAIN/SMALL STREAM URBAN" 
## [256] "HEAVY RAIN/SNOW"                "HEAVY RAIN/URBAN FLOOD"         "HEAVY RAIN/WIND"               
## [259] "HEAVY RAIN; URBAN FLOOD WINDS;" "HEAVY RAINFALL"                 "HEAVY RAINS"                   
## [262] "HEAVY RAINS/FLOODING"           "HEAVY SEAS"                     "HEAVY SHOWER"                  
## [265] "HEAVY SHOWERS"                  "HEAVY SNOW"                     "HEAVY SNOW-SQUALLS"            
## [268] "HEAVY SNOW   FREEZING RAIN"     "HEAVY SNOW & ICE"               "HEAVY SNOW AND"                
## [271] "HEAVY SNOW AND HIGH WINDS"      "HEAVY SNOW AND ICE"             "HEAVY SNOW AND ICE STORM"      
## [274] "HEAVY SNOW AND STRONG WINDS"    "HEAVY SNOW ANDBLOWING SNOW"     "HEAVY SNOW SHOWER"             
## [277] "HEAVY SNOW SQUALLS"             "HEAVY SNOW/BLIZZARD"            "HEAVY SNOW/BLIZZARD/AVALANCHE" 
## [280] "HEAVY SNOW/BLOWING SNOW"        "HEAVY SNOW/FREEZING RAIN"       "HEAVY SNOW/HIGH"               
## [283] "HEAVY SNOW/HIGH WIND"           "HEAVY SNOW/HIGH WINDS"          "HEAVY SNOW/HIGH WINDS & FLOOD" 
## [286] "HEAVY SNOW/HIGH WINDS/FREEZING" "HEAVY SNOW/ICE"                 "HEAVY SNOW/ICE STORM"          
## [289] "HEAVY SNOW/SLEET"               "HEAVY SNOW/SQUALLS"             "HEAVY SNOW/WIND"               
## [292] "HEAVY SNOW/WINTER STORM"        "HEAVY SNOWPACK"                 "HEAVY SURF"                    
## [295] "HEAVY SURF AND WIND"            "HEAVY SURF COASTAL FLOODING"    "HEAVY SURF/HIGH SURF"          
## [298] "HEAVY SWELLS"                   "HEAVY WET SNOW"                 "HIGH"                          
## [301] "HIGH  SWELLS"                   "HIGH  WINDS"                    "HIGH SEAS"                     
## [304] "HIGH SURF"                      "HIGH SURF ADVISORIES"           "HIGH SURF ADVISORY"            
## [307] "HIGH SWELLS"                    "HIGH TEMPERATURE RECORD"        "HIGH TIDES"                    
## [310] "HIGH WATER"                     "HIGH WAVES"                     "HIGH WIND"                     
## [313] "HIGH WIND (G40)"                "HIGH WIND 48"                   "HIGH WIND 63"                  
## [316] "HIGH WIND 70"                   "HIGH WIND AND HEAVY SNOW"       "HIGH WIND AND HIGH TIDES"      
## [319] "HIGH WIND AND SEAS"             "HIGH WIND DAMAGE"               "HIGH WIND/ BLIZZARD"           
## [322] "HIGH WIND/BLIZZARD"             "HIGH WIND/BLIZZARD/FREEZING RA" "HIGH WIND/HEAVY SNOW"          
## [325] "HIGH WIND/LOW WIND CHILL"       "HIGH WIND/SEAS"                 "HIGH WIND/WIND CHILL"          
## [328] "HIGH WIND/WIND CHILL/BLIZZARD"  "HIGH WINDS"                     "HIGH WINDS 55"                 
## [331] "HIGH WINDS 57"                  "HIGH WINDS 58"                  "HIGH WINDS 63"                 
## [334] "HIGH WINDS 66"                  "HIGH WINDS 67"                  "HIGH WINDS 73"                 
## [337] "HIGH WINDS 76"                  "HIGH WINDS 80"                  "HIGH WINDS 82"                 
## [340] "HIGH WINDS AND WIND CHILL"      "HIGH WINDS DUST STORM"          "HIGH WINDS HEAVY RAINS"        
## [343] "HIGH WINDS/"                    "HIGH WINDS/COASTAL FLOOD"       "HIGH WINDS/COLD"               
## [346] "HIGH WINDS/FLOODING"            "HIGH WINDS/HEAVY RAIN"          "HIGH WINDS/SNOW"               
## [349] "HIGHWAY FLOODING"               "HOT AND DRY"                    "HOT PATTERN"                   
## [352] "HOT SPELL"                      "HOT WEATHER"                    "HOT/DRY PATTERN"               
## [355] "HURRICANE"                      "HURRICANE-GENERATED SWELLS"     "HURRICANE EDOUARD"             
## [358] "HURRICANE EMILY"                "HURRICANE ERIN"                 "HURRICANE FELIX"               
## [361] "HURRICANE GORDON"               "HURRICANE OPAL"                 "HURRICANE OPAL/HIGH WINDS"     
## [364] "HURRICANE/TYPHOON"              "HVY RAIN"                       "HYPERTHERMIA/EXPOSURE"         
## [367] "HYPOTHERMIA"                    "HYPOTHERMIA/EXPOSURE"           "ICE"                           
## [370] "ICE AND SNOW"                   "ICE FLOES"                      "ICE FOG"                       
## [373] "ICE JAM"                        "ICE JAM FLOOD (MINOR"           "ICE JAM FLOODING"              
## [376] "ICE ON ROAD"                    "ICE PELLETS"                    "ICE ROADS"                     
## [379] "ICE STORM"                      "ICE STORM AND SNOW"             "ICE STORM/FLASH FLOOD"         
## [382] "ICE/SNOW"                       "ICE/STRONG WINDS"               "ICESTORM/BLIZZARD"             
## [385] "ICY ROADS"                      "LACK OF SNOW"                   "LAKE-EFFECT SNOW"              
## [388] "LAKE EFFECT SNOW"               "LAKE FLOOD"                     "LAKESHORE FLOOD"               
## [391] "LANDSLIDE"                      "LANDSLIDE/URBAN FLOOD"          "LANDSLIDES"                    
## [394] "LANDSLUMP"                      "LANDSPOUT"                      "LARGE WALL CLOUD"              
## [397] "LATE-SEASON SNOWFALL"           "LATE FREEZE"                    "LATE SEASON HAIL"              
## [400] "LATE SEASON SNOW"               "LATE SEASON SNOWFALL"           "LATE SNOW"                     
## [403] "LIGHT FREEZING RAIN"            "LIGHT SNOW"                     "LIGHT SNOW AND SLEET"          
## [406] "LIGHT SNOW/FLURRIES"            "LIGHT SNOW/FREEZING PRECIP"     "LIGHT SNOWFALL"                
## [409] "LIGHTING"                       "LIGHTNING"                      "LIGHTNING  WAUSEON"            
## [412] "LIGHTNING AND HEAVY RAIN"       "LIGHTNING AND THUNDERSTORM WIN" "LIGHTNING AND WINDS"           
## [415] "LIGHTNING DAMAGE"               "LIGHTNING FIRE"                 "LIGHTNING INJURY"              
## [418] "LIGHTNING THUNDERSTORM WINDS"   "LIGHTNING THUNDERSTORM WINDSS"  "LIGHTNING."                    
## [421] "LIGHTNING/HEAVY RAIN"           "LIGNTNING"                      "LOCAL FLASH FLOOD"             
## [424] "LOCAL FLOOD"                    "LOCALLY HEAVY RAIN"             "LOW TEMPERATURE"               
## [427] "LOW TEMPERATURE RECORD"         "LOW WIND CHILL"                 "MAJOR FLOOD"                   
## [430] "MARINE ACCIDENT"                "MARINE HAIL"                    "MARINE HIGH WIND"              
## [433] "MARINE MISHAP"                  "MARINE STRONG WIND"             "MARINE THUNDERSTORM WIND"      
## [436] "MARINE TSTM WIND"               "METRO STORM, MAY 26"            "MICROBURST"                    
## [439] "MICROBURST WINDS"               "MILD AND DRY PATTERN"           "MILD PATTERN"                  
## [442] "MILD/DRY PATTERN"               "MINOR FLOOD"                    "MINOR FLOODING"                
## [445] "MIXED PRECIP"                   "MIXED PRECIPITATION"            "MODERATE SNOW"                 
## [448] "MODERATE SNOWFALL"              "MONTHLY PRECIPITATION"          "MONTHLY RAINFALL"              
## [451] "MONTHLY SNOWFALL"               "MONTHLY TEMPERATURE"            "MOUNTAIN SNOWS"                
## [454] "MUD SLIDE"                      "MUD SLIDES"                     "MUD SLIDES URBAN FLOODING"     
## [457] "MUD/ROCK SLIDE"                 "MUDSLIDE"                       "MUDSLIDE/LANDSLIDE"            
## [460] "MUDSLIDES"                      "NEAR RECORD SNOW"               "NO SEVERE WEATHER"             
## [463] "NON-SEVERE WIND DAMAGE"         "NON-TSTM WIND"                  "NON SEVERE HAIL"               
## [466] "NON TSTM WIND"                  "NONE"                           "NORMAL PRECIPITATION"          
## [469] "NORTHERN LIGHTS"                "OTHER"                          "PATCHY DENSE FOG"              
## [472] "PATCHY ICE"                     "PROLONG COLD"                   "PROLONG COLD/SNOW"             
## [475] "PROLONG WARMTH"                 "PROLONGED RAIN"                 "RAIN"                          
## [478] "RAIN (HEAVY)"                   "RAIN AND WIND"                  "RAIN DAMAGE"                   
## [481] "RAIN/SNOW"                      "RAIN/WIND"                      "RAINSTORM"                     
## [484] "RAPIDLY RISING WATER"           "RECORD  COLD"                   "RECORD COLD"                   
## [487] "RECORD COLD AND HIGH WIND"      "RECORD COLD/FROST"              "RECORD COOL"                   
## [490] "RECORD DRY MONTH"               "RECORD DRYNESS"                 "RECORD HEAT"                   
## [493] "RECORD HEAT WAVE"               "RECORD HIGH"                    "RECORD HIGH TEMPERATURE"       
## [496] "RECORD HIGH TEMPERATURES"       "RECORD LOW"                     "RECORD LOW RAINFALL"           
## [499] "RECORD MAY SNOW"                "RECORD PRECIPITATION"           "RECORD RAINFALL"               
## [502] "RECORD SNOW"                    "RECORD SNOW/COLD"               "RECORD SNOWFALL"               
## [505] "RECORD TEMPERATURE"             "RECORD TEMPERATURES"            "RECORD WARM"                   
## [508] "RECORD WARM TEMPS."             "RECORD WARMTH"                  "RECORD WINTER SNOW"            
## [511] "RECORD/EXCESSIVE HEAT"          "RECORD/EXCESSIVE RAINFALL"      "RED FLAG CRITERIA"             
## [514] "RED FLAG FIRE WX"               "REMNANTS OF FLOYD"              "RIP CURRENT"                   
## [517] "RIP CURRENTS"                   "RIP CURRENTS HEAVY SURF"        "RIP CURRENTS/HEAVY SURF"       
## [520] "RIVER AND STREAM FLOOD"         "RIVER FLOOD"                    "RIVER FLOODING"                
## [523] "ROCK SLIDE"                     "ROGUE WAVE"                     "ROTATING WALL CLOUD"           
## [526] "ROUGH SEAS"                     "ROUGH SURF"                     "RURAL FLOOD"                   
## [529] "SAHARAN DUST"                   "SEASONAL SNOWFALL"              "SEICHE"                        
## [532] "SEVERE COLD"                    "SEVERE THUNDERSTORM"            "SEVERE THUNDERSTORM WINDS"     
## [535] "SEVERE THUNDERSTORMS"           "SEVERE TURBULENCE"              "SLEET"                         
## [538] "SLEET & FREEZING RAIN"          "SLEET STORM"                    "SLEET/FREEZING RAIN"           
## [541] "SLEET/ICE STORM"                "SLEET/RAIN/SNOW"                "SLEET/SNOW"                    
## [544] "SMALL HAIL"                     "SMALL STREAM"                   "SMALL STREAM AND"              
## [547] "SMALL STREAM AND URBAN FLOOD"   "SMALL STREAM AND URBAN FLOODIN" "SMALL STREAM FLOOD"            
## [550] "SMALL STREAM FLOODING"          "SMALL STREAM URBAN FLOOD"       "SMALL STREAM/URBAN FLOOD"      
## [553] "SML STREAM FLD"                 "SMOKE"                          "SNOW"                          
## [556] "SNOW- HIGH WIND- WIND CHILL"    "SNOW ACCUMULATION"              "SNOW ADVISORY"                 
## [559] "SNOW AND COLD"                  "SNOW AND HEAVY SNOW"            "SNOW AND ICE"                  
## [562] "SNOW AND ICE STORM"             "SNOW AND SLEET"                 "SNOW AND WIND"                 
## [565] "SNOW DROUGHT"                   "SNOW FREEZING RAIN"             "SNOW SHOWERS"                  
## [568] "SNOW SLEET"                     "SNOW SQUALL"                    "SNOW SQUALLS"                  
## [571] "SNOW/ BITTER COLD"              "SNOW/ ICE"                      "SNOW/BLOWING SNOW"             
## [574] "SNOW/COLD"                      "SNOW/FREEZING RAIN"             "SNOW/HEAVY SNOW"               
## [577] "SNOW/HIGH WINDS"                "SNOW/ICE"                       "SNOW/ICE STORM"                
## [580] "SNOW/RAIN"                      "SNOW/RAIN/SLEET"                "SNOW/SLEET"                    
## [583] "SNOW/SLEET/FREEZING RAIN"       "SNOW/SLEET/RAIN"                "SNOW\\COLD"                    
## [586] "SNOWFALL RECORD"                "SNOWMELT FLOODING"              "SNOWSTORM"                     
## [589] "SOUTHEAST"                      "STORM FORCE WINDS"              "STORM SURGE"                   
## [592] "STORM SURGE/TIDE"               "STREAM FLOODING"                "STREET FLOOD"                  
## [595] "STREET FLOODING"                "STRONG WIND"                    "STRONG WIND GUST"              
## [598] "STRONG WINDS"                   "SUMMARY AUGUST 10"              "SUMMARY AUGUST 11"             
## [601] "SUMMARY AUGUST 17"              "SUMMARY AUGUST 2-3"             "SUMMARY AUGUST 21"             
## [604] "SUMMARY AUGUST 28"              "SUMMARY AUGUST 4"               "SUMMARY AUGUST 7"              
## [607] "SUMMARY AUGUST 9"               "SUMMARY JAN 17"                 "SUMMARY JULY 23-24"            
## [610] "SUMMARY JUNE 18-19"             "SUMMARY JUNE 5-6"               "SUMMARY JUNE 6"                
## [613] "SUMMARY OF APRIL 12"            "SUMMARY OF APRIL 13"            "SUMMARY OF APRIL 21"           
## [616] "SUMMARY OF APRIL 27"            "SUMMARY OF APRIL 3RD"           "SUMMARY OF AUGUST 1"           
## [619] "SUMMARY OF JULY 11"             "SUMMARY OF JULY 2"              "SUMMARY OF JULY 22"            
## [622] "SUMMARY OF JULY 26"             "SUMMARY OF JULY 29"             "SUMMARY OF JULY 3"             
## [625] "SUMMARY OF JUNE 10"             "SUMMARY OF JUNE 11"             "SUMMARY OF JUNE 12"            
## [628] "SUMMARY OF JUNE 13"             "SUMMARY OF JUNE 15"             "SUMMARY OF JUNE 16"            
## [631] "SUMMARY OF JUNE 18"             "SUMMARY OF JUNE 23"             "SUMMARY OF JUNE 24"            
## [634] "SUMMARY OF JUNE 3"              "SUMMARY OF JUNE 30"             "SUMMARY OF JUNE 4"             
## [637] "SUMMARY OF JUNE 6"              "SUMMARY OF MARCH 14"            "SUMMARY OF MARCH 23"           
## [640] "SUMMARY OF MARCH 24"            "SUMMARY OF MARCH 24-25"         "SUMMARY OF MARCH 27"           
## [643] "SUMMARY OF MARCH 29"            "SUMMARY OF MAY 10"              "SUMMARY OF MAY 13"             
## [646] "SUMMARY OF MAY 14"              "SUMMARY OF MAY 22"              "SUMMARY OF MAY 22 AM"          
## [649] "SUMMARY OF MAY 22 PM"           "SUMMARY OF MAY 26 AM"           "SUMMARY OF MAY 26 PM"          
## [652] "SUMMARY OF MAY 31 AM"           "SUMMARY OF MAY 31 PM"           "SUMMARY OF MAY 9-10"           
## [655] "SUMMARY SEPT. 25-26"            "SUMMARY SEPTEMBER 20"           "SUMMARY SEPTEMBER 23"          
## [658] "SUMMARY SEPTEMBER 3"            "SUMMARY SEPTEMBER 4"            "SUMMARY: NOV. 16"              
## [661] "SUMMARY: NOV. 6-7"              "SUMMARY: OCT. 20-21"            "SUMMARY: OCTOBER 31"           
## [664] "SUMMARY: SEPT. 18"              "TEMPERATURE RECORD"             "THUDERSTORM WINDS"             
## [667] "THUNDEERSTORM WINDS"            "THUNDERESTORM WINDS"            "THUNDERSNOW"                   
## [670] "THUNDERSNOW SHOWER"             "THUNDERSTORM"                   "THUNDERSTORM  WINDS"           
## [673] "THUNDERSTORM DAMAGE"            "THUNDERSTORM DAMAGE TO"         "THUNDERSTORM HAIL"             
## [676] "THUNDERSTORM W INDS"            "THUNDERSTORM WIND"              "THUNDERSTORM WIND (G40)"       
## [679] "THUNDERSTORM WIND 50"           "THUNDERSTORM WIND 52"           "THUNDERSTORM WIND 56"          
## [682] "THUNDERSTORM WIND 59"           "THUNDERSTORM WIND 59 MPH"       "THUNDERSTORM WIND 59 MPH."     
## [685] "THUNDERSTORM WIND 60 MPH"       "THUNDERSTORM WIND 65 MPH"       "THUNDERSTORM WIND 65MPH"       
## [688] "THUNDERSTORM WIND 69"           "THUNDERSTORM WIND 98 MPH"       "THUNDERSTORM WIND G50"         
## [691] "THUNDERSTORM WIND G51"          "THUNDERSTORM WIND G52"          "THUNDERSTORM WIND G55"         
## [694] "THUNDERSTORM WIND G60"          "THUNDERSTORM WIND G61"          "THUNDERSTORM WIND TREES"       
## [697] "THUNDERSTORM WIND."             "THUNDERSTORM WIND/ TREE"        "THUNDERSTORM WIND/ TREES"      
## [700] "THUNDERSTORM WIND/AWNING"       "THUNDERSTORM WIND/HAIL"         "THUNDERSTORM WIND/LIGHTNING"   
## [703] "THUNDERSTORM WINDS"             "THUNDERSTORM WINDS      LE CEN" "THUNDERSTORM WINDS 13"         
## [706] "THUNDERSTORM WINDS 2"           "THUNDERSTORM WINDS 50"          "THUNDERSTORM WINDS 52"         
## [709] "THUNDERSTORM WINDS 53"          "THUNDERSTORM WINDS 60"          "THUNDERSTORM WINDS 61"         
## [712] "THUNDERSTORM WINDS 62"          "THUNDERSTORM WINDS 63 MPH"      "THUNDERSTORM WINDS AND"        
## [715] "THUNDERSTORM WINDS FUNNEL CLOU" "THUNDERSTORM WINDS G"           "THUNDERSTORM WINDS G60"        
## [718] "THUNDERSTORM WINDS HAIL"        "THUNDERSTORM WINDS HEAVY RAIN"  "THUNDERSTORM WINDS LIGHTNING"  
## [721] "THUNDERSTORM WINDS SMALL STREA" "THUNDERSTORM WINDS URBAN FLOOD" "THUNDERSTORM WINDS."           
## [724] "THUNDERSTORM WINDS/ FLOOD"      "THUNDERSTORM WINDS/ HAIL"       "THUNDERSTORM WINDS/FLASH FLOOD"
## [727] "THUNDERSTORM WINDS/FLOODING"    "THUNDERSTORM WINDS/FUNNEL CLOU" "THUNDERSTORM WINDS/HAIL"       
## [730] "THUNDERSTORM WINDS/HEAVY RAIN"  "THUNDERSTORM WINDS53"           "THUNDERSTORM WINDSHAIL"        
## [733] "THUNDERSTORM WINDSS"            "THUNDERSTORM WINS"              "THUNDERSTORMS"                 
## [736] "THUNDERSTORMS WIND"             "THUNDERSTORMS WINDS"            "THUNDERSTORMW"                 
## [739] "THUNDERSTORMW 50"               "THUNDERSTORMW WINDS"            "THUNDERSTORMWINDS"             
## [742] "THUNDERSTROM WIND"              "THUNDERSTROM WINDS"             "THUNDERTORM WINDS"             
## [745] "THUNDERTSORM WIND"              "THUNDESTORM WINDS"              "THUNERSTORM WINDS"             
## [748] "TIDAL FLOOD"                    "TIDAL FLOODING"                 "TORNADO"                       
## [751] "TORNADO DEBRIS"                 "TORNADO F0"                     "TORNADO F1"                    
## [754] "TORNADO F2"                     "TORNADO F3"                     "TORNADO/WATERSPOUT"            
## [757] "TORNADOES"                      "TORNADOES, TSTM WIND, HAIL"     "TORNADOS"                      
## [760] "TORNDAO"                        "TORRENTIAL RAIN"                "TORRENTIAL RAINFALL"           
## [763] "TROPICAL DEPRESSION"            "TROPICAL STORM"                 "TROPICAL STORM ALBERTO"        
## [766] "TROPICAL STORM DEAN"            "TROPICAL STORM GORDON"          "TROPICAL STORM JERRY"          
## [769] "TSTM"                           "TSTM HEAVY RAIN"                "TSTM WIND"                     
## [772] "TSTM WIND  (G45)"               "TSTM WIND (41)"                 "TSTM WIND (G35)"               
## [775] "TSTM WIND (G40)"                "TSTM WIND (G45)"                "TSTM WIND 40"                  
## [778] "TSTM WIND 45"                   "TSTM WIND 50"                   "TSTM WIND 51"                  
## [781] "TSTM WIND 52"                   "TSTM WIND 55"                   "TSTM WIND 65)"                 
## [784] "TSTM WIND AND LIGHTNING"        "TSTM WIND DAMAGE"               "TSTM WIND G45"                 
## [787] "TSTM WIND G58"                  "TSTM WIND/HAIL"                 "TSTM WINDS"                    
## [790] "TSTM WND"                       "TSTMW"                          "TSUNAMI"                       
## [793] "TUNDERSTORM WIND"               "TYPHOON"                        "UNSEASONABLE COLD"             
## [796] "UNSEASONABLY COLD"              "UNSEASONABLY COOL"              "UNSEASONABLY COOL & WET"       
## [799] "UNSEASONABLY DRY"               "UNSEASONABLY HOT"               "UNSEASONABLY WARM"             
## [802] "UNSEASONABLY WARM & WET"        "UNSEASONABLY WARM AND DRY"      "UNSEASONABLY WARM YEAR"        
## [805] "UNSEASONABLY WARM/WET"          "UNSEASONABLY WET"               "UNSEASONAL LOW TEMP"           
## [808] "UNSEASONAL RAIN"                "UNUSUAL WARMTH"                 "UNUSUAL/RECORD WARMTH"         
## [811] "UNUSUALLY COLD"                 "UNUSUALLY LATE SNOW"            "UNUSUALLY WARM"                
## [814] "URBAN AND SMALL"                "URBAN AND SMALL STREAM"         "URBAN AND SMALL STREAM FLOOD"  
## [817] "URBAN AND SMALL STREAM FLOODIN" "URBAN FLOOD"                    "URBAN FLOOD LANDSLIDE"         
## [820] "URBAN FLOODING"                 "URBAN FLOODS"                   "URBAN SMALL"                   
## [823] "URBAN SMALL STREAM FLOOD"       "URBAN/SMALL"                    "URBAN/SMALL FLOODING"          
## [826] "URBAN/SMALL STREAM"             "URBAN/SMALL STREAM  FLOOD"      "URBAN/SMALL STREAM FLOOD"      
## [829] "URBAN/SMALL STREAM FLOODING"    "URBAN/SMALL STRM FLDG"          "URBAN/SML STREAM FLD"          
## [832] "URBAN/SML STREAM FLDG"          "URBAN/STREET FLOODING"          "VERY DRY"                      
## [835] "VERY WARM"                      "VOG"                            "VOLCANIC ASH"                  
## [838] "VOLCANIC ASH PLUME"             "VOLCANIC ASHFALL"               "VOLCANIC ERUPTION"             
## [841] "WAKE LOW WIND"                  "WALL CLOUD"                     "WALL CLOUD/FUNNEL CLOUD"       
## [844] "WARM DRY CONDITIONS"            "WARM WEATHER"                   "WATER SPOUT"                   
## [847] "WATERSPOUT"                     "WATERSPOUT-"                    "WATERSPOUT-TORNADO"            
## [850] "WATERSPOUT FUNNEL CLOUD"        "WATERSPOUT TORNADO"             "WATERSPOUT/"                   
## [853] "WATERSPOUT/ TORNADO"            "WATERSPOUT/TORNADO"             "WATERSPOUTS"                   
## [856] "WAYTERSPOUT"                    "WET MICOBURST"                  "WET MICROBURST"                
## [859] "WET MONTH"                      "WET SNOW"                       "WET WEATHER"                   
## [862] "WET YEAR"                       "WHIRLWIND"                      "WILD FIRES"                    
## [865] "WILD/FOREST FIRE"               "WILD/FOREST FIRES"              "WILDFIRE"                      
## [868] "WILDFIRES"                      "WIND"                           "WIND ADVISORY"                 
## [871] "WIND AND WAVE"                  "WIND CHILL"                     "WIND CHILL/HIGH WIND"          
## [874] "WIND DAMAGE"                    "WIND GUSTS"                     "WIND STORM"                    
## [877] "WIND/HAIL"                      "WINDS"                          "WINTER MIX"                    
## [880] "WINTER STORM"                   "WINTER STORM HIGH WINDS"        "WINTER STORM/HIGH WIND"        
## [883] "WINTER STORM/HIGH WINDS"        "WINTER STORMS"                  "WINTER WEATHER"                
## [886] "WINTER WEATHER MIX"             "WINTER WEATHER/MIX"             "WINTERY MIX"                   
## [889] "WINTRY MIX"                     "WND"
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
str(unique(harmbyevent$EVTYPE))
```

```
##  chr [1:682] "TORNADO" "TSTM WIND" "HAIL" "FREEZING RAIN" "SNOW" "FLOOD" "SNOW/ICE" "WINTER STORM" "HURRICANE" ...
```

I summarized the data again by total of harm per event, and then I selected only the most 5 harmful events.


```r
finalharmtable <- harmbyevent %>%
        group_by(EVTYPE) %>%
        summarize(TOTAL=sum(TOTAL))%>%
        arrange(desc(TOTAL))
finalharmtable <- finalharmtable[1:5,]
finalharmtable
```

```
## # A tibble: 5 x 2
##   EVTYPE    TOTAL
##   <chr>     <dbl>
## 1 TORNADO   97025
## 2 HEAT      12362
## 3 FLOOD     10129
## 4 TSTM WIND 10080
## 5 LIGHTNING  6046
```

In order to complete the analysis, I created two new tables to see if the same event is also the event with more fatalities and more injuries if we analysed it separately, and not only as a sum of fatalities and injuries.


```r
totalfatalities <- harmbyevent %>%
        group_by(EVTYPE) %>%
        summarize(FATALITIES = sum(FATALITIES)) %>%
        arrange(desc(FATALITIES))
```


```r
totalinjuries <- harmbyevent %>%
        group_by(EVTYPE) %>%
        summarize(INJURIES=sum(INJURIES)) %>%
        arrange(desc(INJURIES))
```

### The second question I want to answer is wich types of events have the greatest economic consequences?

In order to do this first I need only the columns "EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG" and "CROPDMGEXP"


```r
economicimpact <- StormData %>%
        select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
        print
```

I did the same adjustments to the column EVTYPE that I did for question 1 in order to solve some duplicates and spelling mistakes.


```r
economicimpact$EVTYPE <- trimws(toupper(economicimpact$EVTYPE))
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

I analysed what is included in the columns PROPDMGEXP and CROPDMGEXP:


```r
unique(economicimpact$PROPDMGEXP)
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```


```r
unique(economicimpact$CROPDMGEXP)
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
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


```r
economicimpact$CROPDMGEXP <- mgsub(string=as.character(economicimpact$CROPDMGEXP),
                                   pattern = c("?|0","", "2", "B", "k|K", "M|m"),
                                   replacement = c("1","1", "100", "1000000000", "1000", 
                                                   "1000000"))
```

I created 3 new columns:  
- Total of property damage  
- Total of crop damages  
- Total damage (sum of property and crop)  


```r
economicimpact$PROPERTYDAMAGE <- economicimpact$PROPDMG * as.numeric(economicimpact$PROPDMGEXP)
```

```
## Warning: NAs introduced by coercion
```

```r
economicimpact$CROPDAMAGE <- economicimpact$CROPDMG * as.numeric(economicimpact$CROPDMGEXP)
```

```
## Warning: NAs introduced by coercion
```

```r
economicimpact$TOTALDAMAGE <- economicimpact$PROPERTYDAMAGE + economicimpact$CROPDAMAGE
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
totalimpact <- totalimpact [1:5,]
totalimpact
```

```
## # A tibble: 5 x 2
##   EVTYPE    DAMAGE_IN_BUSD
##   <chr>              <dbl>
## 1 FLOOD             158.  
## 2 HURRICANE          44.3 
## 3 TORNADO            16.6 
## 4 HAIL               10.0 
## 5 ICE STORM           5.93
```

In order to complete the analysis, I created two new tables to see if the same event is also the event with more property damage and more crop damage if we analysed it separately, and not only as a sum of property and crop.


```r
totalpropdamage <- economicimpact %>%
        group_by(EVTYPE) %>%
        summarize(PROPERTYDAMAGE_MUSD = sum(PROPERTYDAMAGE, na.rm = TRUE)/1000000) %>%
        arrange(desc(PROPERTYDAMAGE_MUSD))
```


```r
totalcropdamage <- economicimpact %>%
        group_by(EVTYPE) %>%
        summarize(CROPDAMAGE_MUSD = sum(CROPDAMAGE, na.rm = TRUE)/1000000) %>%
        arrange(desc(CROPDAMAGE_MUSD))
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
mtext(text="Event Type", side = 1, line = 9)
mtext(text="Quantity of Fatalities and Injuries", side = 2, line = 4)
axis(2, at=pretty(finalharmtable$TOTAL),
     labels=format(pretty(finalharmtable$TOTAL), scientific=FALSE), las=1)
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)

The most harmful event is the Tornado. If we analyze Injuries and Fatalities separately, the most harmful event is also the Tornado.


```r
head(totalfatalities, 5)
```

```
## # A tibble: 5 x 2
##   EVTYPE    FATALITIES
##   <chr>          <dbl>
## 1 TORNADO         5636
## 2 HEAT            3138
## 3 FLOOD           1525
## 4 LIGHTNING        816
## 5 TSTM WIND        712
```

```r
head(totalinjuries, 5)
```

```
## # A tibble: 5 x 2
##   EVTYPE    INJURIES
##   <chr>        <dbl>
## 1 TORNADO      91389
## 2 TSTM WIND     9368
## 3 HEAT          9224
## 4 FLOOD         8604
## 5 LIGHTNING     5230
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
mtext(text="Event Type", side = 1, line = 9)
mtext(text="Damage in Billions of USD", side = 2, line = 4)
axis(2, at=pretty(totalimpact$DAMAGE_IN_BUSD),
     labels = format(pretty(totalimpact$DAMAGE_IN_BUSD), scientific=FALSE), las=1)
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26-1.png)

The event with the worst economic consequences is Floods. 
If we analyzed the information separately (property damage and crop damage), the result is very interesting. 
In the case of properties the event with the worst economic consequences is flood but in the case of crops is drought.


```r
head(totalpropdamage, 5)
```

```
## # A tibble: 5 x 2
##   EVTYPE      PROPERTYDAMAGE_MUSD
##   <chr>                     <dbl>
## 1 FLOOD                   168212.
## 2 HURRICANE                84756.
## 3 TORNADO                  56999.
## 4 STORM SURGE              47965.
## 5 HAIL                     15735.
```

```r
head(totalcropdamage, 5)
```

```
## # A tibble: 5 x 2
##   EVTYPE    CROPDAMAGE_MUSD
##   <chr>               <dbl>
## 1 DROUGHT            13973.
## 2 FLOOD              12380.
## 3 HURRICANE           5515.
## 4 ICE STORM           5022.
## 5 HAIL                3026.
```

