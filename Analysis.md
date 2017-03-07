# Identification of Severe Weather Events Most Harmful to Population Health and Economy
Keh-Harng Feng  
February 19, 2017  



## Synopsis
Data from the NOAA storm database is analyzed to identify the most harmful severe 
weather events when it comes to population health and economy. Event type 
identifiers from the original dataset is first extensively cleaned up using regular 
expressions. Two indices, the casualty index and ecodamage index are then computed 
to represent the overall harm from a specific type of weather event. COLD SNOW is 
found to be the most dangerous weather event to population health while HURRICANE 
is the most economically damaging.

## Data Processing
The data set used is a csv file prepared by Roger D. Peng using the U.S. National 
Oceanic and Atmospheric Administration storm database.

### Data Ingress

The data is downloaded and load into R as a compressed csv file:

```r
if (!file.exists('StormData.csv.bz2')) {
    download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', 
                  destfile = 'StormData.csv.bz2')
}

if (!('data' %in% ls())) {
    data <- read.csv('StormData.csv.bz2')
}
```

### Event Type Clean-up

A sample of unique storm event types from the data set is listed below

```r
head(sort(unique(data$EVTYPE)), 20)
```

```
##  [1]    HIGH SURF ADVISORY   COASTAL FLOOD          FLASH FLOOD          
##  [4]  LIGHTNING              TSTM WIND              TSTM WIND (G45)      
##  [7]  WATERSPOUT             WIND                  ?                     
## [10] ABNORMAL WARMTH        ABNORMALLY DRY         ABNORMALLY WET        
## [13] ACCUMULATED SNOWFALL   AGRICULTURAL FREEZE    APACHE COUNTY         
## [16] ASTRONOMICAL HIGH TIDE ASTRONOMICAL LOW TIDE  AVALANCE              
## [19] AVALANCHE              BEACH EROSIN          
## 985 Levels:    HIGH SURF ADVISORY  COASTAL FLOOD ... WND
```

Unfortunately the list contains MANY typographical errors. A method is 
devised to identify and correct *some* of them automatically.

An 'cleanup' function is written to first correct many of the common typos 
and group together types that are considered similar. It then gets rid of numbers 
and special characters or phrases like 'AND' in the event type (except for ones 
that are in 'Summary'). It extracts "words" without spaces and other symbols 
(plus the word 'AND') in an event type and convert them to upper cases. The cleaned 
up event type is then regenerated using paste().

```r
# Helper function to 
# 1. Convert to upper case and correct typos.
# 2. Fuzzy group categories.
cleanup <- function(x) {
    x <- toupper(x)
    
    # Typo/redundancy corrections
    # A
    x <- gsub('AVALANCE', 'AVALANCHE', x, fixed = TRUE)
    x <- gsub(' ANDBLOWING', '', x, fixed = TRUE)
    x <- gsub('ABNORMAL.* ', 'EXCESSIVE', x)
    
    # C
    x <- gsub('CSTL', 'COASTAL', x, fixed = TRUE)
    x <- gsub('CLOUDS', 'CLOUD', x, fixed = TRUE)
    
    # D
    x <- gsub('DEVEL', 'DEVIL', x, fixed = TRUE)
    x <- gsub(' DAMAGE.*', '', x)
    
    # E
    x <- gsub('EROSIN', 'EROSION', x, fixed = TRUE)
    x <- gsub('EXTREME', 'EXCESSIVE', x, fixed = TRUE)
    x <- gsub('EXTENDED', 'EXCESSIVE', x, fixed = TRUE)
    x <- gsub(' EFFECTS', '', x, fixed = TRUE)
    
    # F
    x <- gsub('FIRES', 'FIRE', x, fixed = TRUE)
    x <- gsub('FLOODS', 'FLOOD', x, fixed = TRUE)
    x <- gsub('FLOODING', 'FLOOD', x, fixed = TRUE)
    x <- gsub('FLD', 'FLOOD', x, fixed = TRUE)
    x <- gsub('FLDG', 'FLOOD', x, fixed = TRUE)
    x <- gsub('FUNNELS', 'FUNNEL', x, fixed = TRUE)
    
    # H
    x <- gsub('HEAVY', 'EXCESSIVE', x, fixed = TRUE)
    x <- gsub('HIGH', 'EXCESSIVE', x, fixed = TRUE)
    x <- gsub('HVY ', '', x, fixed = TRUE)
    
    # L
    x <- gsub('LIGHTING', 'LIGHTNING', x, fixed = TRUE)
    x <- gsub('LIGNTNING', 'LIGHTNING', x, fixed = TRUE)
    
    # M
    x <- gsub('MIRCO', 'MICRO', x, fixed = TRUE)
    x <- gsub('MPH', '', x, fixed = TRUE)
    x <- gsub('MISHAP', 'ACCIDENT', x, fixed = TRUE)
    
    # R
    x <- gsub('RECORD', 'EXCESSIVE', x, fixed = TRUE)
    
    # S
    x <- gsub('SEVERE', 'EXCESSIVE', x, fixed = TRUE)
    x <- gsub('SPELL', '', x, fixed = TRUE)
    x <- gsub('SML', 'SMALL', x, fixed = TRUE)
    x <- gsub('STRM', 'STREAM', x, fixed = TRUE)
    x <- gsub('STROM', 'STORM', x, fixed = TRUE)
    x <- gsub('SORM', 'STORM', x, fixed = TRUE)
    x <- gsub('STORMS', 'STORM', x, fixed = TRUE)
    x <- gsub('STRONG', 'EXCESSIVE', x, fixed = TRUE)
    x <- gsub(' SEAS[ $]', 'SEA', x)
    
    # T
    x <- gsub('TEMPERATURES', 'TEMPERATURE', x, fixed = TRUE)
    x <- gsub('TEMP$', 'TEMPERATURE', x)
    x <- gsub('TIDES', 'TIDE', x, fixed = TRUE)
    x <- gsub('TORNDAO', 'TORNADO', x, fixed = TRUE)
    x <- gsub('TORNADO.*', 'TORNADO', x)
    x <- gsub('TUNDERSTORM', 'THUNDERSTORM', x, fixed = TRUE)
    x <- gsub('TORRENTIAL', 'EXCESSIVE', x, fixed = TRUE)
    x <- gsub('THUNDERTORM', 'THUNDERSTORM', x, fixed = TRUE)
    x <- gsub('THU.*STORM', 'THUNDERSTORM', x)
    x <- gsub('.*TSTM.*', 'THUNDERSTORM', x)
    
    
    
    # U
    x <- gsub('UNSEASON.* ', 'EXCESSIVE', x)
    x <- gsub('UNUS.* ', 'EXCESSIVE', x)
    
    # V
    x <- gsub('VOG', 'FOG', x, fixed = TRUE)
    x <- gsub('VERY', 'EXCESSIVE', x, fixed = TRUE)
    
    # W
    x <- gsub('WINDS', 'WIND', x, fixed = TRUE)
    x <- gsub('WINDCHILL', 'WIND CHILL', x, fixed = TRUE)
    x <- gsub('WND', 'WIND', x, fixed = TRUE)
    x <- gsub('WAYTER', 'WATER', x, fixed = TRUE)
    x <- gsub('WATER.*SPOUT.*', 'WATERSPOUT', x)
    
    # Category Collapse
    # High Priority: the following are processed first (therefore most likely 
    # will override other checks)
    x <- gsub('.*BLIZZARD.*', 'BLIZZARD', x)
    x <- gsub('^BLOWING SNOW.*', 'EXCESSIVE SNOW', x)
    x <- gsub('.*FOG.*', 'FOG', x)
    x <- gsub('.*FLOOD.*', 'FLOOD', x)
    x <- gsub('.*THUNDERSTORM.*', 'THUNDERSTORM', x)
    x <- gsub('EXCESSIVE SNOW.*', 'EXCESSIVE SNOW', x)
    x <- gsub('LIGHTNING.*', 'LIGHTNING', x)
    x <- gsub('^EXCESSIVE.*SEA', 'ROUGH SEA', x)
    x <- gsub('^EXCESSIVE.*HEAT.*', 'EXCESSIVE HEAT', x)
    
    # Normal priority
    x <- gsub('ACCUMULATED SNOWFALL', 'EXCESSIVE SNOW', x, fixed = TRUE)
    
    x <- gsub('^BEACH EROSION.*', 'BEACH EROSION', x)
    x <- gsub('^BITTER WIND CHILL.*', 'WIND CHILL', x)
    
    
    
    x <- gsub('^COASTAL.*(FLOOD|SURGE).*', 'COASTAL FLOOD', x)
    x <- gsub('COLD WEATHER', 'EXCESSIVE COLD', x, fixed = TRUE)
    x <- gsub('^COLD WIND.*', 'EXCESSIVE COLD', x)
    x <- gsub('^COLD$', 'EXCESSIVE COLD', x, fixed = TRUE)
    
    
    x <- gsub('DAM BREAK', 'DAM FAILURE', x, fixed = TRUE)
    x <- gsub('DAMAGING FREEZE', 'COLD TEMPERATURE', x, fixed = TRUE)
    x <- gsub('.*DOWNBURST.*', 'DOWNBURST', x)
    x <- gsub('^DRY.*', 'DRY', x)
    x <- gsub('DRIEST MONTH', 'DROUGHT', x, fixed = TRUE)
    x <- gsub('^DROUGHT.*', 'DROUGHT', x)
    x <- gsub('^DUST DEVIL.*', 'DUST DEVIL', x)
    
    x <- gsub('^EROSION COASTAL.*', 'COASTAL FLOOD EROSION', x)
    x <- gsub('.*EXCESSIVE.*RAIN.*', 'EXCESSIVE RAIN', x)
    x <- gsub('.*WIND.*CHILL.*', 'EXCESSIVE WIND CHILL', x)
    
    x <- gsub('.*HAIL.*', 'HAIL', x)
    x <- gsub('^HURRICANE.*', 'HURRICANE', x)
    x <- gsub('HEAT DROUGHT', 'DROUGHT', x, fixed = TRUE)
    x <- gsub('^HEAT.*DROUGHT', 'DROUGHT', x)
    x <- gsub('^HEAT .*', 'HEAT', x)
    
    if (grepl('.*ICE STORM.*', x)) {
        x <- 'ICE STORM'
    } else if(grepl('(ICE|ICY)', x)) {
        x <- 'EXCESSIVE ICE'
    }
    
    x <- gsub('LIGHT SNOW.*', 'LIGHT SNOW', x)
    
    
    x <- gsub('MICROBURST.*', 'MICROBURST', x)
    x <- gsub('MUD.*SLIDE.*', 'MUDSLIDE', x)
    
    x <- gsub('^FROST.*', 'FROST', x)
    x <- gsub('FUNNEL CLOUD HAIL', 'HAIL', x, fixed = TRUE)
    
    x <- gsub('^GLAZE.*', 'GLAZE', x)

    x <- gsub('GROUND BLIZZARD', 'BLIZZARD', x, fixed = TRUE)
    x <- gsub('^GUSTNADO.*', 'GUSTNADO', x)
    x <- gsub('^GUSTY.*', 'GUSTY WIND', x)
    
    x <- gsub('^RAIN FLOODING', 'FLOOD', x)
    x <- gsub('^RAIN.*', 'RAIN', x)
    x <- gsub('RAIN', 'EXCESSIVE RAIN', x)
    
    x <- gsub('^STORM .*', 'STORM', x)
    x <- gsub('^SNOW .*', 'SNOW', x)
    x <- gsub('^SNOWCOLD', 'SNOW', x)
    
    x <- gsub('^TROPICAL STORM.*', 'TROPICAL STORM', x)
    x <- gsub('TYPHOON', 'TROPICAL STORM', x, fixed = TRUE)
    
    x <- gsub('^URBAN SMALL STREAM.*', 'FLOOD', x)
    
    x <- gsub('VOLANIC ASH.*', 'VOLCANIC ASH', x)
    
    x <- gsub('WATERSPROUT.*', 'WATERSPROUT', x)
    x <- gsub('^WILD.*FIRE.*', 'WILDFIRE', x)
    x <- gsub('WINTER STORM.*', 'WINTER STORM', x)
    x <- gsub('WINTER WEATHER.*', 'WINTER WEATHER', x)
    
    # Only carry out number eradication on events that are not "Summary"
    if (!grepl('^SUMMARY', x)) {
        x <- gsub('[0-9\\.]', '', x)
    }
    
    x <- gsub('AND', '&', x)
    vec <- strsplit(x, '[ &/\\(\\)]')[[1]]
    x <- paste(vec[sapply(vec, nchar) > 0], collapse = ' ')
    
}

new_data <- data
new_data$EVTYPE <- sapply(new_data$EVTYPE, FUN = cleanup)
```

The event types with automatic cleanup are listed below:

```r
print(unique(sort(new_data$EVTYPE)), quote = TRUE)
```

```
##   [1] "?"                                  
##   [2] "AGRICULTURAL FREEZE"                
##   [3] "APACHE COUNTY"                      
##   [4] "ASTRONOMICAL EXCESSIVE TIDE"        
##   [5] "ASTRONOMICAL LOW TIDE"              
##   [6] "AVALANCHE"                          
##   [7] "BEACH EROSION"                      
##   [8] "BELOW NORMAL PRECIPITATION"         
##   [9] "BLIZZARD"                           
##  [10] "BLOW-OUT TIDE"                      
##  [11] "BLOWING DUST"                       
##  [12] "BRUSH FIRE"                         
##  [13] "COASTAL EROSION"                    
##  [14] "COASTAL FLOOD"                      
##  [15] "COASTAL STORM"                      
##  [16] "COASTALSTORM"                       
##  [17] "COLD"                               
##  [18] "COLD AIR FUNNEL"                    
##  [19] "COLD AIR TORNADO"                   
##  [20] "COLD FROST"                         
##  [21] "COLD SNOW"                          
##  [22] "COLD TEMPERATURE"                   
##  [23] "COLD WAVE"                          
##  [24] "COLD WET CONDITIONS"                
##  [25] "COLD WIND"                          
##  [26] "COOL"                               
##  [27] "COOL WET"                           
##  [28] "DAM FAILURE"                        
##  [29] "DENSE SMOKE"                        
##  [30] "DOWNBURST"                          
##  [31] "DRIFTING SNOW"                      
##  [32] "DROUGHT"                            
##  [33] "DROWNING"                           
##  [34] "DRY"                                
##  [35] "DUST DEVIL"                         
##  [36] "DUST STORM"                         
##  [37] "DUST STORM EXCESSIVE WIND"          
##  [38] "DUSTSTORM"                          
##  [39] "EARLY EXCESSIVE RAIN"               
##  [40] "EARLY FREEZE"                       
##  [41] "EARLY FROST"                        
##  [42] "EARLY SNOW"                         
##  [43] "EARLY SNOWFALL"                     
##  [44] "EXCESSIVE"                          
##  [45] "EXCESSIVE COLD"                     
##  [46] "EXCESSIVE COLD EXCESSIVE WIND"      
##  [47] "EXCESSIVE COLD FROST"               
##  [48] "EXCESSIVE COOL"                     
##  [49] "EXCESSIVE DRY"                      
##  [50] "EXCESSIVE DRY MONTH"                
##  [51] "EXCESSIVE DRYNESS"                  
##  [52] "EXCESSIVE EXCESSIVE"                
##  [53] "EXCESSIVE EXCESSIVE COLD"           
##  [54] "EXCESSIVE EXCESSIVE RAIN"           
##  [55] "EXCESSIVE EXCESSIVE TEMPERATURE"    
##  [56] "EXCESSIVE HEAT"                     
##  [57] "EXCESSIVE ICE"                      
##  [58] "EXCESSIVE LAKE SNOW"                
##  [59] "EXCESSIVE LOW"                      
##  [60] "EXCESSIVE MAY SNOW"                 
##  [61] "EXCESSIVE MIX"                      
##  [62] "EXCESSIVE PRECIPATATION"            
##  [63] "EXCESSIVE PRECIPITATION"            
##  [64] "EXCESSIVE RAIN"                     
##  [65] "EXCESSIVE SHOWER"                   
##  [66] "EXCESSIVE SHOWERS"                  
##  [67] "EXCESSIVE SNOW"                     
##  [68] "EXCESSIVE SURF"                     
##  [69] "EXCESSIVE SURF ADVISORIES"          
##  [70] "EXCESSIVE SURF ADVISORY"            
##  [71] "EXCESSIVE SURF EXCESSIVE SURF"      
##  [72] "EXCESSIVE SURF WIND"                
##  [73] "EXCESSIVE SWELLS"                   
##  [74] "EXCESSIVE TEMPERATURE"              
##  [75] "EXCESSIVE TEMPERATURE EXCESSIVE"    
##  [76] "EXCESSIVE TIDE"                     
##  [77] "EXCESSIVE TURBULENCE"               
##  [78] "EXCESSIVE WARM"                     
##  [79] "EXCESSIVE WARM TEMPS"               
##  [80] "EXCESSIVE WARMTH"                   
##  [81] "EXCESSIVE WATER"                    
##  [82] "EXCESSIVE WAVES"                    
##  [83] "EXCESSIVE WET SNOW"                 
##  [84] "EXCESSIVE WETNESS"                  
##  [85] "EXCESSIVE WIND"                     
##  [86] "EXCESSIVE WIND CHILL"               
##  [87] "EXCESSIVE WIND COLD"                
##  [88] "EXCESSIVE WIND DUST STORM"          
##  [89] "EXCESSIVE WIND EXCESSIVE SNOW"      
##  [90] "EXCESSIVE WIND EXCESSIVE TIDE"      
##  [91] "EXCESSIVE WIND G"                   
##  [92] "EXCESSIVE WIND GUST"                
##  [93] "EXCESSIVE WIND SNOW"                
##  [94] "EXCESSIVE WINTER SNOW"              
##  [95] "EXCESSIVECOLD"                      
##  [96] "EXCESSIVECOOL"                      
##  [97] "EXCESSIVEDRY"                       
##  [98] "EXCESSIVEHOT"                       
##  [99] "EXCESSIVELY DRY"                    
## [100] "EXCESSIVELY WET"                    
## [101] "EXCESSIVESNOW"                      
## [102] "EXCESSIVETEMPERATURE"               
## [103] "EXCESSIVEWARM"                      
## [104] "EXCESSIVEWARM WET"                  
## [105] "EXCESSIVEWARMTH"                    
## [106] "EXCESSIVEWET"                       
## [107] "EXCESSIVEYEAR"                      
## [108] "FIRST FROST"                        
## [109] "FIRST SNOW"                         
## [110] "FLASH FLOOODING"                    
## [111] "FLOOD"                              
## [112] "FOG"                                
## [113] "FOREST FIRE"                        
## [114] "FREEZE"                             
## [115] "FREEZING DRIZZLE"                   
## [116] "FREEZING DRIZZLE FREEZING"          
## [117] "FREEZING EXCESSIVE RAIN"            
## [118] "FREEZING EXCESSIVE RAIN SLEET"      
## [119] "FREEZING EXCESSIVE RAIN SLEET LIGHT"
## [120] "FREEZING EXCESSIVE RAIN SNOW"       
## [121] "FREEZING SPRAY"                     
## [122] "FROST"                              
## [123] "FUNNEL"                             
## [124] "FUNNEL CLOUD"                       
## [125] "GLAZE"                              
## [126] "GRADIENT WIND"                      
## [127] "GRASS FIRE"                         
## [128] "GUSTNADO"                           
## [129] "GUSTY WIND"                         
## [130] "HAIL"                               
## [131] "HARD FREEZE"                        
## [132] "HAZARDOUS SURF"                     
## [133] "HEAT"                               
## [134] "HEATBURST"                          
## [135] "HOT"                                
## [136] "HOT DRY"                            
## [137] "HOT DRY PATTERN"                    
## [138] "HOT PATTERN"                        
## [139] "HOT WEATHER"                        
## [140] "HURRICANE"                          
## [141] "HYPERTHERMIA EXPOSURE"              
## [142] "HYPOTHERMIA"                        
## [143] "HYPOTHERMIA EXPOSURE"               
## [144] "ICE STORM"                          
## [145] "L SLIDE"                            
## [146] "L SLIDES"                           
## [147] "L SLUMP"                            
## [148] "L SPOUT"                            
## [149] "LACK OF SNOW"                       
## [150] "LAKE-EFFECT SNOW"                   
## [151] "LAKE EFFECT SNOW"                   
## [152] "LARGE WALL CLOUD"                   
## [153] "LATE-SEASON SNOWFALL"               
## [154] "LATE FREEZE"                        
## [155] "LATE SEASON SNOW"                   
## [156] "LATE SEASON SNOWFALL"               
## [157] "LATE SNOW"                          
## [158] "LIGHT FREEZING EXCESSIVE RAIN"      
## [159] "LIGHT SNOW"                         
## [160] "LIGHTNING"                          
## [161] "LOW TEMPERATURE"                    
## [162] "LOW TEMPERATURE EXCESSIVE"          
## [163] "MARINE ACCIDENT"                    
## [164] "MARINE EXCESSIVE WIND"              
## [165] "METRO STORM, MAY"                   
## [166] "MICROBURST"                         
## [167] "MILD DRY PATTERN"                   
## [168] "MILD PATTERN"                       
## [169] "MIXED PRECIP"                       
## [170] "MIXED PRECIPITATION"                
## [171] "MODERATE SNOW"                      
## [172] "MODERATE SNOWFALL"                  
## [173] "MONTHLY EXCESSIVE RAINFALL"         
## [174] "MONTHLY PRECIPITATION"              
## [175] "MONTHLY SNOWFALL"                   
## [176] "MONTHLY TEMPERATURE"                
## [177] "MOUNTAIN SNOWS"                     
## [178] "MUDSLIDE"                           
## [179] "NEAR EXCESSIVE SNOW"                
## [180] "NO EXCESSIVE WEATHER"               
## [181] "NON-EXCESSIVE WIND"                 
## [182] "NONE"                               
## [183] "NORMAL PRECIPITATION"               
## [184] "NORTHERN LIGHTS"                    
## [185] "OTHER"                              
## [186] "PROLONG COLD"                       
## [187] "PROLONG COLD SNOW"                  
## [188] "PROLONG WARMTH"                     
## [189] "PROLONGED EXCESSIVE RAIN"           
## [190] "RAPIDLY RISING WATER"               
## [191] "RED FLAG CRITERIA"                  
## [192] "RED FLAG FIRE WX"                   
## [193] "REMNANTS OF FLOYD"                  
## [194] "RIP CURRENT"                        
## [195] "RIP CURRENTS"                       
## [196] "RIP CURRENTS EXCESSIVE SURF"        
## [197] "ROCK SLIDE"                         
## [198] "ROGUE WAVE"                         
## [199] "ROTATING WALL CLOUD"                
## [200] "ROUGH SEAS"                         
## [201] "ROUGH SURF"                         
## [202] "SAHARAN DUST"                       
## [203] "SEASONAL SNOWFALL"                  
## [204] "SEICHE"                             
## [205] "SLEET"                              
## [206] "SLEET EXCESSIVE RAIN SNOW"          
## [207] "SLEET FREEZING EXCESSIVE RAIN"      
## [208] "SLEET SNOW"                         
## [209] "SLEET STORM"                        
## [210] "SMALL STREAM"                       
## [211] "SMOKE"                              
## [212] "SNOW"                               
## [213] "SNOW BITTER COLD"                   
## [214] "SNOW BLOWING SNOW"                  
## [215] "SNOW COLD"                          
## [216] "SNOW EXCESSIVE RAIN"                
## [217] "SNOW EXCESSIVE RAIN SLEET"          
## [218] "SNOW EXCESSIVE SNOW"                
## [219] "SNOW EXCESSIVE WIND"                
## [220] "SNOW FREEZING EXCESSIVE RAIN"       
## [221] "SNOW SLEET"                         
## [222] "SNOW SLEET EXCESSIVE RAIN"          
## [223] "SNOW SLEET FREEZING EXCESSIVE RAIN" 
## [224] "SNOWCOLD"                           
## [225] "SNOWFALL EXCESSIVE"                 
## [226] "SNOWSTORM"                          
## [227] "SOUTHEAST"                          
## [228] "STORM"                              
## [229] "SUMMARY AUGUST 10"                  
## [230] "SUMMARY AUGUST 11"                  
## [231] "SUMMARY AUGUST 17"                  
## [232] "SUMMARY AUGUST 2-3"                 
## [233] "SUMMARY AUGUST 21"                  
## [234] "SUMMARY AUGUST 28"                  
## [235] "SUMMARY AUGUST 4"                   
## [236] "SUMMARY AUGUST 7"                   
## [237] "SUMMARY AUGUST 9"                   
## [238] "SUMMARY JAN 17"                     
## [239] "SUMMARY JULY 23-24"                 
## [240] "SUMMARY JUNE 18-19"                 
## [241] "SUMMARY JUNE 5-6"                   
## [242] "SUMMARY JUNE 6"                     
## [243] "SUMMARY OF APRIL 12"                
## [244] "SUMMARY OF APRIL 13"                
## [245] "SUMMARY OF APRIL 21"                
## [246] "SUMMARY OF APRIL 27"                
## [247] "SUMMARY OF APRIL 3RD"               
## [248] "SUMMARY OF AUGUST 1"                
## [249] "SUMMARY OF JULY 11"                 
## [250] "SUMMARY OF JULY 2"                  
## [251] "SUMMARY OF JULY 22"                 
## [252] "SUMMARY OF JULY 26"                 
## [253] "SUMMARY OF JULY 29"                 
## [254] "SUMMARY OF JULY 3"                  
## [255] "SUMMARY OF JUNE 10"                 
## [256] "SUMMARY OF JUNE 11"                 
## [257] "SUMMARY OF JUNE 12"                 
## [258] "SUMMARY OF JUNE 13"                 
## [259] "SUMMARY OF JUNE 15"                 
## [260] "SUMMARY OF JUNE 16"                 
## [261] "SUMMARY OF JUNE 18"                 
## [262] "SUMMARY OF JUNE 23"                 
## [263] "SUMMARY OF JUNE 24"                 
## [264] "SUMMARY OF JUNE 3"                  
## [265] "SUMMARY OF JUNE 30"                 
## [266] "SUMMARY OF JUNE 4"                  
## [267] "SUMMARY OF JUNE 6"                  
## [268] "SUMMARY OF MARCH 14"                
## [269] "SUMMARY OF MARCH 23"                
## [270] "SUMMARY OF MARCH 24"                
## [271] "SUMMARY OF MARCH 24-25"             
## [272] "SUMMARY OF MARCH 27"                
## [273] "SUMMARY OF MARCH 29"                
## [274] "SUMMARY OF MAY 10"                  
## [275] "SUMMARY OF MAY 13"                  
## [276] "SUMMARY OF MAY 14"                  
## [277] "SUMMARY OF MAY 22"                  
## [278] "SUMMARY OF MAY 22 AM"               
## [279] "SUMMARY OF MAY 22 PM"               
## [280] "SUMMARY OF MAY 26 AM"               
## [281] "SUMMARY OF MAY 26 PM"               
## [282] "SUMMARY OF MAY 31 AM"               
## [283] "SUMMARY OF MAY 31 PM"               
## [284] "SUMMARY OF MAY 9-10"                
## [285] "SUMMARY SEPT. 25-26"                
## [286] "SUMMARY SEPTEMBER 20"               
## [287] "SUMMARY SEPTEMBER 23"               
## [288] "SUMMARY SEPTEMBER 3"                
## [289] "SUMMARY SEPTEMBER 4"                
## [290] "SUMMARY: NOV. 16"                   
## [291] "SUMMARY: NOV. 6-7"                  
## [292] "SUMMARY: OCT. 20-21"                
## [293] "SUMMARY: OCTOBER 31"                
## [294] "SUMMARY: SEPT. 18"                  
## [295] "TEMPERATURE EXCESSIVE"              
## [296] "THUNDERSNOW"                        
## [297] "THUNDERSNOW SHOWER"                 
## [298] "THUNDERSTORM"                       
## [299] "TORNADO"                            
## [300] "TROPICAL DEPRESSION"                
## [301] "TROPICAL STORM"                     
## [302] "TSUNAMI"                            
## [303] "URBAN SMALL"                        
## [304] "URBAN SMALL STREAM"                 
## [305] "VOLCANIC ASH"                       
## [306] "VOLCANIC ASH PLUME"                 
## [307] "VOLCANIC ASHFALL"                   
## [308] "VOLCANIC ERUPTION"                  
## [309] "WAKE LOW WIND"                      
## [310] "WALL CLOUD"                         
## [311] "WALL CLOUD FUNNEL CLOUD"            
## [312] "WARM DRY CONDITIONS"                
## [313] "WARM WEATHER"                       
## [314] "WATERSPOUT"                         
## [315] "WET MICOBURST"                      
## [316] "WET MICROBURST"                     
## [317] "WET MONTH"                          
## [318] "WET SNOW"                           
## [319] "WET WEATHER"                        
## [320] "WET YEAR"                           
## [321] "WHIRLWIND"                          
## [322] "WILDFIRE"                           
## [323] "WIND"                               
## [324] "WIND ADVISORY"                      
## [325] "WIND GUSTS"                         
## [326] "WIND STORM"                         
## [327] "WIND WAVE"                          
## [328] "WINTER MIX"                         
## [329] "WINTER STORM"                       
## [330] "WINTER WEATHER"                     
## [331] "WINTERY MIX"                        
## [332] "WINTRY MIX"
```

### Property Damage Representation

The property damage caused by the severe weather event is represented by both 
numerical values in the 'PROPDMG' variable and a descriptive magnitude unit in 
'PROPDMGEXP' variable. The valid descriptors are K for thousand dollars, M for 
million dollars, B for billion dollars or a number between 0 to 1 for the numerical 
exponent. The following code combines these two variables into one single numerical 
variable 'PROPLOSS' that represents the amount of dollars lost from property damage.

```r
scale <- function(x){
    x <- toupper(x)
    if (x %in% c('K', 'M', 'B', '1', '2', '3', '4', '5', '6', '7', '8', '9')){
        if (x == 'K') {
            return(1000)
        } else if (x == 'M') {
            return(1e6)
        } else if (x == 'B') {
            return(1e9)
        } else {
            return(1*10^(as.numeric(x)))
        }
    } else {
        return(0)
    }
}

PROPLOSS <- new_data$PROPDMG*sapply(new_data$PROPDMGEXP, FUN = scale)
```

A similar process is carried out to combine 'CROPDMG' and 'CROPDMGEXP' into a 
single variable 'CROPLOSS' that represents the amount of dollars lost from crop 
damage.

```r
CROPLOSS <- new_data$CROPDMG*sapply(new_data$CROPDMGEXP, FUN = scale)

new_data <- cbind(new_data[, !(names(new_data) %in% c('CROPDMG', 'CROPDMGEXP', 
                                                    'PROPDMG', 'PROPDMGEXP'))],
                  PROPLOSS, CROPLOSS)
```
## Result

### Most Harmful Event to Health

The most harmful event to personal health is identified by the use of 
**casualty index**. The casualty index (CI) is calculated by doing a weighted sum 
of the averages of the number of injuries and the number of fatalities per 
occurrence of event grouped by severe weather event types stored in the 'EVTYPE' 
variable. 

It is important to compute the casualty index as a weighted sum of averages as 
opposed to just a weighted sum because 

1. A sum will skewer results towards years where the number of recorded 
observations are high.

2. A sum will also skewer results towards event types that happen more frequently.

The weighting assigned for injuries:fatalities is one to two, meaning a fatality 
carries twice as much impact compared to an injury. The code to compute CI is 
included here:

```r
avg <- aggregate(new_data[c('INJURIES', 'FATALITIES')], by = list(new_data$EVTYPE),
                 FUN = mean, na.rm = TRUE)

CI <- cbind(avg[1], avg[2] + avg[3]*2)

names(CI) <- c('EVTYPE', 'CASUALTY_INDEX')
```

A plot of the top 20 events by casualty index is shown below.

```r
perm <- order(CI$CASUALTY_INDEX, decreasing = TRUE)
CI <- CI[perm,]
CI$EVTYPE <- factor(CI$EVTYPE, levels = CI$EVTYPE[perm])

library('ggplot2')

ggplot(CI[1:20,], aes(x = EVTYPE[1:20], y = CASUALTY_INDEX[1:20])) + 
    geom_bar(stat = 'identity') +
    xlab('Weather Event') +
    ylab('Casualty Index') +
    ggtitle('Top 20 Most Harmful Severe Weather Events') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          plot.title = element_text(hjust = 0.5))
```

![](Analysis_files/figure-html/CI_plot-1.png)<!-- -->

From the bar plot it is clear that **COLD SNOW** is the most harmful severe 
weather event when it comes to personal safety.

### Most Economically Damaging

The most economically damaging event is identified by the use of the **ecodamage 
index**, EI. This is just a straight sum of the average amount of monetary loss 
from both property loss and crop loss per severe weather event separated by 
event type. Note that the sum is not weighted, hence both property loss and crop 
loss are considered to have equal economical impact.


```r
avg <- aggregate(new_data[c('PROPLOSS', 'CROPLOSS')], by = list(new_data$EVTYPE),
                 FUN = mean, na.rm = TRUE)

EI <- cbind(avg[1], avg[2] + avg[3])
names(EI) <- c('EVTYPE', 'ECODMG_INDEX')
```

A plot of the top 20 most economically damaging severe weather events is shown 
below.


```r
perm <- order(EI$ECODMG_INDEX, decreasing = TRUE)
EI <- EI[perm,]
EI$EVTYPE <- factor(EI$EVTYPE, levels = EI$EVTYPE[perm])

library('ggplot2')

ggplot(EI[1:20,], aes(x = EVTYPE[1:20], y = ECODMG_INDEX[1:20])) + 
    geom_bar(stat = 'identity') +
    xlab('Weather Event') +
    ylab('Ecodamage Index') +
    ggtitle('Top 20 Most Economically Damaging Severe Weather Events') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          plot.title = element_text(hjust = 0.5))
```

![](Analysis_files/figure-html/EI_plot-1.png)<!-- -->

From the plot it seems **HURRICANE** is by far the most economically damaging 
severe weather event in the U.S..

## Discussion & Future Work

- Event type clean-up is not perfect. Only barely tolerable.

- While the rampant typos and lack of standard representations in the EVTYPE 
variable from the original data necessitates the extensive modifications to the 
event type descriptors, the changes made can be thought of as arbitrary and 
lacking systematic underlying guidelines. Since subsequent analysis uses grouping 
by event types it is crucial to have a logical and standard way of approaching 
EVTYPE corrections. However it may be impossible without further consultations 
with NOAA.

- Although this author believes the choice of weighting (or lack thereof) when it 
comes to the computations of the casualty index and ecodamage index is *reasonable*, 
it is based on simple gut feelings with no scientific research to justify such 
choices. There may very well be better algorithms already developed to represent 
overrall harmfulness or economic damage based on the data provided.
