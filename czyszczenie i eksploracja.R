# 1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful
#    with respect to population health?
#    
# 2. Across the United States, which types of events have the greatest economic consequences?
# 
library(lubridate)
library(dplyr)
library(stringdist)
library(ggplot2)
library(gridExtra)

sd <- read.csv("StormData.csv.bz2", stringsAsFactors = F)
sdcpy <- sd
sd <- sdcpy
names(sd)
sd <- select(sd, EVTYPE, BGN_DATE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, REMARKS)
sd$BGN_DATE <- as.Date(sd$BGN_DATE, "%m/%d/%Y %T")

# Assumed definitions:
#   harmful - having more than 0 FATALITIES or having more than 0 INJURIES
#   having econimic consequences - having greater than 0 PROPDMG or CROPDMG
#   "most" = ?

# Event type distribution. Input: http://www.ncdc.noaa.gov/stormevents/details.jsp
#
length(unique(sd$EVTYPE[year(sd$BGN_DATE) <= 1954])) # 1
unique(sd$EVTYPE[year(sd$BGN_DATE) <= 1954])
# [1] "TORNADO"
length(unique(sd$EVTYPE[year(sd$BGN_DATE) > 1954 & year(sd$BGN_DATE) <= 1992])) # 3
unique(sd$EVTYPE[year(sd$BGN_DATE) > 1954 & year(sd$BGN_DATE) <= 1992])
# [1] "TORNADO"   "TSTM WIND" "HAIL"
length(unique(sd$EVTYPE[year(sd$BGN_DATE) > 1992 & year(sd$BGN_DATE) <= 1995])) # 598
length(unique(sd$EVTYPE[year(sd$BGN_DATE) > 1995])) # 516
#
uniqueevs <- sapply(1995:2011, function(x) length(unique(sd$EVTYPE[year(sd$BGN_DATE) > x])))
barplot(uniqueevs, names.arg=1995:2011, las = 3, main = "Unique EVTYPEs reported per year")
#
# The approach is to consider all the years covered by the database. When the principal
# contributors to the questions are identified and those contain the three events registered
# between 1950 and 1992, a check will be made to establish the magnitude of the possible
# bias by comparign the overall results to the results from the period 1992-2011.
###########
########### Zrobić tu analizę udziału powyższych 4 eventów w całości i wyciągnąć wnioski od razu.
########### Potem się skupić tylko na latach 1992-2011.


# PROP & CROP

range(sd$PROPDMG)    # 0-5000
summary(sd$PROPDMG)
sum(sd$PROPDMG)      # 10884500
sum(sd$PROPDMG > 0)  # 239174 events

range(sd$CROPDMG)    # 0-990
summary(sd$CROPDMG)
sum(sd$CROPDMG)      # 1377827
sum(sd$CROPDMG > 0)  # 22099 events

# PROPDMGEXP multiplier cleanup
# 
sd$PROPDMGEXP <- toupper(sd$PROPDMGEXP)
exp <- unique(sd$PROPDMGEXP)
exp <- exp[order(exp)]
#   [1] ""  "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "H" "K" "M"

# We know the histogram of number of observations vs. year, so let's check the span of exponents in years...
# 
sapply(exp, function(x) range(year(sd$BGN_DATE[sd$PROPDMGEXP == x])))
#              -    ?    +    0    1    2    3    4    5    6    7    8    B    H    K    M
# [1,] 1955 1995 1993 1994 1993 1995 1995 1995 1994 1994 1993 1995 1995 1993 1994 1950 1950
# [2,] 2006 1995 1995 1995 2011 1995 1995 1995 1995 1995 1995 1995 1995 2011 1995 2011 2011
#
# No particular hint apart from the fact that most of the numeric exponents are from mid-90s

# How many observations are for each exponent?
# 
sapply(exp, function(x) sum(sd$PROPDMGEXP == x))
#             -      ?      +      0      1      2      3      4      5      6      7      8      B      H      K      M 
# 465934      1      8      5    216     25     13      4      4     28      4      5      1     40      7 424665  11337 

# How many observations with non-zero PROPDMG are for each exponent?
# 
sapply(exp, function(x) sum(sd$PROPDMGEXP == x & sd$PROPDMG != 0))
#             -      ?      +      0      1      2      3      4      5      6      7      8      B      H      K      M 
#     76      1      0      5    209      0      1      1      4     18      3      2      0     40      7 227481  11326 

# What is the "raw" sum of PROPDMG for each exponen?
# 
sapply(exp, function(x) sum(sd$PROPDMG[sd$PROPDMGEXP == x]))
#                  -           ?           +           0           1           2           3           4           5           6 
# 527.41       15.00        0.00      117.00     7108.30        0.00       12.00       20.00       14.50      210.50       65.00 
#      7           8           B           H           K           M 
#  82.00        0.00      275.85       27.00 10735292.10   140733.35 

# Pomysł na wyczyszczenie PROPDMGEXP (ignoring effects of inflation):
# - H, K, M, B are converted to uppercase and applied accordingly (10e2/10e3/10e6/10e9)
# - exponents are ignored if there are no non-zero PROPDMG entries for them (i.e. ?, 1, 8)
# - for exponents +, - the clues are sought in REMARKS (13 events)
# - for remaining numeric exponents the clues are sought in REMARKS, and if not found they are assumed to be exponents of the base
#   of 10. The overall error/contribution is calculated as to the total of PROPDMG for 3 cases: 1) ignoring PROPDMG values for those
#   exponents, 2) assuming they are base 10 exponents, 3) taking raw PROPDMG values ignoring the exponent
# - for "" exponent the clues are sought in REMARKS
# Clues in remarks are strings dollar/USD/$. No translation of descriptive damages to cash according to the NWS instruction is performed
# due to to much time cost associated.

sapply(exp, function(x) summary(sd$PROPDMG[sd$PROPDMGEXP == x & sd$PROPDMG > 0]))
#                -   ?    +      0   1  2  3      4     5     6    7   8       B     H       K      M
# Min.     0.41 15  NA  2.0   0.10  NA 12 20  0.100  0.10 15.00 14.0  NA   0.100 2.000    0.01   0.01
# 1st Qu.  3.00 15  NA 15.0  10.00  NA 12 20  1.675  1.00 19.50 27.5  NA   1.450 2.500    2.50   1.50
# Median   5.00 15  NA 20.0  30.00  NA 12 20  2.200  5.80 24.00 41.0  NA   2.300 5.000   10.00   2.50
# Mean     6.94 15 NaN 23.4  34.01 NaN 12 20  3.625 11.69 21.67 41.0 NaN   6.896 3.857   47.19  12.43
# 3rd Qu.  7.25 15  NA 20.0  50.00  NA 12 20  4.150 12.00 25.00 54.5  NA   5.000 5.000   30.00   6.00
# Max.    75.00 15  NA 60.0 150.00  NA 12 20 10.000 88.00 26.00 68.0  NA 115.000 5.000 5000.00 929.00

dollars <- grepl("dollar?|USD|\\$", sd$REMARKS)

# Check for clues for the exponents where there is non-zero damage
sapply(c("","-","+","0","2","3","4","5","6","7"), function(x) sd$REMARKS[dollars & sd$PROPDMGEXP == x & sd$PROPDMG > 0])
# "" - no clues
# "-" - no clues
# "+" - no clues
# "0" - clues, inconcistent. Probable exponents c(1,6,4,6,6,6,3,3). 201 obsevations with no clues.
sd$PROPDMG[dollars & sd$PROPDMGEXP == "0" & sd$PROPDMG > 0] 
# "2" - no clues
# "3" - no clues
# "4" - no clues
# "5" - clues, inconsistent. Probable exponents c(5,6,3,4). Second item should have PROPDMG value of 18 instead of 30. 14 obs. with no clues
sd$PROPDMG[dollars & sd$PROPDMGEXP == "5" & sd$PROPDMG > 0] 
# "6" - no clues
# "7" - no clues

hist(year(sd$BGN_DATE), breaks = max(year(sd$BGN_DATE)) - min(year(sd$BGN_DATE)) + 1)

###############
# Zaczynamy cleanup
# 
sd1 <- select(sd, prop = PROPDMG, propexp = PROPDMGEXP)
sd1$propexp[dollars & sd$PROPDMGEXP == "0" & sd$PROPDMG > 0] <- c(1,6,4,6,6,6,3,3)
sd1$propexp[dollars & sd$PROPDMGEXP == "5" & sd$PROPDMG > 0] <- c(5,6,3,4)
sd1$prop[dollars & sd$PROPDMGEXP == "5" & sd$PROPDMG > 0][2] <- 18
sd1 <- mutate(sd1, proptot = 0)
sd1$proptot[sd1$propexp == "H"] <- sd1$prop[sd1$propexp == "H"] * 100
sd1$proptot[sd1$propexp == "K"] <- sd1$prop[sd1$propexp == "K"] * 10**3
sd1$proptot[sd1$propexp == "M"] <- sd1$prop[sd1$propexp == "M"] * 10**6
sd1$proptot[sd1$propexp == "B"] <- sd1$prop[sd1$propexp == "B"] * 10**9
sd1$proptot[sd1$propexp == ""] <- sd1$prop[sd1$propexp == ""]

for(ex in c("0","2","3","4","5","6","7")) {
    sd1$proptot[sd1$propexp == ex] <- sd1$prop[sd1$propexp == ex] * 10**as.integer(ex)
}

sd1 <- mutate(sd1, begdate = sd$BGN_DATE)

# What are PROP damages per year?
s <- sd1 %>% group_by(year(begdate)) %>% summarise(yproptot = sum(proptot))
plot(s, type = "l")
# From the plot it can be seen that some significant damages start in early 90s
plot(s[s$`year(begdate)` <= 1990,], type = "l")
# but still, there are significant damages (up to 2B USD) between 1950 and 1990

range(sd1$proptot)    # 0-1.15e+11
summary(sd1$proptot)
sum(sd1$proptot > 0)  # 239167, was 239174 events

# Total PROP damages are
gt <- sum(sd1$proptot)
formatC(gt, format = "g") # 4.283e+11 USD
# thereof with H/K/M/B exponent:
lt <- sum(sd1$proptot[sd1$propexp %in% c("H", "K", "M", "B")])
formatC(lt, format = "g") # 4.273e+11 USD
# so the "strange" exponents account for:
(gt - lt)/gt * 100 # 0.2212669 % per se, but it is possible that they contribute sognificantly to some events

# CROPDMGEXP multiplier cleanup
# 
sd$CROPDMGEXP <- toupper(sd$CROPDMGEXP)
cexp <- unique(sd$CROPDMGEXP)
cexp <- cexp[order(cexp)]
#  [1] ""  "?" "0" "2" "B" "K" "M"

# We know the histogram of number of observations vs. year, so let's check the span of exponents in years...
# 
sapply(cexp, function(x) range(year(sd$BGN_DATE[sd$CROPDMGEXP == x])))
#              ?    0    2    B    K    M
# [1,] 1950 1993 1993 1995 1993 1993 1993
# [2,] 2006 1995 1995 1995 2011 2011 2011
#
# No particular hint apart from the fact that most of the numeric exponents are from mid-90s

# How many observations are for each exponent?
# 
sapply(cexp, function(x) sum(sd$CROPDMGEXP == x))
#             ?      0      2      B      K      M 
# 618413      7     19      1      9 281853   1995 

# How many observations with non-zero CROPDMG are for each exponent?
# 
sapply(cexp, function(x) sum(sd$CROPDMGEXP == x & sd$CROPDMG != 0))
#       ?     0     2     B     K     M 
# 3     0    12     0     7 20158  1919 

# What is the "raw" sum of CROPDMG for each exponen?
# 
sapply(cexp, function(x) sum(sd$CROPDMG[sd$CROPDMGEXP == x]))
#                ?          0          2          B          K          M 
# 11.00       0.00     260.00       0.00      13.61 1343391.91   34150.80 

# Pomysł na wyczyszczenie CROPDMGEXP (ignoring effects of inflation):
# - K, M, B are converted to uppercase and applied accordingly (10e3/10e6/10e9)
# - exponents are ignored if there are no non-zero CROPDMG entries for them (i.e. ?, 2)
# - for remaining numeric exponents the clues are sought in REMARKS, and if not found they are assumed to be exponents of the base
#   of 10. The overall error/contribution is calculated as to the total of CROPDMG for 3 cases: 1) ignoring CROPDMG values for those
#   exponents, 2) assuming they are base 10 exponents, 3) taking raw CROPDMG values ignoring  the exponent
# - for "" exponent the clues are sought in REMARKS. If no clues, values are taken directly.
# Clues in remarks are strings dollar/USD/$. No translation of descriptive damages to cash according to the NWS instruction is performed
# due to to much time cost associated.

sapply(cexp, function(x) summary(sd$CROPDMG[sd$CROPDMGEXP == x & sd$CROPDMG > 0]))
#                 ?     0   2     B      K      M
# Min.    3.000  NA  5.00  NA 0.200   0.01   0.01
# 1st Qu. 3.500  NA  5.00  NA 0.450   5.00   1.00
# Median  4.000  NA 12.50  NA 1.000  10.00   3.00
# Mean    3.667 NaN 21.67 NaN 1.944  66.64  17.80
# 3rd Qu. 4.000  NA 31.25  NA 3.255  50.00  10.00
# Max.    4.000  NA 60.00  NA 5.000 990.00 596.00

sapply(c("","0"), function(x) sd$REMARKS[dollars & sd$CROPDMGEXP == x & sd$CROPDMG > 0])
# "" - no clues
# "0" - no clues

###############
# Zaczynamy cleanup
# 
sd1 <- mutate(sd1, crop = sd$CROPDMG, cropexp = sd$CROPDMGEXP)

sd1 <- mutate(sd1, croptot = 0)
sd1$croptot[sd1$cropexp == "K"] <- sd1$crop[sd1$cropexp == "K"] * 10**3
sd1$croptot[sd1$cropexp == "M"] <- sd1$crop[sd1$cropexp == "M"] * 10**6
sd1$croptot[sd1$cropexp == "B"] <- sd1$crop[sd1$cropexp == "B"] * 10**9
sd1$croptot[sd1$cropexp == ""] <- sd1$crop[sd1$cropexp == ""]

# What are CROP damages per year?
s1 <- sd1 %>% group_by(year(begdate)) %>% summarise(ycroptot = sum(croptot))
plot(s1, type = "l")
# From the plot it can be seen that some significant damages start in early 90s
plot(s1[s1$`year(begdate)` <= 1995,], type = "l")
# Before 1990 the data on crop damages is nonexistent
# 

range(sd1$croptot)    # 0-5e+09
summary(sd1$croptot)
sum(sd1$croptot > 0)  # 22087, was 22099 events

#####################
# FATALITIES

range(sd$FATALITIES)    # 0-583
summary(sd$FATALITIES)
sum(sd$FATALITIES)      # 15145
sum(sd$FATALITIES > 0)  # 6974 events

f <- sd %>% group_by(year = year(BGN_DATE)) %>% summarise(yftotal = sum(FATALITIES))
plot(f, type = "l")

# Looks like FATALITIES variable is clean

#####################
# INJURIES

range(sd$INJURIES)      # 0-1700
summary(sd$INJURIES)
sum(sd$INJURIES > 0) # 17604 events

i <- sd %>% group_by(year = year(BGN_DATE)) %>% summarise(yitotal = sum(INJURIES))
plot(i, type = "l")
points(f, type = "l", col = "red")

# Looks like INJURIES variable is clean

# Now, when the damages and injuries/fatalities are more-less clean we can use them
# to subset EVTYPE and try to clean it up.

sd1 <- mutate(sd1, fatalities = sd$FATALITIES, injuries = sd$INJURIES)

length(unique(sd$EVTYPE)) # 985

# Get EVTYPE and remove leading and trailing whitespace
sd1 <- mutate(sd1, evtype = toupper(gsub("^\\s+|\\s+$", "", sd$EVTYPE)))

####################
# Inspect event types for each of four variabes of interest, filter them out, merge
# those accounting for x% for each variable, merge resulting event types, clear them up
#
impacts <- sd1 %>% group_by(evtype) %>%
            summarise_each(funs(sum), fatalities, injuries, proptot, croptot) %>%
            filter(fatalities > 0 | injuries > 0 | proptot > 0 | croptot > 0)

# Inspect events contributing to fatalities the most
#
# Find events accounting for 80% of fatalities
impacts <- arrange(impacts, desc(fatalities))
fatal80 <- .8 * sum(impacts$fatalities) # 12116
fatalcumsum <- cumsum(impacts$fatalities)
# [1]  5633  7536  8514  9451 10267 10771 11241 11609 11857 12081 ...
topfatidx <- which(fatalcumsum <= fatal80)
topfatevents <- impacts$evtype[topfatidx]
# [1] "TORNADO"        "EXCESSIVE HEAT" "FLASH FLOOD"    "HEAT"           "LIGHTNING"     
# [6] "TSTM WIND"      "FLOOD"          "RIP CURRENT"    "HIGH WIND"      "AVALANCHE"     
impacts$fatalities[topfatidx]
# [1] 5633 1903  978  937  816  504  470  368  248  224
# 
# TSTM should be spelled as THUNDERSTORM accorfing to the NWS instruction, so we correct this and re-run the check
# 
sd1$evtype <- gsub("TSTM", "THUNDERSTORM", sd1$evtype)

impacts <- sd1 %>% group_by(evtype) %>%
    summarise_each(funs(sum), fatalities, injuries, proptot, croptot) %>%
    filter(fatalities > 0 | injuries > 0 | proptot > 0 | croptot > 0)

impacts <- arrange(impacts, desc(fatalities))
fatal80 <- .8 * sum(impacts$fatalities) # 12116
fatalcumsum <- cumsum(impacts$fatalities)
# [1]  5633  7536  8514  9451 10267 10904 11374 11742 11990
topfatidx <- which(fatalcumsum <= fatal80)
topfatevents <- impacts$evtype[topfatidx]
# [1] "TORNADO"           "EXCESSIVE HEAT"    "FLASH FLOOD"       "HEAT"             
# [5] "LIGHTNING"         "THUNDERSTORM WIND" "FLOOD"             "RIP CURRENT"      
# [9] "HIGH WIND"

#
# Find events accounting for 80% of injuries
impacts <- arrange(impacts, desc(injuries))
injury80 <- .8 * sum(impacts$injuries) # 112422.4
injurycumsum <- cumsum(impacts$injuries)
# [1]  91346  99791 106580
topinjidx <- which(injurycumsum <= injury80)
topinjevents <- impacts$evtype[topinjidx]
# [1] "TORNADO"           "THUNDERSTORM WIND" "FLOOD"

# Find events accounting for 80% of prop damages
impacts <- arrange(impacts, desc(proptot))
prop80 <- .8 * sum(impacts$proptot)  # 3.42613e+11
propcumsum <- cumsum(impacts$proptot)
# [1] 144657709807 213963549807 270935930398 314259466398 331082190377 ...
toppropidx <- which(propcumsum <= prop80)
toppropevents <- impacts$evtype[toppropidx]
# [1] "FLOOD"             "HURRICANE/TYPHOON" "TORNADO"           "STORM SURGE"      
# [5] "FLASH FLOOD"      

# Find events accounting for 80% of crop damages
impacts <- arrange(impacts, desc(croptot))
crop80 <- .8 * sum(impacts$croptot) # 39283353537
cropcumsum <- cumsum(impacts$croptot)
# [1] 13972566000 19634534450 24663993450 29686106950 32712061403 35453971403 38061844203 ...
topcropidx <- which(cropcumsum <= crop80)
topcropevents <- impacts$evtype[topcropidx]
# [1] "DROUGHT"           "FLOOD"             "RIVER FLOOD"       "ICE STORM"        
# [5] "HAIL"              "HURRICANE"         "HURRICANE/TYPHOON"

topevents <- unique(c(topfatevents, topinjevents, toppropevents, topcropevents))
# [1] "TORNADO"           "EXCESSIVE HEAT"    "FLASH FLOOD"       "HEAT"             
# [5] "LIGHTNING"         "THUNDERSTORM WIND" "FLOOD"             "RIP CURRENT"      
# [9] "HIGH WIND"         "HURRICANE/TYPHOON" "STORM SURGE"       "DROUGHT"          
# [13] "RIVER FLOOD"       "ICE STORM"         "HAIL"              "HURRICANE"        
# 
# There are all three events which were the only ones registered between 1950 and 1995,
# so it is quite probable that inlcuding all available years will bias the results.
# 
# In order to establish the magnitude of the possible bias, and having inspected visually
# the list of unique EVTYPE values, before we proceed to the evaluation we first need
# to translate the actual list of top contributing events to the terms of the official
# 48 events. This will result in a new, possibly shorter list of top contributing events.
# This new list will help to clean up the global event types list.
# With the EVTYPEs cleaned up and aggregated to top contributors the analysis of top
# contributors will be re-run. If the 1950-1992 EVTYPEs will be still present, the
# magnitude of possible bias will be calculated. If bias is significant, the top contributors
# list will be the one for the events registered from 1993 onwards.
# 
# Top contributors are defined as those contributing to 80% of casualties/damages.
# Verification of the border conditions will be performed, too. By this is ment 
# the check if the closer to 80% is the list of events accountign to just below or just
# above the limit of 80% impact.


# First, let's try to reduce 'topevents' further by matching it to the 48 official events.

propevent <- "Astronomical Low Tide Z Avalanche Z Blizzard Z Coastal Flood Z Cold/Wind Chill Z Debris Flow C Dense Fog Z Dense Smoke Z Drought Z Dust Devil C Dust Storm Z Excessive Heat Z Extreme Cold/Wind Chill Z Flash Flood C Flood C Frost/Freeze Z Funnel Cloud C Freezing Fog Z Hail C Heat Z Heavy Rain C Heavy Snow Z High Surf Z High Wind Z Hurricane (Typhoon) Z Ice Storm Z Lake-Effect Snow Z Lakeshore Flood Z Lightning C Marine Hail M Marine High Wind M Marine Strong Wind M Marine Thunderstorm Wind M Rip Current Z Seiche Z Sleet Z Storm Surge/Tide Z Strong Wind Z Thunderstorm Wind C Tornado C Tropical Depression Z Tropical Storm Z Tsunami Z Volcanic Ash Z Waterspout M Wildfire Z Winter Storm Z Winter Weather Z"
propevent <- strsplit(propevent, " [ZCM] | Z")
propevent <- toupper(propevent[[1]])

# Parameters for stringsim chosen experimentally
strdst <- sapply(topevents, function(x) stringsim(propevent, x, method = "qgram", q = 3))
dst <- apply(X = strdst,MARGIN = 2,max)
sum(dst == 1) # 12
# 
# Out of 16 identified top contributors 12 are exact matches of from the official set
# of 48.
# 
# Positions of the maximum similarities inside 48 official strings vector
maxsimpos <- apply(strdst, 2, which.max)
# proper names for those misspelled
tostridx <- maxsimpos[dst < 1 & dst > .3]
# and the misspelled themselves
fromstridx <- which(dst < 1 & dst > .3)
#
# Now we modify the top contibuting misspelled events from fromstr to tostr 
topevents[fromstridx] <- propevent[tostridx]
topevents <- unique(topevents)
# [1] "TORNADO"             "EXCESSIVE HEAT"      "FLASH FLOOD"        
# [4] "HEAT"                "LIGHTNING"           "THUNDERSTORM WIND"  
# [7] "FLOOD"               "RIP CURRENT"         "HIGH WIND"          
# [10] "HURRICANE (TYPHOON)" "STORM SURGE/TIDE"    "DROUGHT"            
# [13] "ICE STORM"           "HAIL"               
# 
# Top contributing event types list has been reduced to 14 items.
# Now this list will be used to clean up the global event type list in a similar way as
# it was cleaned up itself.

# One possible approach: match all event types then apply resulting indexes to directly
# manipulate the evtype vector.
# Other approach could be to generate a vector of unique evtypes for which there is impact
# on the questions, but it is not used in this analysis.

gstrdst <- sapply(sd1$evtype, function(x) stringsim(topevents, x, method = "qgram", q = 3))
gdst <- apply(gstrdst, 2, max)
sum(gdst == 1) # 774954
sum(gdst < 1 & gdst > 0) # 106260

# Positions of the maximum similarities inside  strings vector
gmaxsimpos <- apply(gstrdst, 2, which.max)
# index into proper names vector for those misspelled
gtostridx <- gmaxsimpos[gdst < 1 & gdst > .3]
# index into the misspelled vector
gfromstridx <- which(gdst < 1 & gdst > .3)
#
# Now we need to check whetehr in the list of evtypes identified to correct there are any
# which match exactly one of the 48 official strings. That would mean they are false positives
# and need not to be corrected.
gexactstrdist <- sapply(sd1$evtype[gfromstridx], function(x) stringsim(propevent, x, method = "qgram", q = 3))
gexactdst <- apply(gexactstrdist, 2, max)
sum(gexactdst == 1) # 30097
# We need to filter out the indexes for which there is an exact match with official 48 evtypes

gtostridx <- gtostridx[gexactdst != 1]
gfromstridx <- gfromstridx[gexactdst != 1]

# Now we modify the top contibuting misspelled events from fromstr to tostr 
sd1$evtype[gfromstridx] <- topevents[gtostridx]

# Visual inspection shows that there are some residual false positives (e.g. "COASTAL FLOODING"
# being tranlated to "FLOOD"), but its impact on "FLOOD" can be neglected (3 to 6 orders of magnitude
# smaller that that of pure "FLOOD" event type.)

# Now we can re-run the analysis of the event types having the biggest influence on the research
# questions and verify if the cleanup of evtypes has significant impact.
# 
impacts2 <- sd1 %>% group_by(evtype) %>%
    summarise_each(funs(sum), fatalities, injuries, proptot, croptot) %>%
    filter(fatalities > 0 | injuries > 0 | proptot > 0 | croptot > 0)

impacts2 <- arrange(impacts2, desc(fatalities))
fatal80_2 <- .8 * sum(impacts2$fatalities) # 12116
fatalcumsum2 <- cumsum(impacts2$fatalities)
# is:
# [1]  5636  7656  8772  9808 10627 11364 11941
# was:
# [1]  5633  7536  8514  9451 10267 10904 11374 11742 11990
topfatidx2 <- which(fatalcumsum2 <= fatal80_2)
topfatevents2 <- impacts2$evtype[topfatidx2]
# is:
# [1] "TORNADO"           "EXCESSIVE HEAT"    "HEAT"              "FLASH FLOOD"      
# [5] "LIGHTNING"         "THUNDERSTORM WIND" "RIP CURRENT"      
# was:
# [1] "TORNADO"           "EXCESSIVE HEAT"    "FLASH FLOOD"       "HEAT"             
# [5] "LIGHTNING"         "THUNDERSTORM WIND" "FLOOD"             "RIP CURRENT"      
# [9] "HIGH WIND"

# Cleanup of evtype resulted in a shorter list of event types contributing to around
# 80% of fatalities (from 9 reduced to 7). Nevertheless there is no change in the 
# content of the list of 7, only the order of some events changed.
# 
# 80% cutoff border verification.
# Absolute difference error for the cutoff just below 80% is:
faterror1 <- abs(80 - fatalcumsum2[max(topfatidx2)]/sum(impacts2$fatalities) * 100) # 1.155497
# Absolute difference error for the cutoff just below 80% is:
faterror2 <- abs(80 - fatalcumsum2[max(topfatidx2) + 1]/sum(impacts2$fatalities) * 100) # 2.04688
#
# As the absolute difference erorr is lower for the cutoff just below 80% we maintain the
# current list of events as the top contributing to fatalities.

#
# Find events accounting for 80% of injuries
impacts2 <- arrange(impacts2, desc(injuries))
injury80_2 <- .8 * sum(impacts2$injuries) # 112422.4
injurycumsum2 <- cumsum(impacts2$injuries)
# is:
# [1]  91407 100918 107713
# was:
# [1]  91346  99791 106580
topinjidx2 <- which(injurycumsum2 <= injury80_2)
topinjevents2 <- impacts2$evtype[topinjidx2]
# is and was:
# [1] "TORNADO"           "THUNDERSTORM WIND" "FLOOD"
#
# So, cleanup of event types didn't influence on the list of top contributors to injuries.
# 
# 80% cutoff border verification.
# Absolute difference error for the cutoff just below 80% is:
injerror1 <- abs(80 - injurycumsum2[max(topinjidx2)]/sum(impacts2$injuries) * 100) # 3.351218
# Absolute difference error for the cutoff just below 80% is:
injerror2 <- abs(80 - injurycumsum2[max(topinjidx2) + 1]/sum(impacts2$injuries) * 100) # 1.41865
#
# As the absolute difference erorr is lower for the cutoff just above 80% we extend the
# current list of events with the next top contributor to injuries:
topinjevents2 <- impacts2$evtype[1:(max(topinjidx2) + 1)]
# [1] "TORNADO"           "THUNDERSTORM WIND" "FLOOD"             "EXCESSIVE HEAT"

# Find events accounting for 80% of prop damages
impacts2 <- arrange(impacts2, desc(proptot))
prop80_2 <- .8 * sum(impacts2$proptot)  # 3.42613e+11
propcumsum2 <- cumsum(impacts2$proptot)
# [1] 144657709807 213963549807 270935930398 314259466398 331082190377 ...
toppropidx2 <- which(propcumsum2 <= prop80_2)
toppropevents2 <- impacts2$evtype[toppropidx2]
# is:
# [1] "FLOOD"               "HURRICANE (TYPHOON)" "TORNADO"             "STORM SURGE/TIDE"
# was:
# [1] "FLOOD"             "HURRICANE/TYPHOON" "TORNADO"           "STORM SURGE"      
# [5] "FLASH FLOOD"      
# Cleanup of evtype resulted in a shorter list of event types contributing to around
# 80% of property damages (from 5 reduced to 4). Nevertheless there is no change in the 
# content of the list of 4, and the order is also the same.
# 
# 80% cutoff border verification.
# Absolute difference error for the cutoff just below 80% is:
properror1 <- abs(80 - propcumsum2[max(toppropidx2)]/sum(impacts2$proptot) * 100) # 0.485149
# Absolute difference error for the cutoff just below 80% is:
properror2 <- abs(80 - propcumsum2[max(toppropidx2) + 1]/sum(impacts2$proptot) * 100) # 3.627027
#
# As the absolute difference erorr is lower for the cutoff just below 80% we maintain the
# current list of events as the top contributing to property damages.

# Find events accounting for 80% of crop damages
impacts2 <- arrange(impacts2, desc(croptot))
crop80_2 <- .8 * sum(impacts2$croptot) # 39283353537
cropcumsum2 <- cumsum(impacts2$croptot)
# is:
# [1] 13972616000 24820119950 30326237750 35365401250 38412288853
# was:
# [1] 13972566000 19634534450 24663993450 29686106950 32712061403 35453971403 38061844203 ...
topcropidx2 <- which(cropcumsum2 <= crop80_2)
topcropevents2 <- impacts2$evtype[topcropidx2]
# is:
# [1] "DROUGHT"             "FLOOD"               "HURRICANE (TYPHOON)" "ICE STORM"          
# [5] "HAIL"               
# was:
# [1] "DROUGHT"           "FLOOD"             "RIVER FLOOD"       "ICE STORM"        
# [5] "HAIL"              "HURRICANE"         "HURRICANE/TYPHOON"
#
# Cleanup of evtype resulted in a shorter list of event types contributing to around
# 80% of crop damages (from 7 reduced to 5). Nevertheless there is no substantial change
# in the content of the list of 5 (some events merged after cleanup), but the HURRICANE/TYPHOON
# after cleanup has more impact that ICE STORM.
# 
# 80% cutoff border verification.
# Absolute difference error for the cutoff just below 80% is:
croperror1 <- abs(80 - cropcumsum2[max(topcropidx2)]/sum(impacts2$croptot) * 100) # 1.773911
# Absolute difference error for the cutoff just below 80% is:
croperror2 <- abs(80 - cropcumsum2[max(topcropidx2) + 1]/sum(impacts2$croptot) * 100) # 1.346397
#
# As the absolute difference erorr is lower for the cutoff just above 80% we extend the
# current list of events with the next top contributor to crop damages:
topcropevents2 <- impacts2$evtype[1:(max(topcropidx2) + 1)]
# [1] "DROUGHT"             "FLOOD"               "HURRICANE (TYPHOON)" "ICE STORM"          
# [5] "HAIL"                "FLASH FLOOD"        

# Final list of contributors before 1950-1995 bias check:
topevents2 <- unique(c(topfatevents2, topinjevents2, toppropevents2, topcropevents2))
# is:
# [1] "TORNADO"             "EXCESSIVE HEAT"      "HEAT"               
# [4] "FLASH FLOOD"         "LIGHTNING"           "THUNDERSTORM WIND"  
# [7] "RIP CURRENT"         "FLOOD"               "HURRICANE (TYPHOON)"
# [10] "STORM SURGE/TIDE"    "DROUGHT"             "ICE STORM"          
# [13] "HAIL"               
# was:
# [1] "TORNADO"             "EXCESSIVE HEAT"      "FLASH FLOOD"        
# [4] "HEAT"                "LIGHTNING"           "THUNDERSTORM WIND"  
# [7] "FLOOD"               "RIP CURRENT"         "HIGH WIND"          
# [10] "HURRICANE (TYPHOON)" "STORM SURGE/TIDE"    "DROUGHT"            
# [13] "ICE STORM"           "HAIL"               
#
# The list has been reduced to 13 event types.

############################
# Analysis of possible bias towards TORNADO, THUNDERSTORM WIND and HAIL.
# In order to check for the possible bias towards the above mentioned event types,
# the analyzed data frame will be reduced to the range spannign the years
# 1993 till 2011 and the list of top contributors towards fatalities, injuries
# and property damage will be recalculated. No recalculation will be done
# for crop damages as this data was not recorded until 1993.

impacts3 <- sd1 %>% 
    filter(year(begdate) >= 1993) %>%
    group_by(evtype) %>%
    summarise_each(funs(sum), fatalities, injuries, proptot, croptot) %>%
    filter(fatalities > 0 | injuries > 0 | proptot > 0 | croptot > 0)

impacts3 <- arrange(impacts3, desc(fatalities))
fatal80_3 <- .8 * sum(impacts3$fatalities) # 12116
fatalcumsum3 <- cumsum(impacts3$fatalities)
# is:
# [1] 2020 3644 4760 5796 6615 7192 7677 8151 8496
# was:
# [1]  5636  7656  8772  9808 10627 11364 11941
topfatidx3 <- which(fatalcumsum3 <= fatal80_3)
topfatevents3 <- impacts3$evtype[topfatidx3]
# is:
# [1] "EXCESSIVE HEAT"    "TORNADO"           "HEAT"              "FLASH FLOOD"      
# [5] "LIGHTNING"         "RIP CURRENT"       "FLOOD"             "THUNDERSTORM WIND"
# [9] "HIGH WIND"        
# was:
# [1] "TORNADO"           "EXCESSIVE HEAT"    "HEAT"              "FLASH FLOOD"      
# [5] "LIGHTNING"         "THUNDERSTORM WIND" "RIP CURRENT"      

# Bias check resulted in again 9 items on the list and slightly re-arranged.
# 
# 80% cutoff border verification.
# Absolute difference error for the cutoff just below 80% is:
faterror31 <- abs(80 - fatalcumsum3[max(topfatidx3)]/sum(impacts3$fatalities) * 100) # 1.803958
# Absolute difference error for the cutoff just below 80% is:
faterror32 <- abs(80 - fatalcumsum3[max(topfatidx3) + 1]/sum(impacts3$fatalities) * 100) # 0.2577082
#
# As the absolute difference erorr is lower for the cutoff just above 80% we extend the
# current list of events with the next top contributor to fatalities:
topfatevents3 <- impacts3$evtype[1:(max(topfatidx3) + 1)]
# [1] "EXCESSIVE HEAT"    "TORNADO"           "HEAT"              "FLASH FLOOD"      
# [5] "LIGHTNING"         "RIP CURRENT"       "FLOOD"             "THUNDERSTORM WIND"
# [9] "HIGH WIND"         "AVALANCHE"        
#
topfatscores3 <- impacts3$fatalities[1:(max(topfatidx3) + 1)]
# [1] 2020 1624 1116 1036  819  577  485  474  345
ggplot(data.frame(topfatevents3, topfatscores3), aes(x = topfatevents3, y = topfatscores3)) + geom_bar(stat = "identity", fill = "darkgreen") + labs(x = "Event type", y = "Fatalities")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
# One new item appeared on the list - AVALANCHE
# Let's check with the simple grep if there are possibly misspeled events related to AVALANCHE
impacts3$evtype[grepl("AVAL", impacts3$evtype)]
# Let's check the number of fatalities for each of those event types:
impacts3$fatalities[grepl("AVAL", impacts3$evtype)]
# [1] 224   1   0
# So we can neglect the misspelled event types related to AVALANCHE


# Find events accounting for 80% of injuries
impacts3 <- arrange(impacts3, desc(injuries))
injury80_3 <- .8 * sum(impacts3$injuries) # 112422.4
injurycumsum3 <- cumsum(impacts3$injuries)
# is:
# [1] 23371 30166 36869 43054 48287 50816 52868 54670
# was:
# [1]  91407 100918 107713
topinjidx3 <- which(injurycumsum3 <= injury80_3)
topinjevents3 <- impacts3$evtype[topinjidx3]
# is:
# [1] "TORNADO"           "FLOOD"             "EXCESSIVE HEAT"    "THUNDERSTORM WIND"
# [5] "LIGHTNING"         "HEAT"              "ICE STORM"         "FLASH FLOOD"      
# was:
# [1] "TORNADO"           "THUNDERSTORM WIND" "FLOOD"
#
# So, there was a strong bias towards TORNADO and THUNDERSTORM WIND. The list after bias filtering
# contains 8 items. TORNADO still holds the first place, but there are more important events
# contributing to number of injuries.
# 
# 80% cutoff border verification.
# Absolute difference error for the cutoff just below 80% is:
injerror31 <- abs(80 - injurycumsum3[max(topinjidx3)]/sum(impacts3$injuries) * 100) # 0.497346
# Absolute difference error for the cutoff just below 80% is:
injerror32 <- abs(80 - injurycumsum3[max(topinjidx3) + 1]/sum(impacts3$injuries) * 100) # 1.903585
#
# As the absolute difference erorr is lower for the cutoff just below 80% we maintain the
# current list of events as the top contributing to property damages.
topinjscores3 <- impacts3$injuries[topinjidx3]
# [1] 23371  6795  6703  6185  5233  2529  2052  1802
ggplot(data.frame(topinjevents3, topinjscores3), aes(x = topinjevents3, y = topinjscores3)) + geom_bar(stat = "identity", fill = "darkgreen") + labs(x = "Event type", y = "Injuries")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Find events accounting for 80% of prop damages
impacts3 <- arrange(impacts3, desc(proptot))
prop80_3 <- .8 * sum(impacts3$proptot)  # 3.42613e+11
propcumsum3 <- cumsum(impacts3$proptot)
# [1] 144657709807 213963549807 270935930398 314259466398 331082190377 ...
toppropidx3 <- which(propcumsum3 <= prop80_3)
toppropevents3 <- impacts3$evtype[toppropidx3]
# is:
# [1] "FLOOD"               "HURRICANE (TYPHOON)" "STORM SURGE/TIDE"    "TORNADO"
# was:
# [1] "FLOOD"               "HURRICANE (TYPHOON)" "TORNADO"             "STORM SURGE/TIDE"
#
# Cleanup of evtype resulted in a shorter list of event types contributing to around
# 80% of property damages (from 5 reduced to 4). Nevertheless there is no change in the 
# content of the list of 4, and the order is also the same.
# 
# 80% cutoff border verification.
# Absolute difference error for the cutoff just below 80% is:
properror31 <- abs(80 - propcumsum3[max(toppropidx3)]/sum(impacts3$proptot) * 100) # 2.06136
# Absolute difference error for the cutoff just below 80% is:
properror32 <- abs(80 - propcumsum3[max(toppropidx3) + 1]/sum(impacts3$proptot) * 100) # 2.367224
#
# As the absolute difference erorr is lower for the cutoff just below 80% we maintain the
# current list of events as the top contributing to property damages.
toppropscores3 <- impacts3$proptot[toppropidx3]
# [1] 23371  6795  6703  6185  5233  2529  2052  1802
ggplot(data.frame(toppropevents3, toppropscores3), aes(x = toppropevents3, y = toppropscores3/10^9)) + geom_bar(stat = "identity", fill = "darkgreen") + labs(x = "Event type", y = "B USD")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Find events accounting for 80% of crop damages
impacts3 <- arrange(impacts3, desc(croptot))
crop80_3 <- .8 * sum(impacts3$croptot) # 39283353537
cropcumsum3 <- cumsum(impacts3$croptot)
# is:
# [1] 13972616000 24820119950 30326237750 35365401250 38412288853
# was:
# [1] 13972566000 19634534450 24663993450 29686106950 32712061403 35453971403 38061844203 ...
topcropidx3 <- which(cropcumsum3 <= crop80_3)
topcropevents3 <- impacts3$evtype[topcropidx3]
# is and was:
# [1] "DROUGHT"             "FLOOD"               "HURRICANE (TYPHOON)" "ICE STORM"          
# [5] "HAIL"               
#
# Cleanup of evtype resulted in ...
# 
# 80% cutoff border verification.
# Absolute difference error for the cutoff just below 80% is:
croperror31 <- abs(80 - cropcumsum3[max(topcropidx3)]/sum(impacts3$croptot) * 100) # 1.773911
# Absolute difference error for the cutoff just below 80% is:
croperror32 <- abs(80 - cropcumsum3[max(topcropidx3) + 1]/sum(impacts3$croptot) * 100) # 1.346397
#
# As the absolute difference erorr is lower for the cutoff just above 80% we extend the
# current list of events with the next top contributor to crop damages:
topcropevents3 <- impacts3$evtype[1:(max(topcropidx3) + 1)]
# [1] "DROUGHT"             "FLOOD"               "HURRICANE (TYPHOON)" "ICE STORM"          
# [5] "HAIL"                "FLASH FLOOD"        
topcropscores3 <- impacts3$croptot[1:(max(topcropidx3) + 1)]
# [1] 13972616000 10847503950  5506117800  5039163500  3046887603  1532202150
ggplot(data.frame(topcropevents3, topcropscores3), aes(x = topcropevents3, y = topcropscores3/10^9)) + geom_bar(stat = "identity", fill = "darkgreen") + labs(x = "Event type", y = "B USD")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Final list of contributors:
topevents3 <- unique(c(topfatevents3, topinjevents3, toppropevents3, topcropevents3))
# is:
# [1] "EXCESSIVE HEAT"      "TORNADO"             "HEAT"               
# [4] "FLASH FLOOD"         "LIGHTNING"           "RIP CURRENT"        
# [7] "FLOOD"               "THUNDERSTORM WIND"   "HIGH WIND"          
# [10] "AVALANCHE"           "ICE STORM"           "HURRICANE (TYPHOON)"
# [13] "STORM SURGE/TIDE"    "DROUGHT"             "HAIL"               
# was:
# [1] "TORNADO"             "EXCESSIVE HEAT"      "HEAT"               
# [4] "FLASH FLOOD"         "LIGHTNING"           "THUNDERSTORM WIND"  
# [7] "RIP CURRENT"         "FLOOD"               "HURRICANE (TYPHOON)"
# [10] "STORM SURGE/TIDE"    "DROUGHT"             "ICE STORM"          
# [13] "HAIL"               
#
# The list has been expanded to 15 event types.

# Resulting data set:
resdf <- data.frame(evtype = topfatevents3, value = topfatscores3, group = "Fatalities")
resdf <- rbind(resdf, data.frame(evtype = topinjevents3, value = topinjscores3, group = "Injuries"))
resdf <- rbind(resdf, data.frame(evtype = toppropevents3, value = toppropscores3, group = "Property Damage"))
resdf <- rbind(resdf, data.frame(evtype = topcropevents3, value = topcropscores3, group = "Crop Damage"))

gg <- ggplot(resdf, aes(x = evtype, y = value, fill = group))
gg <- gg + geom_bar(stat = "identity")
gg <- gg + labs(x = "Event type")
gg <- gg + theme(axis.text.x = element_text(angle = 45, hjust = 1))
gg <- gg + facet_wrap(~group, scales = "free", ncol = 2)
gg

plot1 <- ggplot(resdf[resdf$group == "Fatalities",], aes(x = reorder(gsub(" ", "\n", evtype), value), y = value))
plot1 <- plot1 + geom_bar(stat = "identity", fill = "darkred")
plot1 <- plot1 + labs(y = "Fatalities", title= "Fatalities")
plot1 <- plot1 + theme(axis.title.y = element_blank())
plot1 <- plot1 + coord_flip()
plot1

plot2 <- ggplot(resdf[resdf$group == "Injuries",], aes(x = reorder(gsub(" ", "\n", evtype), value), y = value))
plot2 <- plot2 + geom_bar(stat = "identity", fill = "darkorange")
plot2 <- plot2 + labs(y = "Injuries", title = "Injuries")
plot2 <- plot2 + theme(axis.title.y = element_blank())
plot2 <- plot2 + coord_flip()
plot2

plot3 <- ggplot(resdf[resdf$group == "Property Damage",], aes(x = reorder(gsub(" ", "\n", evtype), value), y = value/10^9))
plot3 <- plot3 + geom_bar(stat = "identity", fill = "darkblue")
plot3 <- plot3 + labs(y = "B USD", title = "Property Damage")
plot3 <- plot3 + theme(axis.title.y = element_blank())
plot3 <- plot3 + coord_flip()
plot3

plot4 <- ggplot(resdf[resdf$group == "Crop Damage",], aes(x = reorder(gsub(" ", "\n", evtype), value), y = order(value/10^9)))
plot4 <- plot4 + geom_bar(stat = "identity", fill = "darkgreen")
plot4 <- plot4 + labs(y = "B USD", title = "Crop Damage")
plot4 <- plot4 + theme(axis.title.y = element_blank())
plot4 <- plot4 + coord_flip()
plot4

grid.arrange(plot1, plot2, ncol = 2, top="Fig. 2. Storm event most significant impacts")
grid.arrange(plot3, plot4, ncol = 2, top="Fig. 3. Storm event most significant impacts")

# ############################################### obsolete
# # We have the list of top events, now we can increase precision of impacts estimation
# # by aggregating possible misspelled events.
# # Visual inspection allows for TSTM correction
# # 
# topevents <- gsub("HURRICANE.*$", "HURRICANE (TYPHOON)", topevents)
# topevents <- unique(topevents)
# 
# # Manual inspection of the events:
# # 1. double whitespace removal
# sd1$evtype <- gsub("  ", " ",sd1$evtype)
# # 2. "Hail" in many combinations
# sd1$evtype <- gsub("HAIL.*$", "HAIL",sd1$evtype)
# # 3. "HURRICANE" in all combinations
# sd1$evtype <- gsub("HURRICANE.*$", "HURRICANE (TYPHOON)",sd1$evtype)
# 
# topevents
# # [1] "TORNADO"             "EXCESSIVE HEAT"      "FLASH FLOOD"        
# # [4] "HEAT"                "LIGHTNING"           "THUNDERSTORM WIND"  
# # [7] "FLOOD"               "RIP CURRENT"         "HIGH WIND"          
# # [10] "AVALANCHE"           "HURRICANE (TYPHOON)" "STORM SURGE"        
# # [13] "DROUGHT"             "RIVER FLOOD"         "ICE STORM"          
# # [16] "HAIL"               
# 
# # Let's investigate the error associated with misspelled events which are not taken into
# # account in the above analysis.
# 
# 
# 
# 
# 
# 
# flt <- sd1$proptot > 0 | sd1$croptot > 0 | sd1$fatalities > 0 | sd1$injuries > 0
# fltevent <- sd1$evtype[flt]
# uevent <- unique(fltevent)
# uevent <- uevent[order(uevent)]
# 
# # Inspecting uevent shows that there is one event coded as "?"
# # Let's see if it has any impact on our analysis
# sd$REMARKS[sd1$evtype == "?"]
# # [1] "  "
# sd$PROPDMG[sd1$evtype == "?"]
# # [1] 5
# sd$CROPDMG[sd1$evtype == "?"]
# # [1] 0
# sd$FATALITIES[sd1$evtype == "?"]
# # [1] 0
# sd$INJURIES[sd1$evtype == "?"]
# # [1] 0
# # No impact.
# 
# uevent <- uevent[uevent != "?"]
# # Manual inspection of the events:
# # 1. double whitespace removal
# uevent <- unique(gsub("  ", " ",uevent))
# # 2. "Hail" in many combinations
# uevent <- unique(gsub("HAIL.*$", "HAIL",uevent))
# # 3. "HURRICANE" in all combinations
# uevent <- unique(gsub("HURRICANE.*$", "HURRICANE (TYPHOON)",uevent))
# # 4. "TSTM" -> "THUNDERSTORM"
# uevent <- unique(gsub("TSTM", "THUNDERSTORM",uevent))
# # 5. Remove "APACHE COUNTY"
# uevent <- uevent[uevent != "APACHE COUNTY"]
# 
# dist <- stringdistmatrix(uevent, propevent, method = "qgram", q = 3)
# rownames(dist) <- uevent
# colnames(dist) <- propevent
# 
# matchv <- sapply(1:nrow(dist), function(x) which.min(dist[x,]))
# evtst <- data.frame(uevent = uevent, propname = colnames(dist)[matchv], dst = apply(dist,1,min))
# sortdist <- apply(dist, 2, sort)
# 
# 
# evcl <- hclust(as.dist(dist))
# plot(evcl)

# Report notes:
# - add chapter summarizing coarse exploratory analysis: much more strings that official
#   48, many are misspelled variants of the 48 or combinations thereof, e.g. related to
#   Thunderstorms, hurricanes, hail, flood, tornados, heat, etc.
# Threats to internal validity:
# - no correction for inflation
# - possible per-evtype errors due to assumed digits from EXP field meaning (.2% of overall
#   impact, but there is a possiblity, that on individual evtype level it can be higher)
# - Figure 1: fatalities/injuries/damages vs. year
# - Figure 2: barchart of top event contributing to each cathegory