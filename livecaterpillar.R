library(dplyr)
library(lubridate)
library(data.table)
library(gsheet)
library(maps)
library(sf)
library(stringr)
library(randomcoloR)


fullDataset = read.csv("data/fullDataset_2025-07-28.csv")
url = "https://docs.google.com/spreadsheets/d/1hi7iyi7xunriU2fvFpgNVDjO5ERML4IUgzTkQjLVYCo/edit?gid=0#gid=0"


df = gsheet2tbl(url) %>%
  mutate(DeployDate = as.Date(DeployDate, format = "%m/%d/%Y"),
         CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y")) %>%
  mutate(AdjustedDate = DeployDate + 4,
         cicada_period = ifelse(AdjustedDate >= as.Date("2024-05-14") & AdjustedDate <= as.Date("2024-06-13"), 1, 0))%>%
  mutate(date = str_extract(AdjustedDate, pattern = "\\d+-\\d+-\\d+"),
         year = as.integer(str_extract(DeployDate, "\\d+")),
         leapyear = leap_year(year),
         julianday = case_when(
           str_detect(date, "-05") ~ 120 + leapyear + as.numeric(substr(date, 9, 10)),
           str_detect(date, "-06") ~ 151 + leapyear + as.numeric(substr(date, 9, 10)),
           str_detect(date, "-07") ~ 181 + leapyear + as.numeric(substr(date, 9, 10))),
         julianweek = 7*floor(julianday/7) + 4)%>%
  mutate(year = as.integer(str_extract(DeployDate, "\\d+")))

birdPred = df %>%
  filter('Not_Found' != 1) %>%
  group_by(year, julianweek, Name, julianday) %>%
  summarize(numBirdStrikes = sum(Bird),
            numClayCats = n(),
            pctBird = 100 * numBirdStrikes / numClayCats)

srvyData <- fullDataset%>%
  filter(Year %in% c(2024:2025), Name %in% c("Eno River State Park",
                                             "Triangle Land Conservancy - Johnston Mill Nature Preserve",
                                             "Prairie Ridge Ecostation",
                                             "NC Botanical Garden",
                                             "UNC Chapel Hill Campus"))

#preparatory functions for meanDensity
Mode = function(x){ 
  if (!is.numeric(x)) {
    stop("values must be numeric for mode calculation")
  }
  ta = table(x)
  tam = max(ta)
  mod = as.numeric(names(ta)[ta == tam])
  return(max(mod))
}

#biweek = Vectorize(function(surveys, cday){
  bpwc = birdPred%>%
    ungroup()%>%
    select(year, Name, julianday)%>%
    filter((surveys$julianday[cday] >= (julianday - 7) & surveys$julianday[cday] <= (julianday + 6)) &
             Name == surveys$Name[cday] &
             year == surveys$Year[cday])
  if(length(bpwc$julianday) == 1){return(bpwc$julianday[1])}
  else{return(NA)}
},
vectorize.args = "cday")

bugsbyclayday = biweek(srvyData, c(1:nrow(srvyData)))%>%unlist()

#this adds julianweek or two week weekbin as options externally to meanDensity so that it is not inherently byWeek
srvyData = srvyData%>%
  mutate(weekbin = bugsbyclayday)%>%
  mutate(julianweek = 7*floor(julianday/7) + 4)

meanDensity = function(surveyData = srvyData,
                       year = 2024,
                       site = "all",
                       ordersToInclude = 'All',
                       
                       minLength = 0,         
                       jdRange = c(1,365),
                       outlierCount = 10000,
                       plot = FALSE,
                       plotVar = 'fracSurveys', # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
                       minSurveyCoverage = 0.8, 
                       allDates = TRUE,
                       new = TRUE,
                       color = 'black',
                       allCats = TRUE,
                       ...)                  

{

  if(site=="all") {
    site = c("Eno River State Park",
    "Triangle Land Conservancy - Johnston Mill Nature Preserve",
    "Prairie Ridge Ecostation",
    "NC Botanical Garden",
    "UNC Chapel Hill Campus")}
  surveyData = surveyData%>%
    filter(Year == year, Name %in% site)
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$Group)
  }
  
  numUniqueBranches = length(unique(surveyData$PlantFK))
  
  timeFilter = surveyData %>%
    filter(julianday >= jdRange[1], julianday <= jdRange[2])

  effortByWeek = timeFilter %>%
    group_by(julianweek) %>%
    summarize(nSurveyBranches = n_distinct(PlantFK),
              nSurveys = n_distinct(ID)) %>%
    mutate(modalBranchesSurveyed = Mode(5*ceiling(nSurveyBranches/5)),
           nSurveySets = nSurveys/modalBranchesSurveyed,
           modalSurveySets = Mode(round(nSurveySets)),
           okWeek = ifelse(nSurveySets/modalSurveySets >= minSurveyCoverage, 1, 0))
  if (allDates) {
    effortByWeek$okWeek = 1
  }
  
  if (!allCats) {
    defFilter = timeFilter %>%
      filter(Hairy != 1, Tented != 1, Rolled != 1)
  } else {
    defFilter = timeFilter
  }
  
  arthCount = defFilter %>%
    filter(Length >= minLength, 
           Group %in% ordersToInclude) %>%
    mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% 
    group_by(julianweek) %>%
    summarize(totalCount = sum(Quantity2, na.rm = TRUE),
              numSurveysGTzero = length(unique(ID[Quantity > 0])),
              totalBiomass = sum(Biomass_mg, na.rm = TRUE)) %>% 
    left_join(effortByWeek, by = "julianweek") %>%
    filter(okWeek == 1) %>%
    mutate(meanDensity = totalCount/nSurveys,
           fracSurveys = 100*numSurveysGTzero/nSurveys,
           meanBiomass = totalBiomass/nSurveys) %>%
    arrange(julianweek) %>%
    data.frame()
  return(arthCount)
}
plotDensity = function(site = "all",
                       surveyData = srvyData,
                       year = 2024,
                       ordersToInclude = "all",
                       new = TRUE,
                       plotVar = "fracSurveys",
                       color = randomColor(luminosity = "bright"),
                       ...)
  {
  arthCount = meanDensity(surveyData, year, site, ordersToInclude)
  
  if (new){
  plot(arthCount$julianweek, arthCount[, plotVar], type = 'l', col = color, las = 1, ...)
  points(arthCount$julianweek, arthCount[, plotVar], pch = 16, col = color, ...)
  } else if (new==F) {
  points(arthCount$julianweek, arthCount[, plotVar], type = 'l', col = color, ...)
  points(arthCount$julianweek, arthCount[, plotVar], pch = 16, col = color, ...)
  }
}

plotDensity(site = c("all"), year = 2025, ordersToInclude = "caterpillar")
plotDensity(site = c("all"), year = 2024, ordersToInclude = "caterpillar", new = FALSE)
plotDensity(site = c("Eno River State Park"), year = 2025, ordersToInclude = "caterpillar", new = FALSE)
plotDensity(site = c("Eno River State Park"), year = 2024, ordersToInclude = "caterpillar", new = FALSE)
plotDensity(site = c("Triangle Land Conservancy - Johnston Mill Nature Preserve"), year = 2025, ordersToInclude = "caterpillar", new = FALSE)
plotDensity(site = c("Triangle Land Conservancy - Johnston Mill Nature Preserve"), year = 2024, ordersToInclude = "caterpillar", new = FALSE)
plotDensity(site = c("Prairie Ridge Ecostation"), year = 2025, ordersToInclude = "caterpillar", new = FALSE)
plotDensity(site = c("Prairie Ridge Ecostation"), year = 2024, ordersToInclude = "caterpillar", new = FALSE)
plotDensity(site = c("NC Botanical Garden"), year = 2025, ordersToInclude = "caterpillar", new = FALSE)
plotDensity(site = c("NC Botanical Garden"), year = 2024, ordersToInclude = "caterpillar", new = FALSE)
plotDensity(site = c("UNC Chapel Hill Campus"), year = 2025, ordersToInclude = "caterpillar", new = FALSE)
plotDensity(site = c("UNC Chapel Hill Campus"), year = 2024, ordersToInclude = "caterpillar", new = FALSE)
plot(meanDensity(site = c("all"), year = 2025, ordersToInclude = "caterpillar", jdrange = c(130, 160))$fracSurveys -
     meanDensity(site = c("all"), year = 2024, ordersToInclude = "caterpillar", jdrange = c(130, 160))$fracSurveys,
     meanDensity(site = c("all"), year = 2024, ordersToInclude = "caterpillar", jdrange = c(125, 190))$julianweek)

"Eno River State Park"
"Triangle Land Conservancy - Johnston Mill Nature Preserve"
"Prairie Ridge Ecostation"
"NC Botanical Garden"
"UNC Chapel Hill Campus"
