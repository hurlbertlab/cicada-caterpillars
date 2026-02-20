library(dplyr)
library(lubridate)
library(data.table)
library(gsheet)
library(maps)
library(sf)
library(stringr)
library(randomcoloR)

sitecolsh = data.frame(site = c("Eno River State Park", 
                                "Triangle Land Conservancy - Johnston Mill Nature Preserve", 
                                "Prairie Ridge Ecostation", 
                                "NC Botanical Garden", 
                                "UNC Chapel Hill Campus", "all"), 
                       col = c(col = "red", "#D55E00", "#0072B2", "yellow3", "#CC79A7", "darkgreen"),
                       shape = c(8, 17, 15, 16, 18, 6))
defcolsh = data.frame(defense = c("any", "none", "all defended", "hairy", "tented", "rolled"), 
                       col = c(col = "red", "#D55E00", "#0072B2", "yellow3", "#CC79A7", "darkgreen"),
                       shape = c(8, 17, 15, 16, 18, 6))

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

#this adds julianweek or two week weekbin as options externally to meanDensity so that it is not inherently byWeek
srvyData = srvyData%>%
  # mutate(weekbin = bugsbyclayday)%>%
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
                       defense = "none",
                       ...)                  

{

  if(site=="all") {
    site = c("Eno River State Park",
    "Triangle Land Conservancy - Johnston Mill Nature Preserve",
    "Prairie Ridge Ecostation",
    "NC Botanical Garden",
    "UNC Chapel Hill Campus")}
  surveyData = surveyData%>%
    filter(Year %in% year, Name %in% site)
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
  
  if (defense == "none") {
    defFilter = timeFilter %>%
      filter(Hairy != 1, Tented != 1, Rolled != 1)
  }
  if (defense == "all defended") {
    defFilter = timeFilter %>%
      filter(Hairy == 1 | Tented == 1 | Rolled == 1)
  }
  if (defense == "hairy") {
    defFilter = timeFilter %>%
      filter(Hairy == 1)
  }
  if (defense == "tented") {
    defFilter = timeFilter %>%
      filter(Tented == 1)
  }
  if (defense == "rolled") {
    defFilter = timeFilter %>%
      filter(Rolled == 1)
  }
  if (defense == "any") {
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
                       ...)
  {
  arthCount = meanDensity(surveyData, year, site, ordersToInclude)
  color = sitecolsh$col[sitecolsh$site == site]
  
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
meanDensity()

catdif = function(surveyData = srvyData, yr1, yr2, site,
               plotVar = "fracSurveysdif",
               defense = "any", #or "none", "all" (all defended), "hairy", "tented", or "rolled"
                   new = T,
                   x_min = 130, x_max = 210,
                   ylim = c(-40, 40),
                   type = 'b',
                   shsize = 1,
                   axissize = 1,
                   labelsize = 1,
                   mainsize = 1,
                   main = paste(site, yr2, "-", yr1),
                   legcex = 0.5,
                   ...){
  catyr1 = meanDensity(surveyData, site = site, year = yr1, defense = defense)%>%ungroup()
  catyr2 = meanDensity(surveyData, site = site, year = yr2, defense = defense)%>%ungroup()
  yr2yr = left_join(catyr1, catyr2, by = join_by("julianweek"))%>%
    mutate(fracSurveysdif = fracSurveys.y - fracSurveys.x)%>%
    mutate(meanBiomassdif = meanBiomass.y - meanBiomass.x)%>%
    mutate(totalCountdif = totalCount.y - totalCount.x)
    
  color = defcolsh$col[defcolsh$defense == defense]
  shape = sitecolsh$shape[defcolsh$defense == defense]
  if(new == T){
    plot(yr2yr$julianweek, yr2yr$fracSurveysdif,
         xlab = "Date", ylab = "Change in Caterpillar Count", ylim = ylim, 
         xlim = c(x_min, x_max),
         xaxt = "n", 
         cex.axis = axissize, cex.lab = labelsize, type = type, 
         col = color, cex = shsize, pch = shape, lwd = 2,
         main = main, cex.main = 1)
    # legend("topleft", legend = defcolsh$defense,
    #        col = defcolsh$col,
    #        pch = defcolsh$shape,
    #        lwd = 2,
    #        cex = legcex)
  }
  if(new == F){
    points(yr2yr$julianweek, yr2yr$fracSurveysdif,
           type = type, col = color, pch = shape, cex = shsize, lwd = 2)
  }
  axis.Date(1, at = seq(x_min, x_max, by = 12), format = "%b %d", cex.axis = 1)
  return(yr2yr)
}

##ratio of defended to undefended
catratiodif = function(surveyData = srvyData, yr1, yr2, site,
                  plotVar = "fracSurveysdif",
                  defense = "all", #(all defended), or "hairy", "tented", or "rolled"
                  new = T,
                  x_min = 130, x_max = 210,
                  ylim = c(-40, 40),
                  type = 'b',
                  shsize = 1,
                  axissize = 1,
                  labelsize = 1,
                  mainsize = 1,
                  main = paste(site, yr2, "-", yr1),
                  legcex = 0.5,
                  ...){
  catyr1def = meanDensity(surveyData, site = site, year = yr1, defense = defense)%>%ungroup()%>%
    select(julianweek, totalCount, totalBiomass, meanDensity, fracSurveys, meanBiomass)
  catyr1undef = meanDensity(surveyData, site = site, year = yr1, defense = "none")%>%ungroup()%>%
    select(julianweek, totalCount, totalBiomass, meanDensity, fracSurveys, meanBiomass)
  catyr2def = meanDensity(surveyData, site = site, year = yr2, defense = defense)%>%ungroup()%>%
    select(julianweek, totalCount, totalBiomass, meanDensity, fracSurveys, meanBiomass)
  catyr2undef = meanDensity(surveyData, site = site, year = yr1, defense = "none")%>%ungroup()%>%
    select(julianweek, totalCount, totalBiomass, meanDensity, fracSurveys, meanBiomass)
  
  yr1ratio = left_join(catyr1def, catyr2undef, by = join_by("julianweek"))%>%
    mutate(ratio = fracSurveys.x/fracSurveys.y)
  yr2ratio = left_join(catyr2def, catyr2undef, by = join_by("julianweek"))%>%
    mutate(ratio = fracSurveys.x/fracSurveys.y)
  yr2yr = left_join(yr1ratio, yr2ratio, by = join_by("julianweek"))%>%
    mutate(ratiodif = ratio.y - ratio.x)%>%
    # mutate(meanBiomassdif = meanBiomass.y - meanBiomass.x)%>%
    # mutate(totalCountdif = totalCount.y - totalCount.x)
  
  color = defcolsh$col[defcolsh$defense == defense]
  shape = sitecolsh$shape[defcolsh$defense == defense]
  if(new == T){
    plot(yr2yr$julianweek, yr2yr$ratiodif,
         xlab = "Date", ylab = "Change in Caterpillar Count", ylim = ylim, 
         xlim = c(x_min, x_max),
         xaxt = "n", 
         cex.axis = axissize, cex.lab = labelsize, type = type, 
         col = color, cex = shsize, pch = shape, lwd = 2,
         main = main, cex.main = 1)
    # legend("topleft", legend = defcolsh$defense,
    #        col = defcolsh$col,
    #        pch = defcolsh$shape,
    #        lwd = 2,
    #        cex = legcex)
  }
  if(new == F){
    points(yr2yr$julianweek, yr2yr$ratiodif,
           type = type, col = color, pch = shape, cex = shsize, lwd = 2)
  }
  axis.Date(1, at = seq(x_min, x_max, by = 12), format = "%b %d", cex.axis = 1)
  return(yr2yr)
}

"Eno River State Park"
"Triangle Land Conservancy - Johnston Mill Nature Preserve"
"Prairie Ridge Ecostation"
"NC Botanical Garden"
"UNC Chapel Hill Campus"
sitedif = function(yr1, yr2, site){
  catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "any", new = T)
  catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "all defended", new = F)
  catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "none", new = F)
  catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "hairy", new = F)
  catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "tented", new = F)
  catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "rolled", new = F)
}
siteratiodif = function(yr1, yr2, site){
  catratiodif(yr1 = yr1, yr2 = yr2, site = site, defense = "any", new = T)
  catratiodif(yr1 = yr1, yr2 = yr2, site = site, defense = "all defended", new = F)
  catratiodif(yr1 = yr1, yr2 = yr2, site = site, defense = "none", new = F)
  catratiodif(yr1 = yr1, yr2 = yr2, site = site, defense = "hairy", new = F)
  catratiodif(yr1 = yr1, yr2 = yr2, site = site, defense = "tented", new = F)
  catratiodif(yr1 = yr1, yr2 = yr2, site = site, defense = "rolled", new = F)
}
par(mfrow = c(2, 3), mar = c(6, 5, 4, 1))

siteratiodif(yr1 = 2024, yr2 = 2025, site = "Eno River State Park")
sitedif(yr1 = 2024, yr2 = 2025, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve")
sitedif(yr1 = 2024, yr2 = 2025, site = "Prairie Ridge Ecostation")
sitedif(yr1 = 2024, yr2 = 2025, site = "NC Botanical Garden")
sitedif(yr1 = 2024, yr2 = 2025, site = "UNC Chapel Hill Campus")
