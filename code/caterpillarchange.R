library(dplyr)
library(lubridate)
library(data.table)
library(gsheet)
library(maps)
library(sf)
library(stringr)

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
  filter(Year %in% c(2021:2025), Name %in% c("Eno River State Park",
                                             "Triangle Land Conservancy - Johnston Mill Nature Preserve",
                                             "Prairie Ridge Ecostation",
                                             "NC Botanical Garden",
                                             "UNC Chapel Hill Campus"))%>%
  mutate(julianweek = 7*floor(julianday/7) + 4)

#meanDensity is similar to meanDensityByWeek, but is able to take defense as an argument
Mode = function(x){ 
  if (!is.numeric(x)) {
    stop("values must be numeric for mode calculation")
  }
  ta = table(x)
  tam = max(ta)
  mod = as.numeric(names(ta)[ta == tam])
  return(max(mod))
}

meanDensity = function(surveyData = srvyData,
                       year = 2024,
                       site = "all",
                       ordersToInclude = 'All',
                       
                       minLength = 0,         
                       jdRange = c(1,365),
                       outlierCount = 10000,
                       minSurveyCoverage = 0.8, 
                       allDates = FALSE,
                       defense = "any",
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
           okWeek = ifelse(nSurveyBranches/modalBranchesSurveyed >= minSurveyCoverage, 1, 0))
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
  
  points(arthCount$julianweek, arthCount[, plotVar], type = 'l', col = color, ...)
  points(arthCount$julianweek, arthCount[, plotVar], pch = 16, col = color, ...)
  legend("topright", legend = sitecolsh$site,
         col = sitecolsh$col,
         lwd = 2,
         cex = 1)
}


##plot initializer - this creates an empty plot and removes the need for new = T/F (everything will be new = F)
PLOT = function(x_min = 130, x_max = 210, ...){plot(0, 0,
     xlim = c(x_min, x_max),
     xaxt = "n",
     type = "b", cex = 0, lwd = 0, ...)
axis.Date(1, at = seq(x_min, x_max, by = 15), format = "%b %d", ...)
}
PLOT(xlab = "Date", ylab = "Caterpillar Count", cex.axis = 1, cex.lab = 1)


#taking the difference between years
catdif = function(surveyData = srvyData, yr1, yr2, site,
               plotVar = "fracSurveysdif",
               defense = "any", #or "none", "all defended", "hairy", "tented", or "rolled"
               legend = FALSE,
               legcex = 1,
               ...){
  catyr1 = meanDensity(surveyData, site = site, year = yr1, defense = defense)%>%ungroup()
  catyr2 = meanDensity(surveyData, site = site, year = yr2, defense = defense)%>%ungroup()
  yr2yr = left_join(catyr1, catyr2, by = join_by("julianweek"))%>%
    mutate(fracSurveysdif = fracSurveys.y - fracSurveys.x)%>%
    mutate(meanBiomassdif = meanBiomass.y - meanBiomass.x)%>%
    mutate(totalCountdif = totalCount.y - totalCount.x)
    
  color = defcolsh$col[defcolsh$defense == defense]
  shape = defcolsh$shape[defcolsh$defense == defense]
  points(yr2yr$julianweek, yr2yr[, plotVar],
         type = 'b', col = color, cex = 1, pch = shape, lwd = 2)
  abline(h = 0, lty = 3)
  if(legend){legend("topright", legend = defcolsh$defense,
         col = defcolsh$col,
         pch = defcolsh$shape,
         lwd = 2,
         cex = legcex)}
  return(yr2yr)
}

##ratio of defended to undefended
catratio = function(surveyData = srvyData, site, yr,
                    plotVar = "fracratio",
                    defense = "all defended",
                    legend = "site",
                    legcex = 1.5, ...){
  def = meanDensity(surveyData, site = site, year = yr, defense = defense)%>%ungroup()%>%
    select(julianweek, meanDensity, fracSurveys, meanBiomass)
  undef = meanDensity(surveyData, site = site, year = yr, defense = "none")%>%ungroup()%>%
    select(julianweek, meanDensity, fracSurveys, meanBiomass)
  
  defratio = left_join(def, undef, by = join_by("julianweek"))%>%
    mutate(fracratio = fracSurveys.x/fracSurveys.y)%>%
    mutate(biomassratio = meanBiomass.x/meanBiomass.y)%>%
    mutate(densityratio = meanDensity.x/meanDensity.y)
  color = defcolsh$col[defcolsh$defense == defense]
  shape = sitecolsh$shape[defcolsh$defense == defense]

  if(legend == "def"){
    color = defcolsh$col[defcolsh$defense == defense]
    shape = sitecolsh$shape[defcolsh$defense == defense]
    legend("topright", legend = defcolsh$defense,
           col = defcolsh$col,
           pch = defcolsh$shape,
           lwd = 2,
           cex = legcex)}
  if(legend == "site"){
    color = sitecolsh$col[sitecolsh$site == site]
    shape = sitecolsh$shape[sitecolsh$site == site]
    legend("topright", legend = sitecolsh$site,
           col = sitecolsh$col,
           pch = sitecolsh$shape,
           lwd = 2,
           cex = legcex)}
  points(defratio$julianweek, defratio[, plotVar],
         type = 'b', col = color, cex = 1, pch = shape, lwd = 2)
  return(defratio)
}

##next time: take difference
catratiodif = function(surveyData = srvyData, yr1, yr2, site,
                  plotVar = "fracSurveysdif",
                  defense = "all defended", #"hairy", "tented", or "rolled"
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
  
  yr1ratio = left_join(catyr1def, catyr1undef, by = join_by("julianweek"))%>%
    mutate(ratio = fracSurveys.x/fracSurveys.y)
  yr2ratio = left_join(catyr2def, catyr2undef, by = join_by("julianweek"))%>%
    mutate(ratio = fracSurveys.x/fracSurveys.y)
  yr2yr = left_join(yr1ratio, yr2ratio, by = join_by("julianweek"))%>%
    mutate(ratiodif = ratio.y - ratio.x)#%>%
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

#execute plotting
"Eno River State Park"
"Triangle Land Conservancy - Johnston Mill Nature Preserve"
"Prairie Ridge Ecostation"
"NC Botanical Garden"
"UNC Chapel Hill Campus"
##graphing parameters can be passed into PLOT, catdif, or catratio
##plots density
sitedensity = function(ylim = c(0, 50), main, sites = c(1,1,1,1,1,1), yr, ordersToInclude = "caterpillar"){
  PLOT(ylim = ylim, xlab = "Date", ylab = "fracSurveys", main = main, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  if(sites[1]==1) plotDensity(year = yr, site = "Eno River State Park", ordersToInclude = ordersToInclude)
  if(sites[2]==1) plotDensity(year = yr, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", ordersToInclude = ordersToInclude)
  if(sites[3]==1) plotDensity(year = yr, site = "Prairie Ridge Ecostation", ordersToInclude = ordersToInclude)
  if(sites[4]==1) plotDensity(year = yr, site = "NC Botanical Garden", ordersToInclude = ordersToInclude)
  if(sites[5]==1) plotDensity(year = yr, site = "UNC Chapel Hill Campus", ordersToInclude = ordersToInclude)
  if(sites[6]==1) plotDensity(year = yr, site = "all", ordersToInclude = ordersToInclude)
}
##plots density difference between two years
sitecatdif = function(ylim = c(-25, 25), main, def = c(1,1,1,1,1,1), yr1, yr2, site, legend = F){
  PLOT(ylim = ylim, xlab = "Date", ylab = "Caterpillar Count", main = main, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  if(def[1]==1) catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "any", legend = legend)
  if(def[2]==1) catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "all defended")
  if(def[3]==1) catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "none")
  if(def[4]==1) catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "hairy")
  if(def[5]==1) catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "tented")
  if(def[6]==1) catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "rolled")
}

##plots ratios for multiple defense types for one site
sitecatratio = function(ylim = c(0, 5), main, def = c(1,1,1,1), yr, site, legend = "def"){
  PLOT(ylim = ylim, xlab = "Date", ylab = "Defended/Undefended Caterpillar Ratio", main = main, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  if(def[1]==1) catratio(yr = yr, site = site, defense = "all defended", legend = legend)
  if(def[2]==1) catratio(yr = yr, site = site, defense = "hairy")
  if(def[3]==1) catratio(yr = yr, site = site, defense = "tented")
  if(def[4]==1) catratio(yr = yr, site = site, defense = "rolled")
}

##plots ratio for one defense type for all sites, probably more useful
sitescatratio = function(ylim = c(0, 0.5), main, sites = c(1,1,1,1,1,1), yr, defense = defense, legend = "site"){
  PLOT(ylim = ylim, xlab = "Date", ylab = "Defended/Undefended Caterpillar Ratio", main = main, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  if(sites[1]==1) catratio(yr = yr, site = "Eno River State Park", defense = defense, legend = legend, legcex = 1)
  if(sites[2]==1) catratio(yr = yr, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", defense = defense)
  if(sites[3]==1) catratio(yr = yr, site = "Prairie Ridge Ecostation", defense = defense)
  if(sites[4]==1) catratio(yr = yr, site = "NC Botanical Garden", defense = defense)
  if(sites[5]==1) catratio(yr = yr, site = "UNC Chapel Hill Campus", defense = defense)
  if(sites[6]==1) catratio(yr = yr, site = "all", defense = defense)
}

##plots ratio difference between two years


sitecatdif(main = "2025-2024 at Eno", def = c(1,1,1,0,0,0), yr1 = 2024, yr2 = 2025, site = "Eno River State Park", legend = T)
sitecatdif(main = "2025-2024 at JM", def = c(1,1,1,0,0,0), yr1 = 2024, yr2 = 2025, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve")
sitecatdif(main = "2025-2024 at Prairie Ridge", def = c(1,1,1,0,0,0), yr1 = 2024, yr2 = 2025, site = "Prairie Ridge Ecostation")
sitecatdif(main = "2025-2024 at NCBG", def = c(1,1,1,0,0,0), yr1 = 2024, yr2 = 2025, site = "NC Botanical Garden")
sitecatdif(ylim = c(-25, 40), main = "2025-2024 at UNC", def = c(1,1,1,0,0,0), yr1 = 2024, yr2 = 2025, site = "UNC Chapel Hill Campus")

sitecatdif(main = "2024-pre2024 at Eno", def = c(1,1,1,0,0,0), yr1 = c(2021:2023), yr2 = 2024, site = "Eno River State Park", legend = T)
sitecatdif(main = "2024-pre2024 at JM", def = c(1,1,1,0,0,0), yr1 = c(2021:2023), yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve")
sitecatdif(main = "2024-pre2024 at Prairie Ridge", def = c(1,1,1,0,0,0), yr1 = c(2021:2023), yr2 = 2024, site = "Prairie Ridge Ecostation")
sitecatdif(main = "2024-pre2024 at NCBG", def = c(1,1,1,0,0,0), yr1 = c(2021:2023), yr2 = 2024, site = "NC Botanical Garden")
sitecatdif(ylim = c(-30, 25), main = "2024-pre2024 at UNC", def = c(1,1,1,0,0,0), yr1 = c(2021:2023), yr2 = 2024, site = "UNC Chapel Hill Campus")


sitescatratio(main = "Defended/Undefended ratio in 2024", yr = 2024)
sitescatratio(main = "Defended/Undefended ratio in 2025", yr = 2025)

sitedensity(ylim = c(0,10), main = "Moth Density 2025", yr = 2025, ordersToInclude = "moths")
