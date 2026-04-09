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
defcolsh = data.frame(defense = c("all caterpillars", "undefended", "all defended", "hairy", "tented", "rolled"), 
                       col = c(col = "red", "#D55E00", "#0072B2", "yellow3", "#CC79A7", "darkgreen"),
                       shape = c(19, 17, 15, 8, 2, 1))

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

sizehist = function(data = srvyData, main, taxon = "caterpillar", dayrange = c(130:210), year = c(2021:2025),
                    site = c("Eno River State Park",
                             "Triangle Land Conservancy - Johnston Mill Nature Preserve",
                             "Prairie Ridge Ecostation",
                             "NC Botanical Garden",
                             "UNC Chapel Hill Campus")){
  data = data%>%
    filter(Group %in% taxon, julianday %in% dayrange, Year %in% year, Name %in% site)
  xmax = max(data$Length)
  hist(data$Length, main = main, xlab = "Caterpillar Size (mm)", ylab = "Quantity", col = "green3", border = "lightgray")
  abline(v = mean(data$Length, na.rm = T), col = "navy", lwd = 2)
  abline(v = median(data$Length, na.rm = T), col = "navy", lwd = 2, lty = 3)
}

sizehist(main = "Eno pre-2024", year = 2021:2023, site = "Eno River State Park")
sizehist(main = "JM pre-2024", year = 2021:2023, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve")
sizehist(main = "PR pre-2024", year = 2021:2023, site = "Prairie Ridge Ecostation")
sizehist(main = "NCBG pre-2024", year = 2021:2023, site = "NC Botanical Garden")
sizehist(main = "UNC pre-2024", year = 2021:2023, site = "UNC Chapel Hill Campus")
sizehist(main = "Eno 2024", year = 2024, site = "Eno River State Park")
sizehist(main = "JM 2024", year = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve")
sizehist(main = "PR 2024", year = 2024, site = "Prairie Ridge Ecostation")
sizehist(main = "NCBG 2024", year = 2024, site = "NC Botanical Garden")
sizehist(main = "UNC 2024", year = 2024, site = "UNC Chapel Hill Campus")
sizehist(main = "Eno 2025", year = 2025, site = "Eno River State Park")
sizehist(main = "JM 2025", year = 2025, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve")
sizehist(main = "PR 2025", year = 2025, site = "Prairie Ridge Ecostation")
sizehist(main = "NCBG 2025", year = 2025, site = "NC Botanical Garden")
sizehist(main = "UNC 2025", year = 2025, site = "UNC Chapel Hill Campus")

#meanDensity is similar to meanDensityByWeek, but is able to take defense as an argument
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

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
                       defense = "all arthropods",
                       
                       
                       lengthRange = c(0, 100),         
                       jdRange = c(1,365),
                       outlierCount = 10000,
                       minSurveyCoverage = 0.8, 
                       allDates = FALSE,
                       ...)                  

{
  Site = site
  if(Site=="all") {
    Site = c("Eno River State Park",
    "Triangle Land Conservancy - Johnston Mill Nature Preserve",
    "Prairie Ridge Ecostation",
    "NC Botanical Garden",
    "UNC Chapel Hill Campus")}
  surveyData = surveyData%>%
    filter(Year %in% year, Name %in% Site)
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
  weeks = data.frame(julianweek = seq(from = min(effortByWeek$julianweek), to = max(effortByWeek$julianweek), by = 7)) %>%
    left_join(effortByWeek, by = "julianweek") %>%
    select(julianweek, okWeek)
    
  
  if (defense == "undefended") {
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
  if (defense == "all caterpillars" | defense == "all arthropods") {
    defFilter = timeFilter
  }

  arthCount = defFilter %>%
    filter(Length >= lengthRange[1] & Length <= lengthRange[2], 
           Group %in% ordersToInclude) %>%
    mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% 
    group_by(julianweek) %>%
    summarize(totalCount = sum(Quantity2, na.rm = TRUE),
              numSurveysGTzero = length(unique(ID[Quantity > 0])),
              totalBiomass = sum(Biomass_mg, na.rm = TRUE)) %>%
    right_join(weeks, by = "julianweek") %>%
    arrange(julianweek) %>%
    select(-c(okWeek)) %>% 
    mutate(Name = rep(site, times = nrow(.))) %>%
    left_join(effortByWeek, by = "julianweek") %>%
    mutate_cond(is.na(okWeek), okWeek = 0) %>%
    mutate_cond(is.na(totalCount) & okWeek == 1, totalCount = 0, numSurveysGTzero = 0, totalBiomass = 0, nSurveys = 1) %>%
    mutate(meanDensity = totalCount/nSurveys,
           fracSurveys = 100*numSurveysGTzero/nSurveys,
           meanBiomass = totalBiomass/nSurveys)
  return(arthCount)
}

plotDensity = function(surveyData = srvyData,
                       year = 2024,
                       site = "all",
                       ordersToInclude = "All",
                       defense = "all caterpillars",
                       lengthRange = c(0, 100),
                       plotVar = "fracSurveys",
                       color = "default",
                       colsh = "def",
                       ...)
  {
  arthCount = meanDensity(surveyData, year, site, ordersToInclude, defense, lengthRange) %>%
    as.data.frame()
  if(color == "default"){
    if(colsh == "site"){color = sitecolsh$col[sitecolsh$site == site]}
    if(colsh == "def"){color = defcolsh$col[defcolsh$defense == defense]}
  }
  else{color = color}
  
  if(colsh == "site"){shape = sitecolsh$shape[sitecolsh$site == site]}
  if(colsh == "def"){shape = defcolsh$shape[defcolsh$defense == defense]}
  points(arthCount$julianweek, arthCount[, plotVar], type = 'l', col = color, ...)
  points(arthCount$julianweek, arthCount[, plotVar], pch = shape, col = color, ...)
  }


##plot initializer - this creates an empty plot and removes the need for new = T/F (everything will be new = F)
PLOT = function(x_min = 130, x_max = 210, ...){plot(0, 0,
     xlim = c(x_min, x_max),
     xaxt = "n",
     type = "b", cex = 0, lwd = 0, ...)
axis.Date(1, at = seq(x_min, x_max, by = 14), format = "%b %d", ...)
}
PLOT(ylim = c(0,1), xlab = "Date", ylab = "Caterpillar Count", cex.axis = 1, cex.lab = 1)


#taking the difference between years
catdif = function(surveyData = srvyData, yr1, yr2, site,
               plotVar = "fracSurveysdif",
               defense = "all caterpillars", #or "undefended", "all defended", "hairy", "tented", or "rolled"
               lengthRange = c(0, 100), connected = F,
               offset = 0,
               ...){
  catyr1 = meanDensity(surveyData, year = yr1, site = site, ordersToInclude = "caterpillar", defense = defense, lengthRange = lengthRange)%>%ungroup()%>%
    select(julianweek, Name, meanDensity, fracSurveys, meanBiomass)
  catyr2 = meanDensity(surveyData, year = yr2, site = site, ordersToInclude = "caterpillar", defense = defense, lengthRange = lengthRange)%>%ungroup()%>%
    select(julianweek, Name, meanDensity, fracSurveys, meanBiomass)%>%
    mutate(julianweek = julianweek + offset)
  yr2yr = left_join(catyr1, catyr2, by = join_by("julianweek", "Name")) %>%
    mutate(fracSurveysdif = fracSurveys.y - fracSurveys.x) %>%
    mutate(meanBiomassdif = meanBiomass.y - meanBiomass.x) %>%
    mutate(meanDensitydif = meanDensity.y - meanDensity.x) %>%
    #prevent from being a tibble |>
    as.data.frame()
  
  if(connected == T){yr2yr = filter(yr2yr, is.na(fracSurveysdif) == F)}

  color = defcolsh$col[defcolsh$defense == defense]
  shape = defcolsh$shape[defcolsh$defense == defense]
  points(yr2yr$julianweek, 
         yr2yr[, plotVar],
         type = 'b', col = color, pch = shape, ...)
  abline(h = 0, lty = 3)
  return(yr2yr)
}

##ratio of defended to undefended
catratio = function(surveyData = srvyData, site, yr,
                    plotVar = "fracratio",
                    defense = "all defended",
                    lengthRange = c(0, 100),
                    ...){
  def = meanDensity(surveyData, site = site, year = yr, ordersToInclude = "caterpillar", defense = defense, lengthRange = lengthRange)%>%ungroup()%>%
    select(julianweek, Name, meanDensity, fracSurveys, meanBiomass)
  undef = meanDensity(surveyData, site = site, year = yr, ordersToInclude = "caterpillar", defense = "undefended", lengthRange = lengthRange)%>%ungroup()%>%
    select(julianweek, Name, meanDensity, fracSurveys, meanBiomass)
  
  defratio = left_join(def, undef, by = join_by("julianweek", "Name")) %>%
    mutate(fracratio = fracSurveys.x/fracSurveys.y) %>%
    mutate(biomassratio = meanBiomass.x/meanBiomass.y) %>%
    mutate(densityratio = meanDensity.x/meanDensity.y) %>%
    as.data.frame()
    
  color = sitecolsh$col[sitecolsh$site == site]
  shape = sitecolsh$shape[sitecolsh$site == site]

  points(defratio$julianweek, defratio[, plotVar],
         type = 'b', col = color, pch = shape, lwd = 2, ...)
  abline(h = 1, lty = 3)
  return(defratio)
}

##
catratiodif = function(yr1, yr2, site,
                  plotVar = "fracratiodif",
                  defense = "all defended", #"hairy", "tented", or "rolled"
                  lengthRange = c(0, 100)){
  yr1ratio = catratio(site = site, yr = yr1,
                      defense = defense,
                      lengthRange = lengthRange)
  yr2ratio = catratio(site = site, yr = yr2,
                      defense = defense,
                      lengthRange = lengthRange)
  yr2yr = left_join(yr1ratio, yr2ratio, by = "julianweek")%>%
    mutate(fracratiodif = fracratio.y - fracratio.x)%>%
    mutate(biomassratiodif = biomassratio.y - biomassratio.x)%>%
    mutate(densityratiodif = densityratio.y - densityratio.x)
  
  color = sitecolsh$col[sitecolsh$site == site]
  shape = sitecolsh$shape[sitecolsh$site == site]
  points(yr2yr$julianweek, yr2yr[, plotVar],
           type = 'b', col = color, pch = shape, cex = 1, lwd = 2)
  abline(h = 0, lty = 3)
  return(yr2yr)
}

#execute plotting

##graphing parameters can be passed into PLOT, catdif, or catratio
##plots density
sitesdensity = function(ylim = c(0, 50), main, sites = c(1,1,1,1,1,1), yr, ordersToInclude = "caterpillar", defense = "all caterpillars", lengthRange = c(0, 100)){
  PLOT(ylim = ylim, xlab = "Date", ylab = "fracSurveys", main = main, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  if(sites[1]==1) plotDensity(year = yr, site = "Eno River State Park", ordersToInclude = ordersToInclude, defense = defense, lengthRange = lengthRange)
  if(sites[2]==1) plotDensity(year = yr, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", ordersToInclude = ordersToInclude, defense = defense, lengthRange = lengthRange)
  if(sites[3]==1) plotDensity(year = yr, site = "Prairie Ridge Ecostation", ordersToInclude = ordersToInclude, defense = defense, lengthRange = lengthRange)
  if(sites[4]==1) plotDensity(year = yr, site = "NC Botanical Garden", ordersToInclude = ordersToInclude, defense = defense, lengthRange = lengthRange)
  if(sites[5]==1) plotDensity(year = yr, site = "UNC Chapel Hill Campus", ordersToInclude = ordersToInclude, defense = defense, lengthRange = lengthRange)
  if(sites[6]==1) plotDensity(year = yr, site = "all", ordersToInclude = ordersToInclude, defense = defense, lengthRange = lengthRange)
}

##plots density difference between two years
sitecatdif = function(new = T, ylim = c(-25, 25), main, def = c(1,1,1,1,1,1), yr1, yr2, site, lengthRange = c(0, 100), offset = 0, ...){
  if(yr1 == "pre-2024"){yr1 = c(2021:2023)}
  if(yr2 == "pre-2024"){yr2 = c(2021:2023)}
  if(new) PLOT(ylim = ylim, xlab = "Date", ylab = "Change in Caterpillar Count", main = main, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  if(def[1]==1) catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "all caterpillars", lengthRange = lengthRange, offset = offset, ...)
  if(def[2]==1) catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "undefended", lengthRange = lengthRange, offset = offset, ...)
  if(def[3]==1) catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "all defended", lengthRange = lengthRange, offset = offset, ...)
  if(def[4]==1) catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "hairy", lengthRange = lengthRange, offset = offset, ...)
  if(def[5]==1) catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "tented", lengthRange = lengthRange, offset = offset, ...)
  if(def[6]==1) catdif(yr1 = yr1, yr2 = yr2, site = site, defense = "rolled", lengthRange = lengthRange, offset = offset, ...)
}
##plots density by defense
ddcolsh = data.frame(by = c("pre-2024", 2024, 2025), col = c("gray87", "gray70", "gray45"), shape = c(20,20,20))
defdensity = function(new = T, ylim = c(0, 50), ylab, main, def = c(1,1,1,1,1,1), yr, site, ordersToInclude = "caterpillar", defense = "all caterpillars", lengthRange = c(0, 100), colsh = "def", ...){
  color = ddcolsh$col[ddcolsh$by == yr]
  if(yr == "pre-2024"){yr = c(2021:2023)}
  if(new) PLOT(ylim = ylim, xlab = "Date", ylab = ylab, main = main, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  if(def[1]==1) plotDensity(color = color, year = yr, site = site, ordersToInclude = ordersToInclude, defense = "all caterpillars", lengthRange = lengthRange, colsh = colsh, ...)
  if(def[2]==1) plotDensity(color = color, year = yr, site = site, ordersToInclude = ordersToInclude, defense = "undefended", lengthRange = lengthRange, colsh = colsh, ...)
  if(def[3]==1) plotDensity(color = color, year = yr, site = site, ordersToInclude = ordersToInclude, defense = "all defended", lengthRange = lengthRange, colsh = colsh,  ...)
  if(def[4]==1) plotDensity(color = color, year = yr, site = site, ordersToInclude = ordersToInclude, defense = "hairy", lengthRange = lengthRange, colsh = colsh, ...)
  if(def[5]==1) plotDensity(color = color, year = yr, site = site, ordersToInclude = ordersToInclude, defense = "tented", lengthRange = lengthRange, colsh = colsh, ...)
  if(def[6]==1) plotDensity(color = color, year = yr, site = site, ordersToInclude = ordersToInclude, defense = "rolled", lengthRange = lengthRange, colsh = colsh, ...)
}


##plots ratios for multiple defense types for one site
# sitecatratio = function(ylim = c(0, 5), main, def = c(1,1,1,1), yr, site, lengthRange = c(0, 100)){
#   PLOT(ylim = ylim, xlab = "Date", ylab = "Defended/Undefended Caterpillar Ratio", main = main, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
#   if(def[1]==1) catratio(yr = yr, site = site, defense = "all defended", lengthRange = lengthRange)
#   if(def[2]==1) catratio(yr = yr, site = site, defense = "hairy", lengthRange = lengthRange)
#   if(def[3]==1) catratio(yr = yr, site = site, defense = "tented", lengthRange = lengthRange)
#   if(def[4]==1) catratio(yr = yr, site = site, defense = "rolled", lengthRange = lengthRange)
# }
##plots ratio for one defense type for all sites, probably more useful
sitescatratio = function(ylim = c(0, 1), main, sites = c(1,1,1,1,1,1), plotVar = "fracratio", yr, defense = "all defended", lengthRange = c(0, 100), ...){
  PLOT(ylim = ylim, xlab = "Date", ylab = "Defended/Undefended Caterpillar Ratio", main = main, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  if(sites[1]==1) catratio(yr = yr, site = "Eno River State Park", plotVar = plotVar, defense = defense, lengthRange = lengthRange, ...)
  if(sites[2]==1) catratio(yr = yr, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", plotVar = plotVar, defense = defense, lengthRange = lengthRange, ...)
  if(sites[3]==1) catratio(yr = yr, site = "Prairie Ridge Ecostation", plotVar = plotVar, defense = defense, lengthRange = lengthRange, ...)
  if(sites[4]==1) catratio(yr = yr, site = "NC Botanical Garden", plotVar = plotVar, defense = defense, lengthRange = lengthRange, ...)
  if(sites[5]==1) catratio(yr = yr, site = "UNC Chapel Hill Campus", plotVar = plotVar, defense = defense, lengthRange = lengthRange, ...)
  if(sites[6]==1) catratio(yr = yr, site = "all", plotVar = plotVar, defense = defense, lengthRange = lengthRange, ...)
}

##plots ratio difference between two years
sitescatratiodif = function(ylim = c(-0.2, 2), main, sites = c(1,1,1,1,1,1), yr1, yr2, defense = "all defended", lengthRange = c(0, 100)){
  PLOT(ylim = ylim, xlab = "Date", ylab = "Difference in Defended/Undefended Caterpillar Ratio", main = main, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  if(sites[1]==1) catratiodif(yr1 = yr1, yr2 = yr2, site = "Eno River State Park", defense = defense, lengthRange = lengthRange)
  if(sites[2]==1) catratiodif(yr1 = yr1, yr2 = yr2, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = lengthRange, defense = defense)
  if(sites[3]==1) catratiodif(yr1 = yr1, yr2 = yr2, site = "Prairie Ridge Ecostation", lengthRange = lengthRange, defense = defense)
  if(sites[4]==1) catratiodif(yr1 = yr1, yr2 = yr2, site = "NC Botanical Garden", lengthRange = lengthRange, defense = defense)
  if(sites[5]==1) catratiodif(yr1 = yr1, yr2 = yr2, site = "UNC Chapel Hill Campus", lengthRange = lengthRange, defense = defense)
  if(sites[6]==1) catratiodif(yr1 = yr1, yr2 = yr2, site = "all", lengthRange = lengthRange, defense = defense)
  }


#subset should be 6 (0/not 1 or 1) numbers if dd = F, and 9 if dd = T
addlegend = function(x = "topleft", colsh = "def", dd = F, subset = c(1,1,1,1,1,1), ...){
  if(dd == T){defdensitycolsh = ddcolsh}
  else{defdensitycolsh = data.frame()}
  if(colsh == "def"){
    legendcolsh = defcolsh %>%
      rename(by = defense) %>%
      rbind(defdensitycolsh) %>%
      cbind(subset) %>%
      filter(subset == 1) %>%
      select(by, col, shape)
    }
  if(colsh == "site"){
    legendcolsh = sitecolsh %>%
      rename(by = site) %>%
      rbind(defdensitycolsh) %>%
      cbind(subset) %>%
      filter(subset == 1) %>%
      select(by, col, shape)
  }
  legend(x, legend = legendcolsh$by,
         col = legendcolsh$col,
         pch = legendcolsh$shape,
         lwd = 2,
         ...)
}



scd = function(ylim = c(-25, 25), ylab = "Change in Caterpillar Count", main, def = c(1,1,1,0,0,0), dd = T, yr1, yr2, site, lengthRange, offset = 0, legend = F, legcex = 1.2, ...){
  ddy = c()
  if(dd == T){
    ddy[1] = ifelse(yr1 == "pre-2024" | yr2 == "pre-2024", 1, 0)
    ddy[2] = ifelse(yr1 == 2024 | yr2 == 2024, 1, 0)
    ddy[3] = ifelse(yr1 == 2025 | yr2 == 2025, 1, 0)
    defdensity(ylim = ylim, ylab = ylab, def = def, main = main, yr = yr1, site = site, lengthRange = lengthRange, lty = 2, ...)
    defdensity(new = F, def = def, yr = yr2, site = site, lengthRange = lengthRange, lty = 2, ...)
    sitecatdif(new = F, def = def, yr1 = yr1, yr2 = yr2, site = site, lengthRange = lengthRange, offset = offset, ...)
    }
  if(dd == F) sitecatdif(ylim = ylim, ylab = ylab, main = main, def = def, yr1 = yr1, yr2 = yr2, site = site, lengthRange = lengthRange, offset = offset, ...)
  if(legend == T) addlegend(dd = dd, subset = c(def, ddy), pt.cex = 2, cex = legcex, y.intersp = 0.5)
}


#all caterpillars 2024-2025
par(mfrow = c(3, 2), mar = c(6, 5, 4, 1))

scd(ylim = c(-15, 25), main = "2024-2025 at Eno", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Eno River State Park", lengthRange = c(0, 100), legend = T, cex = 2)
scd(ylim = c(-10, 25), main = "2024-2025 at JM", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(0, 100), cex = 2)
scd(ylim = c(-25, 35), main = "2024-2025 at Prairie Ridge", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Prairie Ridge Ecostation", lengthRange = c(0, 100), cex = 2)
scd(ylim = c(-5, 10), main = "2024-2025 at NCBG", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "NC Botanical Garden", lengthRange = c(0, 100), cex = 2)
scd(ylim = c(-5, 10), main = "2024-2025 at UNC", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "UNC Chapel Hill Campus", lengthRange = c(0, 100), cex = 2)

#small caterpillars 2025-2024
par(mfrow = c(3, 2), mar = c(6, 5, 4, 1))

scd(ylim = c(-10, 20), main = "2024-2025 at Eno", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Eno River State Park", lengthRange = c(0, 10), legend = T, cex = 2)
scd(ylim = c(-10, 20), main = "2024-2025 at JM", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(0, 10), cex = 2)
scd(ylim = c(-15, 20), main = "2024-2025 at Prairie Ridge", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Prairie Ridge Ecostation", lengthRange = c(0, 10), cex = 2)
scd(ylim = c(-5, 10), main = "2024-2025 at NCBG", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "NC Botanical Garden", lengthRange = c(0, 10), cex = 2)
scd(ylim = c(-10, 10), main = "2024-2025 at UNC", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "UNC Chapel Hill Campus", lengthRange = c(0, 10), cex = 2)

#large caterpillars 2025-2024
par(mfrow = c(3, 2), mar = c(6, 5, 4, 1))

scd(ylim = c(-5, 15), main = "2024-2025 at Eno", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Eno River State Park", lengthRange = c(10, 10), legend = T, cex = 2)
scd(ylim = c(-10, 15), main = "2024-2025 at JM", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(10, 10), cex = 2)
scd(ylim = c(-5, 5), main = "2024-2025 at Prairie Ridge", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Prairie Ridge Ecostation", lengthRange = c(10, 10), cex = 2)
scd(ylim = c(-5, 5), main = "2024-2025 at NCBG", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "NC Botanical Garden", lengthRange = c(10, 10), cex = 2)
scd(ylim = c(-5, 5), main = "2024-2025 at UNC", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "UNC Chapel Hill Campus", lengthRange = c(10, 10), cex = 2)


#all caterpillars 2024-pre2024
par(mfrow = c(3, 2), mar = c(6, 5, 4, 1))

scd(ylim = c(-15, 25), main = "2024-pre2024 at Eno", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Eno River State Park", lengthRange = c(0, 100), legend = T, cex = 2)
scd(ylim = c(-5, 25), main = "2024-pre2024 at JM", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(0, 100), cex = 2)
scd(ylim = c(-5, 15), main = "2024-pre2024 at Prairie Ridge", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Prairie Ridge Ecostation", lengthRange = c(0, 100), cex = 2)
scd(ylim = c(-5, 10), main = "2024-pre2024 at NCBG", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "NC Botanical Garden", lengthRange = c(0, 100), cex = 2)
scd(ylim = c(-5, 5), main = "2024-pre2024 at UNC", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "UNC Chapel Hill Campus", lengthRange = c(0, 100), cex = 2)

#small caterpillars 2024-pre2024
par(mfrow = c(3, 2), mar = c(6, 5, 4, 1))

scd(ylim = c(-15, 20), main = "2024-pre2024 at Eno", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Eno River State Park", lengthRange = c(0, 10), legend = T, cex = 2)
scd(ylim = c(-5, 25), main = "2024-pre2024 at JM", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(0, 100), cex = 2)
scd(ylim = c(-5, 15), main = "2024-pre2024 at Prairie Ridge", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Prairie Ridge Ecostation", lengthRange = c(0, 10), cex = 2)
scd(ylim = c(-5, 10), main = "2024-pre2024 at NCBG", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "NC Botanical Garden", lengthRange = c(0, 10), cex = 2)
scd(ylim = c(-5, 5), main = "2024-pre2024 at UNC", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "UNC Chapel Hill Campus", lengthRange = c(0, 10), cex = 2)

#large caterpillars 2024-pre2024
par(mfrow = c(3, 2), mar = c(6, 5, 4, 1))

scd(ylim = c(-5, 15), main = "2024-pre2024 at Eno", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Eno River State Park", lengthRange = c(10, 100), legend = T, cex = 2)
scd(ylim = c(-5, 15), main = "2024-pre2024 at JM", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(10, 100), cex = 2)
scd(ylim = c(-5, 15), main = "2024-pre2024 at Prairie Ridge", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Prairie Ridge Ecostation", lengthRange = c(10, 100), cex = 2)
scd(ylim = c(-5, 10), main = "2024-pre2024 at NCBG", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "NC Botanical Garden", lengthRange = c(10, 100), cex = 2)
scd(ylim = c(-5, 5), main = "2024-pre2024 at UNC", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "UNC Chapel Hill Campus", lengthRange = c(10, 100), cex = 2)


#now if you want to do that all again, but with the weeks compared to the same week +/- a week, you can use offset

scdoffset = function(ylim = c(-25, 25), ylab = "Change in Caterpillar Count", main, def = c(1,1,1,0,0,0), dd = T, yr1, yr2, site, lengthRange, offset, legend = F, legcex = 1.2, ...){
  scd(ylim = ylim, main = main, def = def, dd = F, yr1 = yr1, yr2 = yr2, site = site, lengthRange = lengthRange, offset = -7, legend = T, cex = 2)
  scd(ylim = ylim, main = main, def = def, dd = F, yr1 = yr1, yr2 = yr2, site = site, lengthRange = lengthRange, offset = 0, legend = F, cex = 2)
  scd(ylim = ylim, main = main, def = def, dd = F, yr1 = yr1, yr2 = yr2, site = site, lengthRange = lengthRange, offset = 7, legend = F, cex = 2)
}


#all caterpillars 2024-2025
par(mfcol = c(3, 2), mar = c(6, 5, 4, 1))

scdoffset(ylim = c(-15, 10), main = "2024-2025 at Eno", def = c(0,1,1,0,0,0), dd = F, yr1 = 2025, yr2 = 2024, site = "Eno River State Park", lengthRange = c(0, 100), cex = 2)
scdoffset(ylim = c(-15, 20), main = "2024-2025 at JM", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(0, 100), cex = 2)
scdoffset(ylim = c(-30, 10), main = "2024-2025 at Prairie Ridge", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Prairie Ridge Ecostation", lengthRange = c(0, 100), cex = 2)
scdoffset(ylim = c(-10, 10), main = "2024-2025 at NCBG", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "NC Botanical Garden", lengthRange = c(0, 100), cex = 2)
scdoffset(ylim = c(-10, 10), main = "2024-2025 at UNC", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "UNC Chapel Hill Campus", lengthRange = c(0, 100), cex = 2)

#small caterpillars 2025-2024
par(mfcol = c(3, 2), mar = c(6, 5, 4, 1))

scdoffset(ylim = c(-5, 20), main = "2024-2025 at Eno", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Eno River State Park", lengthRange = c(0, 10), legend = T, cex = 2)
scdoffset(ylim = c(-10, 20), main = "2024-2025 at JM", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(0, 10), cex = 2)
scdoffset(ylim = c(-5, 30), main = "2024-2025 at Prairie Ridge", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Prairie Ridge Ecostation", lengthRange = c(0, 10), cex = 2)
scdoffset(ylim = c(-5, 10), main = "2024-2025 at NCBG", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "NC Botanical Garden", lengthRange = c(0, 10), cex = 2)
scdoffset(ylim = c(-5, 10), main = "2024-2025 at UNC", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "UNC Chapel Hill Campus", lengthRange = c(0, 10), cex = 2)

#large caterpillars 2025-2024
par(mfcol = c(3, 2), mar = c(6, 5, 4, 1))

scdoffset(ylim = c(-5, 15), main = "2024-2025 at Eno", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Eno River State Park", lengthRange = c(10, 10), legend = T, cex = 2)
scdoffset(ylim = c(-10, 15), main = "2024-2025 at JM", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(10, 10), cex = 2)
scdoffset(ylim = c(-5, 20), main = "2024-2025 at Prairie Ridge", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "Prairie Ridge Ecostation", lengthRange = c(10, 10), cex = 2)
scdoffset(ylim = c(-5, 10), main = "2024-2025 at NCBG", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "NC Botanical Garden", lengthRange = c(10, 10), cex = 2)
scdoffset(ylim = c(-5, 5), main = "2024-2025 at UNC", def = c(0,1,1,0,0,0), yr1 = 2025, yr2 = 2024, site = "UNC Chapel Hill Campus", lengthRange = c(10, 10), cex = 2)


#all caterpillars 2024-pre2024
par(mfcol = c(3, 2), mar = c(6, 5, 4, 1))

scdoffset(ylim = c(-15, 20), main = "2024-pre2024 at Eno", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Eno River State Park", lengthRange = c(0, 100), legend = T, cex = 2)
scdoffset(ylim = c(-5, 25), main = "2024-pre2024 at JM", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(0, 100), cex = 2)
scdoffset(ylim = c(-5, 15), main = "2024-pre2024 at Prairie Ridge", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Prairie Ridge Ecostation", lengthRange = c(0, 100), cex = 2)
scdoffset(ylim = c(-5, 10), main = "2024-pre2024 at NCBG", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "NC Botanical Garden", lengthRange = c(0, 100), cex = 2)
scdoffset(ylim = c(-5, 5), main = "2024-pre2024 at UNC", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "UNC Chapel Hill Campus", lengthRange = c(0, 100), cex = 2)

#small caterpillars 2024-pre2024
par(mfcol = c(3, 2), mar = c(6, 5, 4, 1))

scdoffset(ylim = c(-15, 20), main = "2024-pre2024 at Eno", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Eno River State Park", lengthRange = c(0, 10), legend = T, cex = 2)
scdoffset(ylim = c(-5, 25), main = "2024-pre2024 at JM", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(0, 100), cex = 2)
scdoffset(ylim = c(-5, 15), main = "2024-pre2024 at Prairie Ridge", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Prairie Ridge Ecostation", lengthRange = c(0, 10), cex = 2)
scdoffset(ylim = c(-5, 10), main = "2024-pre2024 at NCBG", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "NC Botanical Garden", lengthRange = c(0, 10), cex = 2)
scdoffset(ylim = c(-5, 5), main = "2024-pre2024 at UNC", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "UNC Chapel Hill Campus", lengthRange = c(0, 10), cex = 2)

#large caterpillars 2024-pre2024
par(mfcol = c(3, 2), mar = c(6, 5, 4, 1))

scdoffset(ylim = c(-5, 15), main = "2024-pre2024 at Eno", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Eno River State Park", lengthRange = c(10, 100), legend = T, cex = 2)
scdoffset(ylim = c(-5, 15), main = "2024-pre2024 at JM", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(10, 100), cex = 2)
scdoffset(ylim = c(-5, 15), main = "2024-pre2024 at Prairie Ridge", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "Prairie Ridge Ecostation", lengthRange = c(10, 100), cex = 2)
scdoffset(ylim = c(-5, 10), main = "2024-pre2024 at NCBG", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "NC Botanical Garden", lengthRange = c(10, 100), cex = 2)
scdoffset(ylim = c(-5, 5), main = "2024-pre2024 at UNC", def = c(0,1,1,0,0,0), yr1 = "pre-2024", yr2 = 2024, site = "UNC Chapel Hill Campus", lengthRange = c(10, 100), cex = 2)


par(mfrow = c(1, 2), mar = c(6, 5, 4, 1))
sitescatratio(ylim = c(0, 2), main = "Defended/Undefended ratio in 2024", sites = c(1,1,1,1,1,0), plotVar = "fracratio", yr = 2024, lengthRange = c(0, 100), cex = 2)
addlegend(x = "topleft", colsh = "site", subset = c(1,1,1,1,1,0), inset = c(0, 0), cex = 0.9, y.intersp = 0.7)
sitescatratio(ylim = c(0, 2), main = "Defended/Undefended ratio in 2025", sites = c(1,1,1,1,1,0), plotVar = "fracratio", yr = 2025, lengthRange = c(0, 100), cex = 2)

par(mfrow = c(1, 2), mar = c(6, 5, 4, 1))
sitescatratio(ylim = c(0, 2), main = "Defended/Undefended ratio in 2024", sites = c(0,0,0,0,0,1), plotVar = "fracratio", yr = 2024, lengthRange = c(0, 100), cex = 2)
addlegend(x = "topleft", colsh = "site", subset = c(0,0,0,0,0,1), inset = c(0.1, 0.1), cex = 1.5, y.intersp = 0.7)
sitescatratio(ylim = c(0, 2), main = "Defended/Undefended ratio in 2025", sites = c(0,0,0,0,0,1), plotVar = "fracratio", yr = 2025, lengthRange = c(0, 100), cex = 2)


par(mfrow = c(1, 2), mar = c(6, 5, 4, 1))
sitescatratio(ylim = c(0, 2), main = "Defended/Undefended ratio in 2024", sites = c(1,1,1,1,1,0), plotVar = "densityratio", yr = 2024, lengthRange = c(0, 100), cex = 2)
addlegend(x = "topleft", colsh = "site", subset = c(1,1,1,1,1,0), inset = c(0, 0), cex = 0.9, y.intersp = 0.7)
sitescatratio(ylim = c(0, 2), main = "Defended/Undefended ratio in 2025", sites = c(1,1,1,1,1,0), plotVar = "densityratio", yr = 2025, lengthRange = c(0, 100), cex = 2)

par(mfrow = c(1, 2), mar = c(6, 5, 4, 1))
sitescatratio(ylim = c(0, 2), main = "Defended/Undefended ratio in 2024", sites = c(0,0,0,0,0,1), plotVar = "densityratio", yr = 2024, lengthRange = c(0, 100), cex = 2)
addlegend(x = "topleft", colsh = "site", subset = c(0,0,0,0,0,1), inset = c(0.1, 0.1), cex = 1.5, y.intersp = 0.7)
sitescatratio(ylim = c(0, 2), main = "Defended/Undefended ratio in 2025", sites = c(0,0,0,0,0,1), plotVar = "densityratio", yr = 2025, lengthRange = c(0, 100), cex = 2)


sitescatratiodif(ylim = c(-1, 2), main = "2025-2024 Ratio Difference", sites = c(1,1,1,1,1,1), yr1 = 2024, yr2 = 2025, lengthRange = c(15, 100))



sitedensity(ylim = c(0,5), main = "Large Caterpillar Density 2025", sites = c(1,1,1,1,1,1), yr = 2025, ordersToInclude = "caterpillar", defense = "all defended", lengthRange = c(15, 100))

