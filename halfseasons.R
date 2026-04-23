library(dplyr)
library(lubridate)
library(data.table)
library(gsheet)
library(maps)
library(sf)
library(stringr)

fullDataset = read.csv("data/fullDataset_2025-07-28.csv")

srvyData <- fullDataset %>%
  filter(Year %in% c(2021:2025), Name %in% c("Eno River State Park",
                                             "Triangle Land Conservancy - Johnston Mill Nature Preserve",
                                             "Prairie Ridge Ecostation",
                                             "NC Botanical Garden",
                                             "UNC Chapel Hill Campus"))%>%
  mutate(julianweek = 7*floor(julianday/7) + 4)


meanDensity = function(surveyData = srvyData,
                       year = 2024,
                       site = "all",
                       ordersToInclude = 'All',
                       defense = "all arthropods",
                       
                       
                       lengthRange = c(0, 100),         
                       jdRange = c(1,365),
                       midpoint = 166,
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
  surveyData = surveyData %>%
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
    mutate_cond(is.na(totalCount) & okWeek == 1, totalCount = 0, numSurveysGTzero = 0, totalBiomass = 0, nSurveys = 2*modalBranchesSurveyed) %>%
    mutate(meanDensity = totalCount/nSurveys,
           fracSurveys = 100*numSurveysGTzero/nSurveys,
           meanBiomass = totalBiomass/nSurveys,
           seasonhalf = floor(julianweek/midpoint) + 1)
  return(arthCount)
}

seasonhalf = function(surveyData = srvyData, yr, site,
                      defense = "all caterpillars", #or "undefended", "all defended", "hairy", "tented", or "rolled"
                      lengthRange = c(0, 100),
                      ...){
  halves = meanDensity(surveyData, year = yr, site = site, ordersToInclude = "caterpillar", defense = defense, lengthRange = lengthRange) %>%
    ungroup() %>%
    filter(okWeek == 1) %>%
    group_by(seasonhalf) %>%
    summarize(totalCount = sum(totalCount, na.rm = TRUE),
              numSurveysGTzero = sum(numSurveysGTzero, na.rm = TRUE),
              totalBiomass = sum(totalBiomass, na.rm = TRUE),
              Name = Name[1],
              nSurveyBranches = sum(nSurveyBranches, na.rm = TRUE),
              nSurveys = sum(nSurveys, na.rm = TRUE),
              seasonhalf = mean(seasonhalf),
              lower_ci = mean(100*sum(numSurveysGTzero, na.rm = TRUE)/sum(nSurveys, na.rm = TRUE)) - 1.96 * sqrt(mean(100*sum(numSurveysGTzero, na.rm = TRUE)/sum(nSurveys, na.rm = TRUE)) * (100 - mean(100*sum(numSurveysGTzero, na.rm = TRUE)/sum(nSurveys, na.rm = TRUE))) / sum(nSurveys, na.rm = TRUE)),
              upper_ci = mean(100*sum(numSurveysGTzero, na.rm = TRUE)/sum(nSurveys, na.rm = TRUE)) + 1.96 * sqrt(mean(100*sum(numSurveysGTzero, na.rm = TRUE)/sum(nSurveys, na.rm = TRUE)) * (100 - mean(100*sum(numSurveysGTzero, na.rm = TRUE)/sum(nSurveys, na.rm = TRUE))) / sum(nSurveys, na.rm = TRUE))) %>%
    mutate(meanDensity = totalCount/nSurveys,
           fracSurveys = 100*numSurveysGTzero/nSurveys,
           meanBiomass = totalBiomass/nSurveys)
  return(halves)
}

plothalves = function(surveyData = srvyData, yr, site,
                      defense = "all caterpillars",
                      lengthRange = c(0, 100),
                      off = 0,
                      plotVar = "fracSurveys",
                      shape = 1,
                      color = "black",
                      ...){
  sh = seasonhalf(surveyData, yr = yr, site = site, defense = defense, lengthRange = lengthRange) %>%
    mutate(seasonhalf = seasonhalf + off) %>%
    as.data.frame()
  points(sh$seasonhalf, sh[, plotVar],
         pch = shape, col = color, type = 'b', cex = 2)
  segments(c(1, 2) + off, sh$lower_ci, 
           c(1, 2) + off, sh$upper_ci, 
           col = color, lwd = 1) 
  title(main = site)
  axis(1, at = c(1, 2), labels = c("cicada period", "post-cicada period"), cex.axis = 1)
}

ph = function(site, lengthRange = c(0, 100), legend = F){
  plot(0, 0,
       xlim = c(0.5, 2.5), 
       ylim = c(-0.005, 25),
       cex.lab = 1,
       las = 1,
       lwd = 3, 
       cex = 3,
       type = "n",
       xaxt = "n",
       xlab = "",
       ylab = "% Surveys with Caterpillars")
  
  if(legend){
    legend("topright", legend = c("2024 undefended", "2024 defended", "2025 undefended", "2025 defended"),
    col = c("#D55E00", "#0072B2", "#D55E00", "#0072B2"),
    pch = c(16, 15, 1, 0), cex = 0.8, y.intersp = 0.5)
  }
  
  plothalves(yr = 2024, site = site, defense = "undefended", lengthRange = lengthRange, off = -0.1, shape = 16, color = "#D55E00")
  plothalves(yr = 2024, site = site, defense = "all defended", lengthRange = lengthRange, off = -0.033, shape = 15, color = "#0072B2")
  plothalves(yr = 2025, site = site, defense = "undefended", lengthRange = lengthRange, off = 0.033, shape = 1, color = "#D55E00")
  plothalves(yr = 2025, site = site, defense = "all defended", lengthRange = lengthRange, off = 0.1, shape = 0, color = "#0072B2")
}

par(mfrow = c(3, 2), mar = c(2, 5, 4, 1))

ph(site = "Eno River State Park", lengthRange = c(0, 100), legend = T)
ph(site = "Triangle Land Conservancy - Johnston Mill Nature Preserve", lengthRange = c(0, 100))
ph(site = "Prairie Ridge Ecostation", lengthRange = c(0, 100))
ph(site = "NC Botanical Garden", lengthRange = c(0, 100))
ph(site = "UNC Chapel Hill Campus", lengthRange = c(0, 100))


proptest = function(p1, p2, n1, n2){
  propTestZ = (p1 - p2)/((p1*(1-p1)/n1) + (p2*(1-p2)/n2)) ^ 0.5
  propTestP = 2*pnorm(q=abs(propTestZ), lower.tail=FALSE)
}
proptest()

