library(dplyr)
library(gsheet)
library(lubridate)
library(stringr)

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
  group_by(year, DeployDate, julianweek, Name, julianday) %>%
  summarize(numBirdStrikes = sum(Bird),
            numClayCats = n(),
            pctBird = 100 * numBirdStrikes / numClayCats)


predation = function(x_min = 130, x_max = 190,
                     sites = c("eno", "jm", "pr", "ncbg", "unc", ...), yr = c(2024, 2025, ...),
                     c24 = '#CC79A7', c25 = '#0072B2',
                     mode = 1,
                     type = 'b',
                     color24,
                     color25,
                     shape24 = c(8, 17, 15, 16, 18),
                     shape25 = c(8, 17, 15, 16, 18),
                     shsize = 2,
                     axissize = 1,
                     labelsize = 1,
                     mainsize = 1,
                     main,
                     ...) {
  if(mode == 1){
    color24 <- c(c24, c24, c24, c24, c24)
    color25 <- c(c25, c25, c25, c25, c25)
  }
  if(mode == 2){
    c1 <- c("red", "#D55E00", "#0072B2", "yellow3", "#CC79A7")
    c2 <- c("red", "#D55E00", "#0072B2", "yellow3", "#CC79A7")
  }
  
  ifelse("eno" %in% sites, s1 <- "Eno River State Park", s1 <- 0)
  ifelse("jm" %in% sites, s2 <- "Triangle Land Conservancy - Johnston Mill Nature Preserve", s2 <- 0)
  ifelse("pr" %in% sites, s3 <- "Prairie Ridge Ecostation", s3 <- 0)
  ifelse("ncbg" %in% sites, s4 <- "NC Botanical Garden", s4 <- 0)
  ifelse("unc" %in% sites, s5 <- "UNC Chapel Hill Campus", s5 <- 0)
  site <- c(s1, s2, s3, s4, s5)
  
  plot(birdPred$julianday[birdPred$Name == site[1] & birdPred$year == yr[1]], 
       birdPred$pctBird[birdPred$Name == site[1] & birdPred$year == yr[1]], 
       xlab = "Date", ylab = "% Bird Strikes", ylim = c(0, 50), 
       xlim = c(x_min, x_max),
       xaxt = "n", 
       cex.axis = axissize, cex.lab = labelsize, type = type, 
       col = color24[1], cex = shsize, pch = shape24[1], lwd = 2,
       main = main, cex.main = mainsize)
  for(i in 1:5){
    points(birdPred$julianday[birdPred$Name == site[i] & birdPred$year == yr[1]], 
           birdPred$pctBird[birdPred$Name == site[i] & birdPred$year == yr[1]], 
           type = type, col = color24[i], pch = shape24[i], cex = shsize, lwd = 2)
    points(birdPred$julianday[birdPred$Name == site[i] & birdPred$year == yr[2]], 
           birdPred$pctBird[birdPred$Name == site[i] & birdPred$year == yr[2]], 
           type = type, col = color25[i], pch = shape25[i], cex = shsize, lwd = 2)}
  
  axis.Date(1, at = seq(x_min, x_max, by = 12), format = "%b %d", cex.axis = 1)
}
preset1 = function(sites, main){
  predation(sites = sites, yr = c(2024, 2025), mode = 1, type = 'b', shape24 = c(8, 8, 8, 8, 8), shape25 = c(17, 17, 17, 17, 17),
            axissize = 1, mainsize = 1, labelsize = 1, main = main)}
par(mfrow = c(2, 3), mar = c(4, 5, 4, 1))
preset1(sites = c("eno"), main = "Clay Caterpillar Predation at Eno")
#preset 1 legend
legend("topleft", legend = c(2024, 2025),
       col = c("#CC79A7", "#0072B2"),
       pch = c(8, 17),
       lwd = 2,
       cex = 1)
preset1(sites = c("jm"), main = "Johnston Mill")
preset1(sites = c("pr"), main = "Prairie Ridge")
preset1(sites = c("ncbg"), main = "NCBG")
preset1(sites = c("unc"), main = "UNC")

clayweek = Vectorize(function(yr1, yr2, site, cday){
  birdPredyr1 = birdPred%>%ungroup()%>%filter(year == yr1, Name == site)
  print(birdPredyr1)
  birdPredyr2 = birdPred%>%ungroup()%>%filter(year == yr2, Name == site)
  print(birdPredyr2)
  bpmatch = birdPredyr1%>%
    select(year, Name, julianday)%>%
    filter((birdPredyr2$julianday[cday] >= (julianday - 5) & birdPredyr2$julianday[cday] <= (julianday + 5)))
if(length(bpmatch$julianday) == 1){return(bpmatch$julianday[1])}
else{return(NA)}
},
vectorize.args = "cday")

claymatch = function(yr1, yr2, site){
  cw = clayweek(yr1, yr2, site, c(1:4))%>%unlist()
  yr2match = birdPred%>%
    filter(year == yr2, Name == site)%>%
    mutate(claybin = cw)%>%
    left_join(filter(birdPred, year == yr1, Name == site), by = c("Name","year"))
  return(yr2match)}

preddif = claymatch(2024, 2025, "NC Botanical Garden")
plot


predationdif = function(yr1, yr2, site){
  claymatch
  
  birdPred$pctBird - birdPred$pctBird
  plot()
}