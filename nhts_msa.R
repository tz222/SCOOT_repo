library(dplyr)

# load NHTS trip
nhts_trips <- readRDS("nhts_trips_from_clustering.rds")
nhts_trips <- nhts_trips[nhts_trips$mode!=-8,]
# recode car, transit, ridehail, walk, bike, other
# car: 3,4,5,6,
# transit: 11,12,13,14,15,16
# taxi: 17,18,
# walk: 1
# bike: 2
# other:7,8,9,10,19,20,97

mode_recode <- function(x){
  if (x==1){
    return(c('walk')) 
  }
  else if (x==2){
    return(c('bike'))
  }
  else if (x %in% c(3,4,5,6)){
    return(c('car'))
  }
  else if (x %in% c(17,18)){
    return(c('taxi'))
  }
  else if (x %in% c(11,12,13,14,15,16)){
    return(c('transit'))
  }
  else if (x %in% c(7,8,9,10,19,20,97)){
    return(c('other'))
  }}
nhts_trips$mode_val <- apply(nhts_trips["mode"],MARGIN = 1, 
                             FUN=mode_recode)
colnames(nhts_trips)
house_msa <- unique(dataset[,4:5])

msacode <- read.csv('cbsa2fipsxw.csv')
msacode <- msacode[msacode$metropolitanmicropolitanstatis=='Metropolitan Statistical Area',]
#msapop <- readxl::read_xlsx('cbsa-met-est2021-pop.xlsx')
msapop <- read.csv('cbsa-est2021-alldata.csv')


nhts_hh <- read.csv('csv/hhpub.csv')
nhts_trp <- read.csv('csv/trippub.csv')
nhts_trp <- nhts_trp[,c('HOUSEID','PERSONID','TDTRPNUM','NUMONTRP','WTTRDFIN')]
nhts_hh_cbsa <- nhts_hh[,c('HOUSEID','HH_CBSA','CDIVMSAR')]
length(unique(nhts_hh_cbsa$HH_CBSA))


nhts_msa <- merge(nhts_trips, nhts_hh_cbsa, by.x = 'houseid',by.y = 'HOUSEID')
nhts_mas <- merge(nhts_msa, nhts_trp,by.x = c('houseid','personid','tripnum'),
                  by.y = c('HOUSEID','PERSONID','TDTRPNUM'))
round(table(nhts_msa$CDIVMSAR)/nrow(nhts_msa),2)
nhts_mas$weight_daily <- nhts_mas$weight/365

nhts_share_msa<-nhts_mas %>%
  group_by(HH_CBSA, mode_val) %>%
  summarize(cnt = sum(weight_daily*NUMONTRP)) %>%
  mutate(freq = round(cnt / sum(cnt),3))

nhts_commuteshare_msa<-nhts_mas[nhts_mas$purp=='HBW',] %>%
  group_by(HH_CBSA, mode_val) %>%
  summarize(cnt = sum(weight_daily*NUMONTRP)) %>%
  mutate(freq = round(cnt / sum(cnt),3))

#### MSA of interest for validation and calibration
MSA_ls = c(12420,14460,16980,12060,12580,16740,20500,40060,47900,19740,31080,33460,41860,41940)  

nhts_modeshare_msa_cali <- nhts_mas[nhts_msa$HH_CBSA %in% MSA_ls & nhts_msa$mode_val!='other',] %>%
  group_by(mode_val) %>%
  summarise(cnt = sum(weight_daily*NUMONTRP)) %>%
  mutate(freq = round(cnt / sum(cnt),3))


nhts_total_trip_msa<-nhts_mas %>%
  group_by(HH_CBSA) %>%
  summarise(cnt = sum(weight_daily*NUMONTRP))

write.csv(nhts_total_trip_msa,'nhts_total_trip_msa.csv',row.names = FALSE)

write.csv(nhts_modeshare_msa_cali,'nhts_modeshare_msa_cali.csv',row.names = FALSE)


summary(nhts_share_msa[nhts_share_msa$mode_val=='car',]$freq)
summary(nhts_share_msa[nhts_share_msa$mode_val=='transit',]$freq)




