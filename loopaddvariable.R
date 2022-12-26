library(dplyr)
library(tidyr)
library(fastDummies)

# load necceary datasets
tract_pop_den19  <- read.csv('popden19.csv') # tract level population density

## models from nhts data
walklm <- readRDS('walk_model_lm.rds')
transitlm <- readRDS('transit_model_lm.rds')
ridehaillm <- readRDS('ridehail_model_lm.rds')
carlm <- readRDS('car_model_lm.rds')
bikelm <- readRDS('bike_model_lm.rds')

# model time using distance
walkdata <- walklm$model
walk_model <- lm(travel_time~distance, weights = walkdata$`(weights)`, data=walkdata)
walk_model$coefficients

transitdata <- transitlm$model
transit_model <- lm(travel_time~distance, weights = transitdata$`(weights)`, data=transitlm$model)
transit_model$coefficients

ridehaildata <- ridehaillm$model
ridehail_model <- lm(travel_time~distance, weights = ridehaildata$`(weights)`, data=ridehaillm$model)
ridehail_model$coefficients

cardata <- carlm$model
car_model <- lm(travel_time~distance, weights = cardata$`(weights)`, data=carlm$model)
car_model$coefficients

bikedata <- bikelm$model
bike_model <- lm(travel_time~distance, weights = bikedata$`(weights)`, data=bikelm$model)
bike_model$coefficients

# here distance is the input from nhts trips
cartime_func <- function(data){
  return(exp(car_model$coefficients[1] + log(data$TRPMILES)*car_model$coefficients[2]))
}
#cartime <- exp(car_model$coefficients[1] + log(distance)*car_model$coefficients[2])

transittime_func <- function(data){
  return(exp(transit_model$coefficients[1] + log(data$TRPMILES)*transit_model$coefficients[2]))
}

rdtime_func <- function(data){
  return(exp(ridehail_model$coefficients[1] + log(data$TRPMILES)*ridehail_model$coefficients[2]))
}

walktime_func <- function(data){
  return(exp(walk_model$coefficients[1] + log(data$TRPMILES)*walk_model$coefficients[2]))
}

biketime_func <- function(data){
  return(exp(bike_model$coefficients[1] + log(data$TRPMILES)*bike_model$coefficients[2]))
}


# get travel time for each micromobility trip
micro_time_func <- function(data){
  distance <- as.numeric(data["TRPMILES"])
  basetime_micro <- distance/10*60 #10 mph
  
  dltime <- basetime_micro
  dbtime <- basetime_micro
  sctime <- basetime_micro
  sttime <- 0 # travel time of scooter in scooter + transit
  stransittime <- 0 # travel time of transit in the scooter + transit
  if(distance <= 5 & distance >2){
    # 2-5 miles
    stpct <- 0.3 # percent of scooter distance
    sttime = distance*stpct/10*60
    stransittime = distance*(1-stpct)/17*60
    
  }else if(distance >5){
    # >5 miles
    STTT <- 24 # min of scooter ride
    sttime = STTT
    stransittime = (distance-STTT/60*10)/17*60
    
  }
  list(dltime, dbtime, sctime,sttime,stransittime)
}

# get travel cost for micromobility
micro_price_func <- function(data){
  distance <- as.numeric(data["TRPMILES"])
  basetime_micro <- distance/10*60 #10 mph
  micro_price <- 0.25 # $/min
  micro_cost <- 1+micro_price*basetime_micro
  sccost <- micro_cost
  dlcost <- micro_cost
  dbcost <- micro_cost
  stcost <- 0
  sttotcost <- 0
  
  #2-5 miles
  if(distance <= 5 & distance >2){
    stpct <- 0.3 # percent of scooter distance
    sttime = distance*stpct/10*60
    stcost = 1+sttime*micro_price
    sttotcost = stcost + 2
  }else if(distance >5){
    # >5 miles
    STTT <- 24 # min of scooter ride
    stcost = 1+STTT*micro_price
    sttotcost = stcost + 2
  }
  list(sccost, dlcost, dbcost, stcost,sttotcost)
}

meanhhincome <- function(x){#omitted x==1
  if (x==1){
    5000
  }
  else if (x==2){
    12500
  }else if(x==3){
    17500
  }else if(x==4){
    22500
  }else if(x==5){
    30000
  }else if(x==6){
    42500
  }else if(x==7){
    62500
  }else if(x==8){
    72500
  }else if(x==9){
    125000
  }else if(x==10){
    175000
  }else if(x==11){
    225000
  }else{
    275000
  }
  
}

#trip_person = tract_trip
add_variables <- function(trip_person){
  # this function is under newdataformUtility.R
  # filter the data
  trip_person <- trip_person[trip_person$TRPMILES >0,]
  # are they really in meters?
  #trip_person$TRIPMILES <- trip_person$TRIPMILES*0.000621371
  # individual variables
  trip_person$mean_idincome <- apply(trip_person["idincome"],MARGIN = 1, 
                                     FUN=meanhhincome)
  trip_person$mean_idincomek <- trip_person$mean_idincome/1000
  trip_person$higheredu <- ifelse(trip_person$edu>=5,1,0)
  trip_person$employ <- ifelse(trip_person$work <=2,1,0)
  trip_person$bike <- ifelse(trip_person$bike=='yes',1,0)
  
  
  # get conventional mode travel time
  trip_person$cartime <- cartime_func(trip_person)
  trip_person$transittime <- transittime_func(trip_person)
  trip_person$rdtime <- rdtime_func(trip_person)
  trip_person$walktime <- walktime_func(trip_person)
  trip_person$biketime <- biketime_func(trip_person)
  
  # get micro modes travel time
  micro_time_ls <- apply(trip_person, 1, micro_time_func)
  micro_time_df <- do.call(rbind.data.frame, micro_time_ls)
  colnames(micro_time_df) <- c("dltime", "dbtime", "sctime","sttime","stransittime")
  trip_person <- cbind(trip_person,micro_time_df)
  
  # get micro modes travel cost
  micro_price_ls <- apply(trip_person, 1, micro_price_func)
  micro_price_df <- do.call(rbind.data.frame, micro_price_ls)
  colnames(micro_price_df) <- c("sccost", "dlcost", "dbcost", "stcost","sttotcost")
  trip_person <- cbind(trip_person,micro_price_df)
  
  ## adjust cost according to individual income
  trip_person$sccost_adj <- trip_person$sccost/trip_person$mean_idincomek
  trip_person$dlcost_adj <- trip_person$dlcost/trip_person$mean_idincomek
  trip_person$dbcost_adj <- trip_person$dbcost/trip_person$mean_idincomek
  trip_person$stcost_adj <- trip_person$stcost/trip_person$mean_idincomek
  trip_person$sttotcost_adj <- trip_person$sttotcost/trip_person$mean_idincomek
  
  
  # mode availability
  trip_person$car_av <- ifelse(trip_person$veh>0,1,0)
  trip_person$bike_av <- ifelse(trip_person$bike>0,1,0) 
  ## assuming all single ride of mimo is available for trip length < 5, not available > 5miles
  ## scooter+transit are available for trip > 2 miles
  trip_person$sc_av <- ifelse(trip_person$TRPMILES<5,1,0)
  trip_person$dl_av <- ifelse(trip_person$TRPMILES<5,1,0)
  trip_person$db_av <- ifelse(trip_person$TRPMILES<5,1,0)
  trip_person$st_av <- ifelse(trip_person$TRPMILES>2,1,0)
  trip_person$transit_av <- 1
  trip_person$rh_av <- 1
  trip_person$walk_av <-1
  
  # precipitation
  ##''PRCP''  make dummy 'PRCP_1', 'PRCP_2', 'PRCP_3'
  trip_person$PRCP_1 <- 0
  trip_person$PRCP_2 <- 0
  trip_person$PRCP_3 <- 1
  
  
  # bike lane
  ## BKLNless50
  trip_person$BKLN_1 <- 0
  trip_person$BKLN_2 <- 0
  trip_person$BKLN_3 <- 0
  trip_person$BKLN_4 <- 1
  
  
  # access walking time
  ##'SCAW', 'STAW','DLAW', 'DBAW'
  trip_person$SCAW <-3
  trip_person$STAW <-3
  trip_person$DLAW <-3
  trip_person$DBAW <-3
  
  # access waiting time depends on if SC is AV
  ## 'SCAV', 'STAV', AVtech
  trip_person$SCAV <-3
  trip_person$STAV <- 3
  trip_person$AVtech <-0
  
  # dropoff walking time
  ##'DBDW'
  trip_person$DBDW <- 3
  
  #pop density
  ## 'popdensityk'
  ## get weighted mean with zip level pop density, or should we just use tract level popdensity?
  ## change to just use tract leve population density
  trip_person$TRACT <- as.numeric(trip_person$TRACT)
  trip_person_density <- merge(trip_person, tract_pop_den19, by.x = 'TRACT', by.y = 'FIPS') 
  trip_person_density$popdensityk <- trip_person_density$POP10_SQMI/1000
  
  trip_person_density$BKLNless50 <- 1
  trip_person_density$STAV <- 3
  
  colnames(trip_person_density)[which(colnames(trip_person_density)=='TRIPPURP')] <- "trippurp"
  
  trip_person_density<-dummy_cols(trip_person_density,select_columns = c('trippurp'))
  colnames(trip_person_density)[which(colnames(trip_person_density)=='trippurp_HBSOCREC')] <- "trippurp_HBSOC"
  colnames(trip_person_density)[which(colnames(trip_person_density)=='sttotcost_adj')] <- "sttocost_adj"
  return(trip_person_density)
} 

tract_trip_col <-c("TRACT", "user", "age", "gender","race", "hispanic",
                   "usborn","edu","student","work", "zipcode" ,"hhsize",
                   "child", "hhincome","idincome", "disable","veh","bike",
                   "TRPMILES","TRIPPURP","dest_tract","tour_id_unq", "tour_id",
                   "MSA", "TDTRPNUM" )  

# list of MSA with vehicle availability data
#msa_ls = c(12420,14460,16980,12060,12580,16740,20500,40060,47900,19740,31080,33460,41860,41940)  
trip_count <- c()
# make sure the path to final_population is correct
path <- "H:/tz237/Tourgen/scoot-20221221T214221Z-001/scoot/data/final_population"
msa_files <- dir(path)
# for testing
# i <- 1

for (i in 1:length(msa_files)){
  
  #file_read <- paste0("final_population/generated_tracts_",msa_ls[i],".rds")
  file_read <- paste0(path,"/",msa_files[i])
  tract_sample <- readRDS(file_read) 
  print(nrow(tract_sample))
  tract_trip <- tract_sample[,c(6,7:23,28,27,1,2,3,5,25)]
  #tract_trip <- dataset[,c(6,7:23,28,27,1,2,3,5,25)]
  colnames(tract_trip) <- tract_trip_col
  # get origin of trips in a tour
  # sort the data
  tract_trip <- tract_trip[with(tract_trip, order(tour_id_unq,TDTRPNUM,TRACT)),]
  tract_trip$TRACT.1 <- tract_trip$TRACT
  tract_trip<- mutate(tract_trip, orig_tract = lag(dest_tract, order_by = tour_id_unq))
  tract_trip$orig_tract <- ifelse(tract_trip$TDTRPNUM==1,tract_trip$TRACT,tract_trip$orig_tract)
  
  # try iteratively adding variables and combine results afterwards
  tract_trip <- add_variables(tract_trip) # adding variable is slow 
  
  tract_trip <- tract_trip[,-c(84)]
  file_save<- paste0("appConstruct/addVar_tracts_",gsub("generated_tracts_", "", msa_files[i]))
  print(file_save)
  trip_count <- c(trip_count,nrow(tract_trip))
  print(nrow(tract_trip))
  saveRDS(tract_trip,file_save)
}



