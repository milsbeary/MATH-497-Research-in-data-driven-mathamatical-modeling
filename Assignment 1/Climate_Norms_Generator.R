#THIS SCRIPT GENERATES CLIMATE NORMS FROM THE DAILY DATA (CALCULATED MONTHLY AND BY % WARMEST AND COLDEST MONTHS)
#THIS SCRIPT IDENTIFIES WHERE HW AND CS ARE LOCATED IN TIME USING THIS DAILY DATA

#Last modified my M. H. Kent Nov 19th, 2023

#Delete previous information stored 
rm(list=ls(all=T))

#Makes sure there is no scientific notation on outputs
options(scipen=999)

#Import needed libraries
library(tidyverse)
library(lubridate)
library(plyr)
library(pracma) #For linspace 
library(data.table) #For fread and fwrite
library(parallel)
library(doParallel)
library(MASS)
library(dplyr)


###Setting up script for parallel computing### 

numCores <- detectCores() #Defining what we got in this bad boy
#numCores <- 1
print(numCores) #Seeing how many cores we got 
registerDoParallel(numCores) 


#Import dirrectory list #Will need to change in cluster
mydradc = "/Users/milsbeary/Desktop/Heatwaves/correctdailycities"
filesadc <- c(list.files(path=mydradc, pattern="*.csv", full.names=TRUE)) #extract a list of file dirrectories and specify the list as a vector 
#filesadc #Take a look 
 
#Getting file information and setting vectors
m_raw_amnt <- length(filesadc) #Getting the number of raw files
#m_raw_amnt <- 10000 #Manual input
#m_raw_amnt

#Uploading files

#For temprature norms

tmp_monthy_uploader <- function(i) {

#print(filesam[i]) #View what is being read
amh_fsize <- file.info(filesadc[i])$size #Calculate size of the file in that dirrectory
#print(day_fsize) 

if (amh_fsize == 256) { #Size of file with no data and error message 

#Dont upload/do nothing
#print("Pass")

} else { #uploads the file and does base filtering


data_inadc  <- data.frame(data.table::fread(filesadc[i], select = c("STATION", "DATE", "LATITUDE",  "LONGITUDE", "ELEVATION", "NAME", "TEMP", "TEMP_ATTRIBUTES", "MAX", "MAX_ATTRIBUTES", "MIN", "MIN_ATTRIBUTES")))%>%
		   mutate(STATION = as.character(STATION), #Helps to prevent copious zeros out in front of the entries
		   YEAR = year(DATE), #Creates a month, day and year column for easier filterint 
		   MONTH = month(DATE),
		   DAY = day(DATE),
		   TEMP = as.numeric(TEMP), TEMP_ATTRIBUTES = as.numeric(TEMP_ATTRIBUTES), #Sets dataframe variables into needed types 
                      MAX  = as.numeric(MAX),  MAX_ATTRIBUTES  = as.character(MAX_ATTRIBUTES),
                      MIN  = as.numeric(MIN),  MIN_ATTRIBUTES  = as.character(MIN_ATTRIBUTES), 
                      TEMP_ATTRIBUTES = ifelse(TEMP_ATTRIBUTES == " ", "NA", TEMP_ATTRIBUTES), 
                      MAX_ATTRIBUTES  = ifelse(MAX_ATTRIBUTES  == " ", "NA", MAX_ATTRIBUTES), 
                      MIN_ATTRIBUTES  = ifelse(MIN_ATTRIBUTES  == " ", "NA", MIN_ATTRIBUTES))%>% #Replaces blanks with NAs 
                      filter(MAX<9999, MIN<9999)# #Gets rid of rows with missing max or min values
                                           
} #End of if else statment

} #End of function 


#Computing our function
full_m_ds <- foreach (i=1:m_raw_amnt, .combine='rbind', .multicombine=TRUE, .packages=c('data.table', 'dplyr', 'tidyverse', 'lubridate')) %dopar% tmp_monthy_uploader(i) #Takes about 3 minutes to run on a mac with 12 cores

summary(full_m_ds) #Getting summary of our dataframe
nrow(full_m_ds) #Get number of rows 


###Calculating Monthy Norms###

#Creating our blank dataframes for monthy norms 
norm_columns <- c("STATION", "ELEVATION", "LAT", "LON", "TF_START", "TF_END", linspace(1,12,12))
adv_norms <- data.frame(matrix(ncol = length(norm_columns), nrow = 1)) #Creates a blank dataframe for our adverage climate norms
min_norms <- data.frame(matrix(ncol = length(norm_columns), nrow = 1)) #Creates a blank dataframe for our climate threshold lows 
max_norms <- data.frame(matrix(ncol = length(norm_columns), nrow = 1)) #Creates a dataframe for our climate threshold highs 
colnames(adv_norms) <- norm_columns  #Adding the column names to each dataframe
colnames(min_norms) <- norm_columns 
colnames(max_norms) <- norm_columns

#Defining our itterative vectors for our loops 
daily_stats <- as.numeric(unique(full_m_ds$STATION))
#Defining our vector of time breaks for norm calculation
#breaks <- c("1990-01-01", "1970-01-01", "1950-01-01", "1930-01-01", "1910-01-10")
months <- linspace(1,12,12) #Sets a vectorof months 
month_threshold <- c(31*15, ((29*7)+(28*23))/2, 31*15, 30*15, 31*15, 30*15, 31*15, 31*15, 30*15, 31*15, 30*15, 31*15) #Sets a vector for our maximum threshold (Febuary accounts for leap years)


#Calculating our monthly climate norms
for (i in 1:length(daily_stats)) { 
	
#Create blank dataframes to be appended to 
norm_columns_inp <- c("STATION", "ELEVATION", "LAT", "LON", "TF_START", "TF_END", linspace(1,12,12))
adv_norms_inp <-  data.frame(matrix(ncol = length(norm_columns_inp), nrow = 1)) #Creates a blank dataframe for our mothly adverages
min_norms_inp <- data.frame(matrix(ncol = length(norm_columns_inp), nrow = 1)) #Creates a blank dataframe for our climate threshold lows
max_norms_inp <- data.frame(matrix(ncol = length(norm_columns_inp), nrow = 1)) #Creates a dataframe for our climate threshold highs 
colnames(adv_norms_inp) <- norm_columns_inp 
colnames(min_norms_inp) <- norm_columns_inp #Adding the column names to each dataframe
colnames(max_norms_inp) <- norm_columns_inp

print(i) #Tells us our itteration

subset_m_ds <- full_m_ds%>%filter(STATION == daily_stats[i]) #Define our data subset

summary(subset_m_ds)

#subset_min <- min(subset_m_ds$DATE) #Defines max date in subset
#subset_max <- max(subset_m_ds$DATE) #Defines min date in subset
#stat_int_sub <- breaks[breaks > subset_min & breaks < subset_max] #Modifies our break vector to be contained within the min and max dates of our dataframe
#stat_int <- c(subset_max, stat_int_sub, subset_min) #Creates the vector that will contatin the bounds we will be calculating the climate norms for 

stat_int <- c("2019-12-31", "1990-01-01") #Defining our window manually

for (j in 1:(length(stat_int)-1)) {
	
adv_vec <- rep("NA", length(norm_columns_inp))
min_vec <- rep("NA", length(norm_columns_inp)) #Create blank vectors that the values will be appended to and then added to the dataframe
max_vec <- rep("NA", length(norm_columns_inp))

subset_f_nm <- subset_m_ds%>%filter(DATE <= stat_int[j] & DATE >= stat_int[j+1]) #Produces a data subset for us to calculate or norms for

if (nrow(subset_f_nm) < 1)  {

break #Need at least one row to get basic info from

} else {

adv_vec[1] <- unique(subset_f_nm$STATION)
min_vec[1] <- unique(subset_f_nm$STATION) #Adds station name to the vectors
max_vec[1] <- unique(subset_f_nm$STATION)

adv_vec[2] <- unique(subset_f_nm$ELEVATION)
min_vec[2] <- unique(subset_f_nm$ELEVATION) #Adds Elevation
max_vec[2] <- unique(subset_f_nm$ELEVATION)

adv_vec[3] <- unique(subset_f_nm$LAT)
min_vec[3] <- unique(subset_f_nm$LAT) #Adds latitude
max_vec[3] <- unique(subset_f_nm$LAT)

adv_vec[4] <- unique(subset_f_nm$LON)
min_vec[4] <- unique(subset_f_nm$LON) #Adds longitude
max_vec[4] <- unique(subset_f_nm$LON)

adv_vec[5] <- as.character(stat_int[j+1])
min_vec[5] <- as.character(stat_int[j+1])  #Adds start of the calculation interval 
max_vec[5] <- as.character(stat_int[j+1])

adv_vec[6] <- as.character(stat_int[j])
min_vec[6] <- as.character(stat_int[j])  #Adds end of the calculation interval 
max_vec[6] <- as.character(stat_int[j])

for (k in 1:length(months)) {
		
subset_mo <- subset_f_nm%>%filter(MONTH == months[k])#%>% #Breaks up by month
                           #filter(TEMP < 150, MAX < 150, MIN < 150) #Excludes NA and obviously false enteries (filterd out before so this is just here just in case)

adv_tmps <- subset_mo$TEMP #Extracts a vector of the adverage daily tempratures for that month

cur_mnth_thrsh <- month_threshold[k] #Extracts the current month threshold

ord_day_min <- sort(subset_mo$MIN) #Sorts our set of maxes from least to greatest 
ord_day_max <- sort(subset_mo$MAX) #Sorts our set of mins from least to greatest
len_d_min_t  <- ceiling(length(ord_day_min)*.1) #Extracts the bottom 10% element and rounds up to the nearest whole integer 
len_d_max_n <- ceiling(length(ord_day_max)*.9) #Extracts the the op ten percent and rounds up

min_tp_apnd <- ord_day_min[len_d_min_t]
max_tp_apnd <- ord_day_max[len_d_max_n]


if (length(adv_tmps) < cur_mnth_thrsh) {

adv_vec[6+k] <- "NA" #If there is no reccord, append NA
	
} else {

adv_vec[6+k] <- mean(adv_tmps) #If the reccord exist, put in the reccord

} #End of adverage

if (length(ord_day_min) < cur_mnth_thrsh) {

min_vec[6+k] <- "NA" #If there is no reccord, append NA
	
} else {

min_vec[6+k] <- min_tp_apnd #If the reccord exist, put in the reccord

} #End of min if else

if (length(ord_day_max) < cur_mnth_thrsh) {

max_vec[6+k] <- "NA"	
	
} else {

max_vec[6+k] <- max_tp_apnd	

} #End of max if else

	
} #End of k for loop

vec_adv_df <- transpose(data.frame(adv_vec))
vec_min_df <- transpose(data.frame(min_vec)) #Making vectors into dataframes so they can be added
vec_max_df <- transpose(data.frame(max_vec))
colnames(vec_adv_df) <- norm_columns_inp
colnames(vec_min_df) <- norm_columns_inp #Giving the dataframes matching columns 
colnames(vec_max_df) <- norm_columns_inp

adv_norms_inp <- rbind(adv_norms_inp, vec_adv_df)
min_norms_inp <- rbind(min_norms_inp, vec_min_df) #Appends min vector to min imput dataframe
max_norms_inp <- rbind(max_norms_inp, vec_max_df) #Appends max vector to max imput dataframe

} #End of if else statement

} #End of j for loop 

adv_norms <- rbind(adv_norms, adv_norms_inp)
min_norms <- rbind(min_norms, min_norms_inp)#%>%na.omit()) #Appends to full dataset and removes NA row
max_norms <- rbind(max_norms, max_norms_inp)#%>%na.omit()) 


} #End of i for loop 


###Creating final monthy norm dataframes### 

#Create function for F to C conversion 
FtoC <- function(F) {
		(F-32)*(5/9) #Calculate value
}

#Creating final adverage norm dataframe 

adv_norms_final_f <- adv_norms%>%na.omit()%>% #Getting rid of all only NA rows 
                               mutate_at(7:18, as.numeric) #Converting all temprature values to numerical variable
summary(adv_norms_final_f)

#Brunt forceing farenhight to celcius
for (i in 7:ncol(adv_norms_final_f)) {
for (j in 1:nrow(adv_norms_final_f)) {	
	
adv_f_tmp <- adv_norms_final_f[j,i]
#print(min_f_tmp)

if (is.na(adv_f_tmp)) { #If the temprature value is NA, do nothing 

} else {
	
adv_norms_final_f[j,i] <- round(FtoC(adv_f_tmp), 2) #Converts the value, rounds it to two decimal places, and appends the value back into the dataframe 
		
} #End of if else statement 
} #End of j for loop
} #End of i for loop

summary(adv_norms_final_f) #Seems to have worked
adv_norms_final <- adv_norms_final_f%>%na.omit() #Renames dataframe and removes NA rows 
nrow(adv_norms_final) #Missing 18 cities 

#Creating final minumumn norm dataframe

min_norms_final_f <- min_norms%>%na.omit%>% #Getting rid of all only NA rows 
                               mutate_at(7:18, as.numeric) #Converting all temprature values to numerical variable
summary(min_norms_final_f)

#Brunt forceing farenhight to celcius
for (i in 7:ncol(min_norms_final_f)) {
for (j in 1:nrow(min_norms_final_f)) {	
	
min_f_tmp <- min_norms_final_f[j,i]
#print(min_f_tmp)

if (is.na(min_f_tmp)) { #If the temprature value is NA, do nothing 

} else {
	
min_norms_final_f[j,i] <- round(FtoC(min_f_tmp), 2) #Converts the value, rounds it to two decimal places, and appends the value back into the dataframe 
		
} #End of if else statement 
} #End of j for loop
} #End of i for loop

summary(min_norms_final_f) #Seems to have worked
min_norms_final <- min_norms_final_f%>%na.omit() #Renames dataframe and removes NA rows 
nrow(min_norms_final) #Missing 18 cities 

#Creating maximum norm dataframe

max_norms_final_f <- max_norms%>%na.omit%>%
                               mutate_at(7:18, as.numeric)
summary(max_norms_final_f)

#Brunt forceing farenhight to celcius
for (i in 7:ncol(max_norms_final_f)) {
for (j in 1:nrow(max_norms_final_f)) {	
	
max_f_tmp <- max_norms_final_f[j,i]
#print(min_f_tmp)

if (is.na(max_f_tmp)) { 

} else {
	
max_norms_final_f[j,i] <- round(FtoC(max_f_tmp), 2) 
		
}	
}
}

summary(max_norms_final_f)
max_norms_final <- max_norms_final_f%>%na.omit() #Create final set removing rows with NAs 
nrow(max_norms_final) #Overall missing 18 cities write.csv(max_norms_final, "/Users/milsbeary/Desktop/Heatwaves/Climate_Norms/Maximum_Norms.csv") #Export minumum norms
#Exporting climate norm dataframes
#write.csv(adv_norms_final, "/Users/milsbeary/Desktop/Heatwaves/Climate_Norms/Adverage_Norms.csv") #Export adverage norms
#write.csv(min_norms_final, "/Users/milsbeary/Desktop/Heatwaves/Climate_Norms/Minumum_Norms.csv") #Export minumum norms
#write.csv(max_norms_final, "/Users/milsbeary/Desktop/Heatwaves/Climate_Norms/Maximum_Norms.csv") #Export minumum norms



###Identifying The Warm and Cold Season for Each City### 
#Identifying the 5 warmest and 5 coldest months from the adverage norms

#Creating a blank dataframe for the warm and cold seasons which will contain norms
cold_sz_df <- as.data.frame(matrix(nrow=nrow(adv_norms_final), ncol=(ncol(adv_norms_final)-7)))
colnames(cold_sz_df) <- colnames(adv_norms_final)[1:11]
summary(cold_sz_df)

warm_sz_df <- as.data.frame(matrix(nrow=nrow(adv_norms_final), ncol=(ncol(adv_norms_final)-7)))
colnames(warm_sz_df) <- colnames(adv_norms_final)[1:11]
summary(warm_sz_df)

for (i in 1:nrow(max_norms_final)) {
	
	adv_row <- adv_norms_final[i,] #Extract row 
	cold_sz_df[i, c(1:6)] <- adv_row[1, c(1:6)]
	warm_sz_df[i, c(1:6)] <- adv_row[1, c(1:6)] #Extract station information and append it to the new dataframe
	adv_temps <- transpose(adv_row[1, c(7:ncol(adv_row))]) #extract temps
	adv_temps_row <- c(adv_temps$V1)
	curr_month <- data.frame(adv_temps_row, months) #Creates a small dataframe of months
	cold_sz <- curr_month[order(curr_month$adv_temps_row, decreasing=FALSE),] #First five elements should be the cold season
	warm_sz <- curr_month[order(curr_month$adv_temps_row, decreasing=TRUE),] #First five elements should be the warm season
    
    cold_sz_df[i, c(7:11)] <- sort(cold_sz$months[1:5])
	warm_sz_df[i, c(7:11)] <- sort(warm_sz$months[1:5])
	
	}

print(cold_sz_df)
print(warm_sz_df)

#Identifying cities with random months
odd_cold <- c(97, 116, 158, 167, 175, 178, 236, 238, 241, 260, 276, 277, 278 ,280) #Generating vectors for them 
cold_sz_df[odd_cold,] #Viewing the cities 
print(warm_sz_df)
odd_warm <- c(97, 100, 116, 157, 161, 166, 169, 175, 177, 178, 182, 233, 237, 238, 243, 244, 260, 264, 267, 276, 277, 278, 282)
warm_sz_df[odd_warm,]


###Calculating Climate Norms for Warm and Cold Seasons###

#setequal(cold_sz_df$STATION, warm_sz_df$STATION) #Check to see if included stations in current norms are equal 

#Generating a blank dataframe which will contain the norms 
sz_norm_columns <- c("STATION", "ELEVATION", "LAT", "LON", "TF_START", "TF_END", "COLD_SZN", "WARM_SZN")
adv_norms <- data.frame(matrix(ncol = length(sz_norm_columns), nrow = nrow(cold_sz_df)))
colnames(adv_norms) <- sz_norm_columns 

for (i in 1:nrow(warm_sz_df)) {

cold_szn_in <- cold_sz_df%>%filter(STATION == cold_sz_df$STATION[i]) #Extract the station data
warm_szn_in <- warm_sz_df%>%filter(STATION == warm_sz_df$STATION[i])
cspon_data  <- full_m_ds%>%filter(STATION == warm_sz_df$STATION[i]) #Extracts the data from the full set

cspon_cold <- cspon_data%>%filter(MONTH %in% c(cold_sz_df[1,7], cold_sz_df[1,8], cold_sz_df[1,9], cold_sz_df[1,10], cold_sz_df[1,11])) #Identifies the five hottest and coldest months 
cspon_warm <- cspon_data%>%filter(MONTH %in% c(warm_sz_df[1,7], warm_sz_df[1,8], warm_sz_df[1,9], warm_sz_df[1,10], warm_sz_df[1,11]))

n_ord_day_min <- sort(cspon_cold$MIN) #Sorts our set of maxes from least to greatest 
n_ord_day_max <- sort(cspon_warm$MAX) #Sorts our set of mins from least to greatest
n_len_d_min_t  <- ceiling(length(n_ord_day_min)*.1) #Extracts the bottom 10% element and rounds up to the nearest whole integer (10% threshold)
n_len_d_max_n <- ceiling(length(n_ord_day_max)*.9) #Extracts the the top 10% and rounds up (90% thresehold)

min_tp_apnd <- round(FtoC(n_ord_day_min[n_len_d_min_t]), 2) #Calculates threshold and converts to celcius
max_tp_apnd <- round(FtoC(n_ord_day_max[n_len_d_max_n]), 2) 

#Append everything to the dataframe
adv_norms[i, "STATION"] <- warm_szn_in[1,"STATION"]
adv_norms[i, "ELEVATION"] <- warm_szn_in[1,"ELEVATION"]
adv_norms[i, "LAT"] <- warm_szn_in[1, "LAT"]
adv_norms[i, "LON"] <- warm_szn_in[1, "LON"]
adv_norms[i,"TF_START"] <- warm_szn_in[1,"TF_START"]
adv_norms[i, "TF_END"] <- warm_szn_in[1, "TF_END"]
adv_norms[i, "COLD_SZN"] <- min_tp_apnd 
adv_norms[i, "WARM_SZN"] <- max_tp_apnd
	
}

write.csv(adv_norms, "/Users/milsbeary/Desktop/Heatwaves/Climate_Norms/warm_and_cold_season_norms.csv")



###Identifying Heatwaves and Cold Snaps in the Data From Seasonal Norms

#Create a dataframe that will contained the identified heatwaves and cold snaps 
HW_CS_DS <- full_m_ds%>%mutate(MAX = round(FtoC(MAX), 2), #converts farenhight to celcius
							   MIN = round(FtoC(MIN), 2),
							   TEMP = round(FtoC(TEMP), 2),
							   IS_HW = linspace(0, 0, nrow(full_m_ds)), #Creates a variable for heatwave identifier
							   IS_CS = linspace(0, 0, nrow(full_m_ds)), #Creates a variable for coldsnap identifier
							   MONTH = month(DATE))%>% #Creates a column for month for easier filtering later down the line
					   filter(DATE <= "2019-12-31" & DATE >= "1990-01-01")#%>% #Extracts the date window that we have norms for
					   #filter(STATION == warm_sz_df$STATION) #Keeps only the stations we have norms for
summary(HW_CS_DS)	   


#Locating heatwaves and coldsnaps in the daily data 

HW_CS_DS_cmp <- as.data.frame(matrix(nrow=1,ncol=ncol(HW_CS_DS))) #Create a blank matrix to append everything two 
colnames(HW_CS_DS_cmp) <- colnames(HW_CS_DS)
HW_CS_DS_cmp				   

#Takes about 4 minutes to run  

for (i in 1:nrow(adv_norms)) { #
	
print(i)

heat_vector <- c() #Creates a blank vector for heatwaves 
cold_vector <- c() #Creates a blank vector for coldsnaps 

f_stat_norms <- adv_norms%>%filter(STATION == adv_norms$STATION[i])
HW_CS_DS_FOC <- HW_CS_DS%>%filter(STATION == adv_norms$STATION[i])
HW_CS_STAT_L <- nrow(HW_CS_DS_FOC)

summary(HW_CS_DS_FOC)

cold_norm <- f_stat_norms[1, 'COLD_SZN']  #Norms
warm_norm <- f_stat_norms[1, 'WARM_SZN']

#Error in data.frame(..., check.names = FALSE) : 
#  arguments imply differing number of rows: 10954, 10956


for (j in 1:HW_CS_STAT_L) {
	
day_min <- HW_CS_DS_FOC$MIN[j] #Get daily min 
day_max <- HW_CS_DS_FOC$MAX[j] #Get daily max
	
in_check = j #Calculates what itteration we are on
	
if (in_check == 1) {  #No difftime able to be calculated here so this is left as a seperate if else
	
	
	if (warm_norm <= day_max) {
	heat_vector <- append(heat_vector, 1) #Appends a "1" to the heatvector if there is a heatwave 
    } else {
    heat_vector <- c()
    }
    
    if (cold_norm >= day_min) {
	cold_vector <- append(cold_vector, 1)
	} else {
	cold_vector <- c()
	}
    
	
} else { 


day_diftime <- as.numeric(difftime(HW_CS_DS_FOC$DATE[j], HW_CS_DS_FOC$DATE[j-1], units="days")) #Calculates the difference between consecutive mesurements 

if (day_diftime != 1) {

heat_vector <- c() #Clear vectors because the time period is broken
cold_vector <- c() 	

} else { # Run normal sequence 


if (warm_norm <= day_max) {
	
heat_vector <- append(heat_vector, 1) #Appends a "1" to the heatvector if there is a heatwave 

if (length(heat_vector) >= 3) {
HW_CS_DS_FOC[j, 'IS_HW'] <- 1 #Appends to df 
HW_CS_DS_FOC[j-1, 'IS_HW'] <- 1
HW_CS_DS_FOC[j-2, 'IS_HW'] <- 1
} else {
#Do nothing 
} #End of inbeded if else

} else {
	
heat_vector <- c() #Clear heat vector 
	
} #End of heat vector if else 


if (cold_norm >= day_min) {
	
cold_vector <- append(cold_vector, 1) #Appends a "1" to the coldvector if there is a coldsnap 

if (length(cold_vector) >= 3) {
HW_CS_DS_FOC[j, 'IS_CS'] <- 1 #Appends to df 
HW_CS_DS_FOC[j-1, 'IS_CS'] <- 1
HW_CS_DS_FOC[j-2, 'IS_CS'] <- 1
} else {
#Do nothing 
} #End of imbeded if else

} else {
	
cold_vector <- c() #Clear heat vector 
	
} #End of cold vector if else 

} #End of difftime if else

} #End of big if else  
	
} #End of j loop 

HW_CS_DS_cmp <- rbind(HW_CS_DS_cmp, HW_CS_DS_FOC) #Combines into new dataframe

} #End of i loop
			   
HW_CS_DS_FINAL <- HW_CS_DS_cmp%>%na.omit() #Remove single NA row

summary(HW_CS_DS_FINAL) #View summary of final dataframe


###Export HW_CS_DS_FINAL as CSVs by Station###

Final_Stat_List <- unique(HW_CS_DS_FINAL$STATION) #Gets a list of stations

for (i in 1:length(Final_Stat_List)) {
	
HW_CS_DS_cmp%>%filter(STATION == Final_Stat_List[i])
write.csv(HW_CS_DS_cmp%>%filter(STATION == Final_Stat_List[i]), 
paste("/Users/milsbeary/Desktop/HeatWaves/daily_data_w_hw/", Final_Stat_List[i], ".csv", sep = ""))

}





















