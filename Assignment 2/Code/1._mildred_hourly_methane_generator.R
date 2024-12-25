#CREATES CLEAN MILDRED LAKE HOURLY DATASET FOR ALL MODEL FITTING SCRIPTS FOR A SINGLE STATION


#Last modified by M.H.Kent on Dec 25th, 2023


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
library(ggplot2)
library(minpack.lm) #For nlsLM
library(caret)
library(AICcmodavg) #For AIC score 
library(nlstools)
library(broom)
library(car)


#Set ggplot theme 
theme_set(theme_classic())


###Defining usefull functions###

#defining function for RMSE for lm
RS_ME <- function(x) { #Input is the lm model name
RSS <-  c(crossprod(x$residuals))
MSE <- RSS / length(x$residuals)
RMSE <- sqrt(MSE)
print(RMSE)
}

#Defining RMSE for nls
RS_ME_nls <- function(x) { #Input is the lm model name
RSS <-  c(crossprod(as.numeric(residuals(x))))
MSE <- RSS / length(as.numeric(residuals(x)))
RMSE <- sqrt(MSE)
print(RMSE)
}

#Defining a function for AIC for lm model (No small sample size so no need to correct) 
lm_AIC <- function(x) { #imput is a linear model
logLike <- as.numeric(logLik(x))
dOf <- as.numeric(summary(x)$fstatistic[2]) + 2 
AICss <-  2*dOf - 2*logLike 
print(AICss)
}

#Defining a function for AIC for nls model (No small sample size so no need to correct)
nls_AIC <- function(x) { #imput is a linear model
logLike <- as.numeric(logLik(x))
dOf <- nrow(data.frame(tidy(x))) + 2 
AICss <-  2*dOf - 2*logLike 
print(AICss)
}

#Calculates geodisic distance between two points on a map
gcd_hf <- function(long1, lat1, long2, lat2) {
	R <- 6371 # Earth mean radius [km]
	delta.long <- (long2 - long1) #need to be converted to radians first 
	delta.lat <- (lat2 - lat1)
	a <- (sin(delta.lat/2)^2) + (cos(lat1) * cos(lat2) * (sin(delta.long/2)^2))
	c <- 2 * asin(min(1,sqrt(a)))
 	d = R * c
	return(d) # Distance in km
}

#Convert degrees to radians
deg2rad <- function(deg) { 
return(deg*pi/180)
}

#Convert radians to degrees 
rad2deg <- function(rad) {
return(rad*180/pi)
} 

#Defining function to calculate angle between mildred lake and the station station from the north 
ang_to_stat <- function(long1, lat1, long2, lat2) { #The seccond entry is the orgin
long_rad1   <- deg2rad(long1)
long_rad2   <- deg2rad(long2)
lat_rad1    <- deg2rad(lat1)
lat_rad2    <- deg2rad(lat2) 
thet <- atan((lat_rad1-lat_rad2)/(long_rad1-long_rad2)) #Output in degrees
return(rad2deg(thet) - 90) #-90 degree difference from east to north
} #Note that this gives the angle in the opposite dirrection 


#Defining the cordinates of mildred lake
mild_cords <- c(57.052502, -111.588333) 


###Uploading and Cleaning Data###

#Setting paths as variables  

hour_mild_path <- "/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/midred_weather_hourly"  #Weather data path

aq_mild_path <- "/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/Organized_Warehouse_Data/clean1066.csv" #Air quality station path for mildred lake 


#Uploading mildred hourly weather data 

hour_mild_files <- c(list.files(path=hour_mild_path, pattern="*.csv", full.names=TRUE)) #Get list of paths  
#hour_mild_files

mild_weth_out <- read.csv(hour_mild_files[1], check.names=FALSE) #Setting initial condition for upload loop
#head(mild_weth_out) 

for (i in 2:length(hour_mild_files)) { 

print(i) #Print itteration

mild_weth_in <- read.csv(hour_mild_files[i], check.names=FALSE) #Import dataset
#length(colnames(mild_weth_in))
mild_weth_out <- merge(mild_weth_out, mild_weth_in, all = TRUE) #Merge with already uploaded data

}

summary(mild_weth_out) #Seeing what we got 


#Resetting column names 

og_weth_colnames <- colnames(mild_weth_out) #Seeing the column names that exist
#og_weth_colnames #View 

new_weth_cnames <- c("Longitude", "Latitude", "Station_Name", "Climate_ID", "Date_Time", "Year", "Month", "Day", "Time_LST", "Temp_C",          
"Temp_Flag", "Dew_Point_Temp_C", "Dew_Point_Temp_Flag", "Rel_Hum_per",         
"Rel_Hum_Flag", "Wind_Dir_ten_s_deg",  
"Wind_Dir_Flag", "Wind_Spd_km/h", "Wind Spd Flag", "Visibility_km",     
"Visibility_Flag", "Stn_Press_kPa",     
"Stn_Press_Flag", "Hmdx", "Hmdx_Flag", "Wind_Chill",          
"Wind_Chill_Flag", "Weather", "Precip_Amount_mm",  
"Precip_Amount_Flag") #Defining column names that are a little more machine readable 
#new_weth_cnames #View

#length(og_weth_colnames)# making sure they are the same
#length(new_weth_cnames)

colnames(mild_weth_out) <- new_weth_cnames #Setting new column names
summary(mild_weth_out) #View dataset


#Cleaning Mildred Hourly Weather Data

cl_mild_weth <- data.frame(mild_weth_out)%>%select_if(~sum(!is.na(.)) > 0)%>% #Removing rows that are all NAs
                                            mutate(Date = make_date(Year, Month, Day)) #Defining our data column as an actual date           
cl_mild_weth$secconds <- c(rep(":00", length(cl_mild_weth$Time_LST))) #Creating a "seconds" collumn
cl_mild_weth$Time <- paste(cl_mild_weth$Time_LST, cl_mild_weth$secconds, sep = "")  #Combining the two time columns into one 
clean_mild_weth <- cl_mild_weth%>%mutate(Date_Time = ymd_hms(paste(Date, Time)))%>% #creating our final date column
					    dplyr::select(-Year, -Month, -Day, -Time_LST, -Date, -secconds, -Time)

head(clean_mild_weth)
summary(cl_mild_weth)


#Uploading and cleaning Mildred Air quality data  

mild_aq_data <- fread(aq_mild_path)%>%select_if(~sum(!is.na(.)) > 0) %>% #Upload air quality data and remove rows with only NA values 
                                  mutate(IntervalStart = mdy_hms(IntervalStart),
                                         IntervalEnd   = mdy_hms(IntervalEnd),
						     Date_Time = IntervalStart)#, #We are choosing to match up the start of every air-quality mesurment interval to the corrilating temprature mesurement
						    #Date_time = IntervalStart)   #Only need one of these
mild_aq_data


#Combining the two dataframes by date 

raw_mild_join <- left_join(mild_aq_data, clean_mild_weth, by= "Date_Time")

#Note that the weather station and the methane mesurement station are 1km from each other on a relativly flat surface 
#I will assume that certain parameters such as humidity, pressure, dewpoint can be assumed to be the same between the two station due to the distance being only about 1km apart
#I will not assume wind speed or dirrection is the same for now 

#Getting data with only methane mesurements
mildred_clean_meth <- raw_mild_join%>%drop_na("Methane_GC/FID-Calibrated with Methane_ppm_VVC74")
mildred_clean_meth 

#Temprature probeing and replacing
sum(is.na(mildred_clean_meth$"Outdoor Air Temperature_Thermal Resistor_deg c_VVC332")) #How many NAs are there?
which(is.na(mildred_clean_meth$"Outdoor Air Temperature_Thermal Resistor_deg c_VVC332"), arr.ind=TRUE) #What rows contain NAs?
mildred_clean_meth[c(649, 650, 651)] #The weather station mesures these values so we can use these
mildred_clean_meth[c(649, 650, 651) , "Outdoor Air Temperature_Thermal Resistor_deg c_VVC332"] <- mildred_clean_meth[c(649, 650, 651), Temp_C]
#Can now get rid of temp_C column

#Humidity 
sum(is.na(mildred_clean_meth$"Relative Humidity_Hygrometer_percent_VVC352"))
hum_row_list <- c(which(is.na(mildred_clean_meth$"Relative Humidity_Hygrometer_percent_VVC352")))
mildred_clean_meth[hum_row_list,] #Seems that the weather station picks up most of what is being put down 
mildred_clean_meth[hum_row_list, "Relative Humidity_Hygrometer_percent_VVC352"] <- mildred_clean_meth[hum_row_list, Rel_Hum_per] #Replacing the values 
#Can remove Rel_Hum_per and the remaing NA rows of "Relative Humidity_Hygrometer_percent_VVC352"

#Windspeed and Dirrection
sum(is.na(mildred_clean_meth$"Wind Speed_Cup or Prop Anemometer_km/hr_VVC155"))
#Will need to remove 128 rows 
sum(is.na(mildred_clean_meth$"Wind Direction_Wind Vane_deg_VVC161"))
#Also remove 128 rows 

#Precipitation 
sum(is.na(mildred_clean_meth$Precip_Amount_mm))
which(is.na(mildred_clean_meth$Precip_Amount_mm))
#seems like they stopped messuring precipitation at a certain point

summary(mildred_clean_meth) #View


#Creating final clean Dataframe

Mildred_final_n <- mildred_clean_meth%>%select_if(~sum(!is.na(.)) > 0)%>%
                                      dplyr::select(-c("Total Hydrocarbons_FID-Calibrated with Methane/Propane_ppm_VVC48", "Total Hydrocarbons_GC/FID-Calibrated with Methane/Propane_ppm_VVC52",  "Hydrogen Sulphide_UV Pulsed Fluorescence_ppb_VVC31", "Sulphur Dioxide_UV Pulsed Fluorescence_ppb_VVC29", "Hydrogen Sulphide_UV Pulsed Fluorescence_ppb_VVC31", "Rel_Hum_per", "Wind_Dir_ten_s_deg", "Wind_Spd_km.h", "Wind.Spd.Flag", "Hmdx", "Wind_Chill",  "Temp_C", "Dew_Point_Temp_C", "Longitude.y", "Latitude.y", "Station_Name", "Climate_ID", "Non-methane Hydrocarbons_GC/FID-Calibrated with Methane/Propane_ppm_VVC60"))%>%
                                      drop_na("Relative Humidity_Hygrometer_percent_VVC352")%>% #Drop remaining NAs in dataset variables
						  drop_na("Wind Speed_Cup or Prop Anemometer_km/hr_VVC155")%>% 
						  drop_na("Wind Direction_Wind Vane_deg_VVC161")%>% 
						  drop_na("Precip_Amount_mm")%>%
                                      mutate(DewPoint = "NA")%>%
						  mutate(Month = factor(month(Date_Time)))
for (i in 1:length(Mildred_final_n$DewPoint)) { #Calculating and appending dewpoint values using the simplified formulat from "https://inspectapedia.com/Energy/Dew_Point_Calculation.php#:~:text=How%20to%20Calculate%20the%20Dew%20Point%20more%20Precisely,Td%20%3D%20the%20Dew%20Point%20Temperature%20in%20Kelvin"
Mildred_final_n[i, "DewPoint"] <- Mildred_final_n$"Outdoor Air Temperature_Thermal Resistor_deg c_VVC332"[i] - ((100 - Mildred_final_n$"Relative Humidity_Hygrometer_percent_VVC352"[800])/.5)
}
colnames(Mildred_final_n) <- c("V1", "IntervalStart", "IntervalEnd", "ProgramCode", "StationName", "StationID", "StationType", "Latitude", "Longitude", "Outdoor_Air_Temp_C", "Relative_Humidity_Per", "Wind_Direction_deg", "Wind_Speed_Std_Dev",  "Wind_Direction_Std_Dev", "Methane_ppm", "Wind_Speed_kmhr", "Date_Time",  "Precip_Amount_mm", "Precip_Amount_Flag", "DewPoint", "Month")
Mildred_final <- Mildred_final_n%>%filter(Precip_Amount_mm < 20)#%>%
					     #filter(Wind_Speed_kmhr < 20)#Getting rid of fulty values 
summary(Mildred_final) #Viewing final dataset

#Writing a csv file for clean data 
#write.csv(df, "C:/Users/miles/Desktop/ILLME Research/Air Quality Monotoring of Polutents/Data_For_Learning/mildred_methane_data")


#Identifying corrilations in indipendend variables 
cor(Mildred_Ind_Num)
#Dewpoint and outdoor airtemp bc dewpoint is calculated from outdoor air temp
#Moderate negative between relitive nubmidity and outdoor air temp 
#Moderate between Relative Humidity and Dewpoint
#Moderate between wind speed standard diviation and relative humidity 
#Strong corrilation between windspeed and windspeed standard diviation
#Moderate negative between windspeed and wind dirrection standard diviation

mild_dist <- gcd_hf(deg2rad(mild_cords[2]), deg2rad(mild_cords[1]), deg2rad(unique(Mildred_final$Longitude)), deg2rad(unique(Mildred_final$Latitude))) 
mild_dist
Mildred_final$Dist_To_Stat_km <- mild_dist

#Creating variable for wind dirrection towards station from center of midldred 
tward_mild <- ang_to_stat(unique(Mildred_final$Longitude), unique(Mildred_final$Latitude), mild_cords[2], mild_cords[1])
tward_mild

Mildred_final$Ang_To_Stat_deg <- 1 #Seting as 1 for a numeric variable

for (i in 1:length(Mildred_final$Ang_To_Stat_deg)) {

mild_ang_app <-  (Mildred_final$"Wind_Direction_deg"[i]) 

if (mild_ang_app > 180) {
	
	mild_ang_to <- mild_ang_app - 360 #Centering north at 0 degrees and south at +/- 180
	
} else {
	
	mild_ang_to <- mild_ang_app
	
} #End of first if else statement

mild_ang_tt <- mild_ang_to - tward_mild #Subtracting because it is a negative value 

if (mild_ang_tt > 180) {
	
	mild_ang_tth <- mild_ang_tt - 360 #Recentering
	
} else {
	
	mild_ang_tth <- mild_ang_tt 
	
} #End of if else statement

Mildred_final$Ang_To_Stat_deg[i] <- mild_ang_tth #Appending the value 
print((i/length(Mildred_final$Ang_To_Stat_deg)*100)) #Takes a little while so doing this to keep an eye on it 

} #End of loop 

summary(Mildred_final) #View final dataframe
colnames(Mildred_final)
nrow(Mildred_final)

#Export CSV
#write.csv(Mildred_final, "/Users/milsbeary/Desktop/MODIFIED_MILDRED.csv")


#Taking a look at difference in emission by month
levels(Mildred_final$Month)
plot(Methane_ppm ~ Month, 
     data = Mildred_final, 
     xlab = "Month",
     ylab = "Methane (ppm)",
     ylim = c(1.5,3.0))

#Statistical test
kruskal.test(Methane_ppm ~ Month, data = Mildred_final)

#Printing each mean 

for (i in 1:12) { 
	
	my_month <- Mildred_final%>%filter(Month == (i))
	print(i)
	print(median(my_month$Methane_ppm))
	
	}


#Making vairable transformations for regression analysis
Mildred_final_ill <- Mildred_final%>%mutate(Outdoor_Air_Temp_C_Two = (Outdoor_Air_Temp_C)^2, #Transform variables
                                            Outdoor_Air_Temp_C_Thr = (Outdoor_Air_Temp_C)^3,
                                            Outdoor_Air_Temp_C_Fou = (Outdoor_Air_Temp_C)^4,
                                            Outdoor_Air_Temp_C_Fiv = (Outdoor_Air_Temp_C)^5,
                                            Outdoor_Air_Temp_C_Six = (Outdoor_Air_Temp_C)^6,
                                            Outdoor_Air_Temp_C_Sev = (Outdoor_Air_Temp_C)^7,
                                            Outdoor_Air_Temp_C_Eig = (Outdoor_Air_Temp_C)^8,
                                            Outdoor_Air_Temp_C_Nin = (Outdoor_Air_Temp_C)^9,
                                            Outdoor_Air_Temp_C_Ten = (Outdoor_Air_Temp_C)^10,
                                            #Outdoor_Air_Temp_C_nTwo = (Outdoor_Air_Temp_C)^-2, #Transform variables
                                         #Outdoor_Air_Temp_C_nThr = (Outdoor_Air_Temp_C)^-3,
                                            #Outdoor_Air_Temp_C_nFou = (Outdoor_Air_Temp_C)^-4,
                                            #Outdoor_Air_Temp_C_nFiv = (Outdoor_Air_Temp_C)^-5,
                                            #Outdoor_Air_Temp_C_nSix = (Outdoor_Air_Temp_C)^-6,
                                            #Outdoor_Air_Temp_C_nSev = (Outdoor_Air_Temp_C)^-7,
                                            #Outdoor_Air_Temp_C_nEig = (Outdoor_Air_Temp_C)^-8,
                                            #Outdoor_Air_Temp_C_nNin = (Outdoor_Air_Temp_C)^-9,
                                            #Outdoor_Air_Temp_C_nTen = (Outdoor_Air_Temp_C)^-10,
                                            Relative_Humidity_Per_Two = (Relative_Humidity_Per)^2,
                                            Relative_Humidity_Per_Thr = (Relative_Humidity_Per)^3,
                                            Relative_Humidity_Per_Fou = (Relative_Humidity_Per)^4,
                                            Relative_Humidity_Per_Fiv = (Relative_Humidity_Per)^5,
                                            Relative_Humidity_Per_Six = (Relative_Humidity_Per)^6,
                                            Relative_Humidity_Per_Sev = (Relative_Humidity_Per)^7,
                                            Relative_Humidity_Per_Eig = (Relative_Humidity_Per)^8,
                                            Relative_Humidity_Per_Nin = (Relative_Humidity_Per)^9,
                                            Relative_Humidity_Per_Ten = (Relative_Humidity_Per)^10,
                                            #Relative_Humidity_Per_nTwo = (Relative_Humidity_Per)^-2,
                                            #Relative_Humidity_Per_nThr = (Relative_Humidity_Per)^-3,
                                            #Relative_Humidity_Per_nFou = (Relative_Humidity_Per)^-4,
                                            #Relative_Humidity_Per_nFiv = (Relative_Humidity_Per)^-5,
                                            #Relative_Humidity_Per_nSix = (Relative_Humidity_Per)^-6,
                                            #Relative_Humidity_Per_nSev = (Relative_Humidity_Per)^-7,
                                            #Relative_Humidity_Per_nEig = (Relative_Humidity_Per)^-8,
                                            #Relative_Humidity_Per_nNin = (Relative_Humidity_Per)^-9,
                                            #Relative_Humidity_Per_nTen = (Relative_Humidity_Per)^-10,
                                            Wind_Direction_Std_Dev_Two = (Wind_Direction_Std_Dev)^2,
                                            Wind_Direction_Std_Dev_Thr = (Wind_Direction_Std_Dev)^3,
                                            Wind_Direction_Std_Dev_Fou = (Wind_Direction_Std_Dev)^4,
                                            Wind_Direction_Std_Dev_Fiv = (Wind_Direction_Std_Dev)^5,
                                            Wind_Direction_Std_Dev_Six = (Wind_Direction_Std_Dev)^6,
                                            Wind_Direction_Std_Dev_Sev = (Wind_Direction_Std_Dev)^7,
                                            Wind_Direction_Std_Dev_Eig = (Wind_Direction_Std_Dev)^8,
                                            Wind_Direction_Std_Dev_Nin = (Wind_Direction_Std_Dev)^9,
                                            Wind_Direction_Std_Dev_Ten = (Wind_Direction_Std_Dev)^10,
                                            #Wind_Direction_Std_Dev_nTwo = (Wind_Direction_Std_Dev)^-2,
                                            #Wind_Direction_Std_Dev_nThr = (Wind_Direction_Std_Dev)^-3,
                                            #Wind_Direction_Std_Dev_nFou = (Wind_Direction_Std_Dev)^-4,
                                            #Wind_Direction_Std_Dev_nFiv = (Wind_Direction_Std_Dev)^-5,
                                            #Wind_Direction_Std_Dev_nSix = (Wind_Direction_Std_Dev)^-6,
                                            #Wind_Direction_Std_Dev_nSev = (Wind_Direction_Std_Dev)^-7,
                                            #Wind_Direction_Std_Dev_nEig = (Wind_Direction_Std_Dev)^-8,
                                            #Wind_Direction_Std_Dev_nNin = (Wind_Direction_Std_Dev)^-9,
                                            #Wind_Direction_Std_Dev_nTen = (Wind_Direction_Std_Dev)^-10,
                                            Wind_Speed_kmhr_Two = (Wind_Speed_kmhr)^2,
                                            Wind_Speed_kmhr_Thr = (Wind_Speed_kmhr)^3,
                                            Wind_Speed_kmhr_Fou = (Wind_Speed_kmhr)^4,
                                            Wind_Speed_kmhr_Fiv = (Wind_Speed_kmhr)^5,
                                            Wind_Speed_kmhr_Six = (Wind_Speed_kmhr)^6,
                                            Wind_Speed_kmhr_Sev = (Wind_Speed_kmhr)^7,
                                            Wind_Speed_kmhr_Eig = (Wind_Speed_kmhr)^8,
                                            Wind_Speed_kmhr_Nin = (Wind_Speed_kmhr)^9,
                                            Wind_Speed_kmhr_Ten = (Wind_Speed_kmhr)^10,
                                            #Wind_Speed_kmhr_nTwo = (Wind_Speed_kmhr)^-2,
                                            #Wind_Speed_kmhr_nThr = (Wind_Speed_kmhr)^-3,
                                            #Wind_Speed_kmhr_nFou = (Wind_Speed_kmhr)^-4,
                                            #Wind_Speed_kmhr_nFiv = (Wind_Speed_kmhr)^-5,
                                            #Wind_Speed_kmhr_nSix = (Wind_Speed_kmhr)^-6,
                                            #Wind_Speed_kmhr_nSev = (Wind_Speed_kmhr)^-7,
                                            #Wind_Speed_kmhr_nEig = (Wind_Speed_kmhr)^-8,
                                            #Wind_Speed_kmhr_nNin = (Wind_Speed_kmhr)^-9,
                                            #Wind_Speed_kmhr_nTen = (Wind_Speed_kmhr)^-10,
                                            Wind_Speed_Std_Dev_Two = (Wind_Speed_Std_Dev)^2,
                                            Wind_Speed_Std_Dev_Thr = (Wind_Speed_Std_Dev)^3,
                                            Wind_Speed_Std_Dev_Fou = (Wind_Speed_Std_Dev)^4,
                                            Wind_Speed_Std_Dev_Fiv = (Wind_Speed_Std_Dev)^5,
                                            Wind_Speed_Std_Dev_Six = (Wind_Speed_Std_Dev)^6,
                                            Wind_Speed_Std_Dev_Sev = (Wind_Speed_Std_Dev)^7,
                                            Wind_Speed_Std_Dev_Eig = (Wind_Speed_Std_Dev)^8,
                                            Wind_Speed_Std_Dev_Nin = (Wind_Speed_Std_Dev)^9,
                                            Wind_Speed_Std_Dev_Ten = (Wind_Speed_Std_Dev)^10,
                                            #Wind_Speed_Std_Dev_nTwo = (Wind_Speed_Std_Dev)^-2,
                                            #Wind_Speed_Std_Dev_nThr = (Wind_Speed_Std_Dev)^-3,
                                            #Wind_Speed_Std_Dev_nFou = (Wind_Speed_Std_Dev)^-4,
                                            #Wind_Speed_Std_Dev_nFiv = (Wind_Speed_Std_Dev)^-5,
                                            #Wind_Speed_Std_Dev_nSix = (Wind_Speed_Std_Dev)^-6,
                                            #Wind_Speed_Std_Dev_nSev = (Wind_Speed_Std_Dev)^-7,
                                            #Wind_Speed_Std_Dev_nEig = (Wind_Speed_Std_Dev)^-8,
                                            #Wind_Speed_Std_Dev_nNin = (Wind_Speed_Std_Dev)^-9,
                                            #Wind_Speed_Std_Dev_Ten = (Wind_Speed_Std_Dev)^-10),
                                            Ang_To_Stat_deg_Two = (Ang_To_Stat_deg)^2,
                                            Ang_To_Stat_deg_Thr = (Ang_To_Stat_deg)^3,
                                            Ang_To_Stat_deg_Fou = (Ang_To_Stat_deg)^4,
                                            Ang_To_Stat_deg_Fiv = (Ang_To_Stat_deg)^5,
                                            Ang_To_Stat_deg_Six = (Ang_To_Stat_deg)^6,
                                            Ang_To_Stat_deg_Sev = (Ang_To_Stat_deg)^7,
                                            Ang_To_Stat_deg_Eig = (Ang_To_Stat_deg)^8,
                                            Ang_To_Stat_deg_Nin = (Ang_To_Stat_deg)^9,
                                            Ang_To_Stat_deg_Ten = (Ang_To_Stat_deg)^10,
                                            #Ang_To_Stat_deg_nTwo = (Ang_To_Stat_deg)^-2,
                                            #Ang_To_Stat_deg_nThr = (Ang_To_Stat_deg)^-3,
                                            #Ang_To_Stat_deg_nFou = (Ang_To_Stat_deg)^-4,
                                            #Ang_To_Stat_deg_nFiv = (Ang_To_Stat_deg)^-5,
                                            #Ang_To_Stat_deg_nSix = (Ang_To_Stat_deg)^-6,
                                            #Ang_To_Stat_deg_nSev = (Ang_To_Stat_deg)^-7,
                                            #Ang_To_Stat_deg_nEig = (Ang_To_Stat_deg)^-8,
                                            #Ang_To_Stat_deg_nNin = (Ang_To_Stat_deg)^-9,
                                            #Ang_To_Stat_deg_nTen = (Ang_To_Stat_deg)^-10,
                                            Dist_To_Stat_km_Two = (Dist_To_Stat_km)^2,
                                            Dist_To_Stat_km_Thr = (Dist_To_Stat_km)^3,
                                            Dist_To_Stat_km_Fou = (Dist_To_Stat_km)^4,
                                            Dist_To_Stat_km_Fiv = (Dist_To_Stat_km)^5,
                                            Dist_To_Stat_km_Six = (Dist_To_Stat_km)^6,
                                            Dist_To_Stat_km_Sev = (Dist_To_Stat_km)^7,
                                            Dist_To_Stat_km_Eig = (Dist_To_Stat_km)^8,
                                            Dist_To_Stat_km_Nin = (Dist_To_Stat_km)^9,
                                            Dist_To_Stat_km_Ten = (Dist_To_Stat_km)^10,
                                            #Dist_To_Stat_km_nTwo = (Dist_To_Stat_km)^-2,
                                            #Dist_To_Stat_km_nThr = (Dist_To_Stat_km)^-3,
                                            #Dist_To_Stat_km_nFou = (Dist_To_Stat_km)^-4,
                                            #Dist_To_Stat_km_nFiv = (Dist_To_Stat_km)^-5,
                                            #Dist_To_Stat_km_nSix = (Dist_To_Stat_km)^-6,
                                            #Dist_To_Stat_km_nSev = (Dist_To_Stat_km)^-7,
                                            #Dist_To_Stat_km_nEig = (Dist_To_Stat_km)^-8,
                                            #Dist_To_Stat_km_nNin = (Dist_To_Stat_km)^-9,
                                            #Dist_To_Stat_km_nTen = (Dist_To_Stat_km)^-10,
                                            Relative_Humidity_Per_Two = (Relative_Humidity_Per)^2,
                                            Relative_Humidity_Per_Thr = (Relative_Humidity_Per)^3,
                                            Relative_Humidity_Per_Fou = (Relative_Humidity_Per)^4,
                                            Relative_Humidity_Per_Fiv = (Relative_Humidity_Per)^5,
                                            Relative_Humidity_Per_Six = (Relative_Humidity_Per)^6,
                                            Relative_Humidity_Per_Sev = (Relative_Humidity_Per)^7,
                                            Relative_Humidity_Per_Eig = (Relative_Humidity_Per)^8,
                                            Relative_Humidity_vPer_Nin = (Relative_Humidity_Per)^9,
                                            Relative_Humidity_Per_Ten = (Relative_Humidity_Per)^10,
                                            #Relative_Humidity_Per_nTwo = (Relative_Humidity_Per)^-2,
                                            #Relative_Humidity_Per_nThr = (Relative_Humidity_Per)^-3,
                                            #Relative_Humidity_Per_nFou = (Relative_Humidity_Per)^-4,
                                            #Relative_Humidity_Per_nFiv = (Relative_Humidity_Per)^-5,
                                            #Relative_Humidity_Per_nSix = (Relative_Humidity_Per)^-6,
                                            #Relative_Humidity_Per_nSev = (Relative_Humidity_Per)^-7,
                                            #Relative_Humidity_Per_nEig = (Relative_Humidity_Per)^-8,
                                            #Relative_Humidity_Per_nNin = (Relative_Humidity_Per)^-9,
                                            #Relative_Humidity_Per_nTen = (Relative_Humidity_Per)^-10,
                                            Precip_Amount_mm_Two = (Precip_Amount_mm)^2,
                                            Precip_Amount_mm_Thr = (Precip_Amount_mm)^3,
                                            Precip_Amount_mm_Fou = (Precip_Amount_mm)^4,
                                            Precip_Amount_mm_Fiv = (Precip_Amount_mm)^5,
                                            Precip_Amount_mm_Six = (Precip_Amount_mm)^6,
                                            Precip_Amount_mm_Sev = (Precip_Amount_mm)^7,
                                            Precip_Amount_mm_Eig = (Precip_Amount_mm)^8,
                                            Precip_Amount_mm_Nin = (Precip_Amount_mm)^9,
                                            Precip_Amount_mm_Ten = (Precip_Amount_mm)^10#,
                                            #Precip_Amount_mm_nTwo = (Precip_Amount_mm)^-2,
                                            #Precip_Amount_mm_nThr = (Precip_Amount_mm)^-3,
                                            #Precip_Amount_mm_nFou = (Precip_Amount_mm)^-4,
                                            #Precip_Amount_mm_nFiv = (Precip_Amount_mm)^-5,
                                            #Precip_Amount_mm_nSix = (Precip_Amount_mm)^-6,
                                            #Precip_Amount_mm_nSev = (Precip_Amount_mm)^-7,
                                            #Precip_Amount_mm_nEig = (Precip_Amount_mm)^-8,
                                            #Precip_Amount_mm_nNin = (Precip_Amount_mm)^-9,
                                            #Precip_Amount_mm_nTen = (Precip_Amount_mm)^-10
                                            )
                                            
                                            %>%
                                            dplyr::select(-c(Precip_Amount_Flag)) #Removing this for now

                                                                                                                                   
summary(Mildred_final_ill) 
nrow(Mildred_final_ill) #24937


#Things to add to the models in the futre 
#Add night or day 
#Add sun activity 
#Add dulutant waste
#Will need to look at other tailing models 
#See what happens with the forest at nights
#Send weather data to L 




