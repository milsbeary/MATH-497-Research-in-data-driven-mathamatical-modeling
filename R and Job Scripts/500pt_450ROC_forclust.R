#THIS SCRIPT GENERATES ROC CURVES AND STAT RESULTS FOR 500 DAY TIMESERIES AT BF = 450 
#SCRIPT RAN IN COMPUTE CANADA, BELUGA


#Last modified my M. H. Kent Dec 18th, 2023 

#Delete previous information stored 
rm(list=ls(all=T))

#Makes sure there is no scientific notation on outputs
options(scipen=999)

#Import needed libraries
library(tidyverse)
library(lubridate)
library(plyr)
library(tidyr)
library(pracma) #For linspace 
library(data.table) #For fread and fwrite
library(parallel)
library(doParallel)
library(MASS)
library(dplyr)
library(pROC)

#Setting up script for parallel computing 
numCores <- detectCores() #Defining what we got in this bad boy
print(numCores) #Seeing how many cores we got 
registerDoParallel(numCores)  #Registers so the cluster uses all cores


#Defining our functions for our metrics 
#Note: tryCatch within the fuction allows for code to keep running if an error is generated 

#Accuracy 
accuracy = function(tp, tn, fp, fn) {
	tryCatch({
	correct = tp + tn
	total = tp + tn + fp + fn
	return(correct/total)
	}, 
	error=function(e) { #Print NA if error 
		return("NA")
	}, 
	warning=function(w) {
	correct = tp + tn
	total = tp + tn + fp + fn
	return(correct/total)
	})
}

#Precision (how accuratly do we predict positive classes)
precision = function(tp, fp) {
	tryCatch({
	return(tp/(tp+fp))
	}, 
	error=function(e) {
		return("NA")
	}, 
	warning=function(w) {
	return(tp/(tp+fp))
	})	
}

#Recall/Sensitivity (ratio of predicted positive classes)
recall = function(tp, fn) {
	tryCatch({
	return(tp/(tp+fn))
	}, 
	error=function(e) {
		return("NA")
	}, 
	warning=function(w) {
	return(tp/(pt+fn))
	})
}

#Recall converse (ratio )
crecall = function(tn, fp) {
	tryCatch({
	return(fp/(fp+tn))
	}, 
	error=function(e) {
		return("NA")
	}, 
	warning=function(w) {
	return(fp / (fp + tn))
})
}

#F1 score (weighted adverage of recall and prescision, one is best and zero is worse)
f1_score = function(tp, tn, fp, fn) {
	tryCatch({
	p=precision(tp, fp)
	r=recall(tp, fn)
	return((2 * p * r) / (p + r))
	},
	error=function(e) {
		return("NA")
	}, 
	warning=function(w) {
	p=precision(tp, fp)
	r=recall(tp, fn)
	return((2 * p * r) / (p + r))
	})
}


#Specificity (true negative rate, how many negatives identified correctly) 
specificity = function(tn, fp) {
	tryCatch({
	return(tn / (tn + fp))
	},
	error=function(e) {
		return("NA")
	}, 
	warning=function(w) {
	return(tn / (tn + fp))
	})
}

#Prevalence (represents how often positive events occured)
prevalence = function(tp, tn, fp, fn) {
	tryCatch({
	t = tp + fn
	total = tp + tn + fp + fn
	return(t/total)
	},
	error=function(e) {
		return("NA")
	}, 
	warning=function(w) {
	t = tp + fn
	total = tp + tn + fp + fn
	return(t/total)
})
}

#Kappa (used to assess effectiveness of our model)
kappa = function(tp, tn, fp, fn) {
	tryCatch({
	return((2*(tp*tn - fn*fp))/(((tp+fp)*(fp+tn)) + ((tp+fn)*(fn+tn))))
	},
	error=function(e) {
		return("NA")
	}, 
	warning=function(w) {
	return((2*(tp*tn - fn*fp))/(((tp+fp)*(fp+tn)) + ((tp+fn)*(fn+tn))))	
		})
}


#Uploading norms
#wc_sz_nrm_path <- "/Users/milsbeary/Desktop/HeatWaves/Climate_Norms/warm_and_cold_season_norms.csv" #Desktop path
wc_sz_nrm_path <- "/home/mhkent/scratch/warm_and_cold_season_norms.csv"  #Cluster path
wc_sz_nrm_df <- data.frame(data.table::fread(wc_sz_nrm_path)) #upload
#summary(wc_sz_nrm_df)
wc_sz_nrm_stats <- c(as.character(wc_sz_nrm_df$STATION)) #Creates a list of stations we have norms for as a vector 
#wc_sz_nrm_stats


#Uploading classified time series
#class_ts_path <- "/Users/milsbeary/Desktop/HeatWaves/predict data/450"
class_ts_path <- "/home/mhkent/scratch/predict data/450" 
class_ts_files <- c(list.files(path=class_ts_path, pattern="*.csv", full.names=TRUE))
cl_ts_len <- length(class_ts_files)
#cl_ts_len <- 20 #Manual
cl_ts_len

class_ts_ids <- data.frame(data.table::fread(class_ts_files[1])) Set initial condition for upload loop
ogga_nms <- colnames(class_ts_ids)

for (i in 2:cl_ts_len) {

print((i/cl_ts_len)*100)
class_ts_in <- data.frame(data.table::fread(class_ts_files[i]))

if (length(colnames(class_ts_in)) != length(ogga_nms)) {
	
	print(unique(class_ts_in$STATION))
	print(unique(class_ts_in$REPORT_TYPE))
	
} else {
	
	class_ts_ids <- rbind(class_ts_ids, class_ts_in)
	
}
	
	
}

class_ts_out <- class_ts_ids

#Generate function for cluster uploading
#gen_ts_uploader <- function(i) {

#class_ts_in <- data.frame(data.table::fread(class_ts_files[i]))

#}

print("Data Uploaded")

#Upload in parrallel
#class_ts_out <- foreach(i=1:cl_ts_len, .combine='rbind', .multicombine=TRUE, .packages=c('data.table', 'dplyr', 'tidyverse', 'lubridate')) %dopar% gen_ts_uploader(i) 
#summary(class_ts_out)

#Modifies class_ts_out to be more usable 
colnames(class_ts_out) <- c("Count", "STATION", "REPORT_TYPE", "LATITUDE", "LONGITUDE", "START_DATETIME", "END_DATETIME", "PARAMETER", linspace(1,500,500), "fold", "hoph", "trans", "null" ) #Renaming columns to something a little bit more usable
classif_ts <- class_ts_out%>%filter(START_DATETIME >= "1990-01-01", END_DATETIME <= "2019-12-31") #Renameing and filtering out dates 
#summary(classif_ts) #Take a look

ts_stat_list <- c(as.character(sort(unique(classif_ts$STATION)))) #Get a list of the stations  we have timeseries for in increasing order as a vector
#ts_stat_list

common_stats <- c(as.numeric(intersect(wc_sz_nrm_stats, ts_stat_list))) #Getting a list of stations we have both classified timeseries and norms for
print("Station list")
print(common_stats)	


#Setting up for the most inefficent loop ever

class_thresh_list <- c(.5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1) #List of thresholds
class_thr_list <- c("thre_50", "thre_55", "thre_60", "thre_65", "thre_70", "thre_75", "thre_80", "thre_85", "thre_90", "thre_95", "thre_100")
bif_thresh <- 450


#for (m in 1:length(class_thresh_list)) {

#print(class_thr_list[m])

#Function to run everything in parallell
threshold_ooga_booga <- function(m) {
	
#for (m in 1:length(class_thresh_list)) {
	
#for (m in 1:1) {
	
#Creates dataframe for total calculations
det_cnames <- c("STATION", "REPORT_TYPE", "LATITUDE", "LONGITUDE", "START_DATETIME","END_DATETIME", "BIF_DATETIME", "TRUE_POS", "TRUE_NEG", "FALSE_POS", "FALSE_NEG", "HEATWAVE", "COLDSNAP", "IDENTIFIED_BIFURCATION", "PROB_FOLD", "PROB_HOPH", "PROB_TRANS", "PROB_NULL", "N_ROW") #Define the column names we need
det_forh_tot <- data.frame(matrix(ncol = length(det_cnames), nrow = 1)) #Creates blank dataframe 
colnames(det_forh_tot) <- det_cnames 
	
	
#Getting a list of threasholds which we will select a value m
#class_thresh_list <- c(.5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1) #List of thresholds
#class_thr_list <- c("thre_50", "thre_55", "thre_60", "thre_65", "thre_70", "thre_80", "thre_85", "thre_90", "thre_95", "thre_100")
#bif_thresh <- 450

class_threh <- class_thresh_list[m]
print(class_thr_list[m])
#print(class_threh)

#By station

#function <- inner_func(i) {

for (i in 1:length(common_stats)) {
	
#for (i in 1:5) {
	
print((i/length(common_stats))*100)

#Uploading needed data 	
stat_wc_norms <- wc_sz_nrm_df%>%filter(STATION == common_stats[i]) #Uploading norms for station i
inp_cl_ts <- classif_ts%>%filter(STATION == common_stats[i])%>% #Upload station data
						  distinct(START_DATETIME, .keep_all = TRUE) #Remove extra mesurements from different #report types
#summary(inp_cl_ts)
#print(nrow(inp_cl_ts))

#Extracting warm and cold season norms (will be used later)
warm_norm <- stat_wc_norms$WARM_SZN
cold_norm <- stat_wc_norms$COLD_SZN


#Generates detection dataframe for ROC curves
det_cnames <- c("STATION", "REPORT_TYPE", "LATITUDE", "LONGITUDE", "START_DATETIME","END_DATETIME", "BIF_DATETIME", "TRUE_POS", "TRUE_NEG", "FALSE_POS", "FALSE_NEG", "HEATWAVE", "COLDSNAP", "IDENTIFIED_BIFURCATION", "PROB_FOLD", "PROB_HOPH", "PROB_TRANS", "PROB_NULL", "N_ROW") #Define the column names we need
det_forh <- data.frame(matrix(ncol = length(det_cnames), nrow = nrow(inp_cl_ts))) #Creates blank dataframe 
colnames(det_forh) <- det_cnames  #Attaching colnames to dataframe 

#By row 
for (j in 1:nrow(inp_cl_ts)) {

row_in_quest <- inp_cl_ts[j,] #Extracting row
#print(row_in_quest)

#Appending what we can from this row to det_forh
det_forh[j, "STATION"]        <- as.character(row_in_quest[1,"STATION"]) #Specifies as a character since it inputs weird if i dont
det_forh[j, "REPORT_TYPE"]    <- row_in_quest[1, "REPORT_TYPE"]
det_forh[j, "LATITUDE"]       <- row_in_quest[1, "LATITUDE"]
det_forh[j, "LONGITUDE"]      <- row_in_quest[1, "LONGITUDE"]
det_forh[j, "START_DATETIME"] <- as.character(row_in_quest[1, "START_DATETIME"])
det_forh[j, "END_DATETIME"]   <- as.character(row_in_quest[1, "END_DATETIME"])
det_forh[j, "PROB_FOLD"]      <- as.character(row_in_quest[1, "fold"])
det_forh[j, "PROB_HOPH"]      <- as.character(row_in_quest[1, "hoph"])
det_forh[j, "PROB_TRANS"]     <- as.character(row_in_quest[1, "trans"])
det_forh[j, "PROB_NULL"]      <- as.character(row_in_quest[1, "null"])
det_forh[j, "N_ROW"]          <- as.character(nrow(inp_cl_ts))


#Generating temprature and date dataframe for analysis of detection 
dates_in_quest <- seq(row_in_quest[1, "START_DATETIME"], row_in_quest[1, "END_DATETIME"], by = "3 hour") #Getting all of the dates we can match everything up
temps_in_quest <- as.numeric(row_in_quest%>%dplyr::select(-c("Count", "STATION", "REPORT_TYPE", "LATITUDE", "LONGITUDE",      "START_DATETIME", "END_DATETIME", "PARAMETER", "fold", "hoph", "trans", "null"))) #as.numeric tempratures into vector
count_vec <- linspace(1, length(dates_in_quest), length(dates_in_quest))
temp_date_df_base <- data.frame(temps_in_quest, c(dates_in_quest), c(count_vec)) #Building dataframe
colnames(temp_date_df_base) <- c("TEMP", "DATE_TIME", "COUNT") #Setting each column name to this as a date

temp_date_df <- suppressWarnings({temp_date_df_base%>%separate(DATE_TIME, c("DATE", "TIME"), sep = ' ', remove = FALSE)}) #Warning supressed because it caused by the "00:00:00" elements
temp_date_df <- replace(temp_date_df, is.na(temp_date_df), as.character("00:00:00")) #Doing this esperatly because r is being a cheap whore and eveything i try doesnt work
#temp_date_df

date_thresh <- temp_date_df[bif_thresh, "DATE_TIME"] #Extracts date of estimated bifurcation
det_forh[j, "BIF_DATETIME"] <- as.character(date_thresh)  #Appending to our dataframe

time_of_extrema <- temp_date_df%>%filter(DATE_TIME > date_thresh) #Filters for our time window 

if (bif_thresh == 400) { 

time_of_extrema <- time_of_extrema%>%filter(COUNT < 451) #Only includes the equivilent of 6.25 days after
	
}

dates_of_extrema <- unique(time_of_extrema$DATE) #Extracts the dates of extrema for HW and CS analysis

#Creates blank vectors to identify when we are over threshold 
is_hw_vec <- c()
is_cs_vec <- c()

#Creates a vector to identify if a bifurcation happens 
is_hw <- c()
is_cs <- c()


#By day 

#for HWs
for (k in 1:(length(dates_of_extrema))) {
#print(k)

hw_datetemps <- time_of_extrema%>%filter(DATE == dates_of_extrema[k]) #Extracts a full 24 day for hw analysis 

#Calculate max and min temps and extracting needed norms
max_tmp <- max(hw_datetemps$TEMP)

#Identifying if there is a heatwave or cold snap in these 7 days
if (max_tmp >= warm_norm) {
	
	is_hw_vec <- append(is_hw_vec, 1) #Appends ones to both
		
} else if (max_tmp < warm_norm) { 
	
	is_hw_vec <- c()
		
} else {
	
	print("Missing mesurement")
	
} #End of if else 1 


if (length(is_hw_vec) >= 3) {
		
	is_hw <- 1
	det_forh[j, "HEATWAVE"] <- 1
	
	break 

} else if (length(is_hw_vec) < 3 & k == 7) {
	is_hw <- 0
	det_forh[j, "HEATWAVE"] <- 0
	
	break 

} else {
	
	#Do nothing
	
} #End of if else 2 

} #End of k loop 1


#For coldsnaps
for (q in 1:(length(dates_of_extrema))) {
#print(q)

if (q==1) { 
	
cs_datetemps <- time_of_extrema%>%filter(DATE == dates_of_extrema[q] & TIME <= "12:00:00") 

#print(cs_datetemps)
	
} else if (q == length(dates_of_extrema)) {

cs_datetemps <- time_of_extrema%>%filter(DATE >= dates_of_extrema[q] & TIME > "12:00:00") 

#print(cs_datetemps)	
	
} else {

cs_datetemps1 <- time_of_extrema%>%filter(DATE == dates_of_extrema[q-1] & TIME > "12:00:00") #Filtering what we need
cs_datetemps2 <- time_of_extrema%>%filter(DATE == dates_of_extrema[q] & TIME <= "12:00:00")
cs_datetemps <- rbind(cs_datetemps1, cs_datetemps2)	#Combining both together to get our window

#print(cs_datetemps)
	
} #End of if else 3 
  

#Calculate min temp with trycatch to account for empty row
min_temp_calculator <- function(df) {tryCatch({
	return(min(df$TEMP))
	}, 
	error=function(e) {
	return(9999) #Use value that is so low that it registers a not a cold snap for very cold tempratures
	}, 
	warning=function(w) {
	return(9999) 
	})
}
min_tmp <- min_temp_calculator(cs_datetemps)

#Identifying if there is a heatwave or cold snap in these 8 days
if (min_tmp <= cold_norm) {
	
	is_cs_vec <- append(is_cs_vec, 1)
	
} else if (min_tmp > cold_norm) {
	
	is_cs_vec <- c()
		
} else {
	
	print("Missing mesurement")
	
} #End of if else 3 


if (length(is_cs_vec) >= 3) { 
	
	is_cs <- 1
	det_forh[j, "COLDSNAP"] <- 1
	
	break 

} else if (length(is_cs_vec) < 3 & q == 7) {
	
	is_cs <- 0
	det_forh[j, "COLDSNAP"] <- 0
	
	break 

} else {
	
	#Do nothing
	
} #End of if else 4 

} #End of q loop 2


#Taking a look to see if this corrisponds with any bifurcations

#Extracting our bifurcations 
bif_probs <- row_in_quest[,c("fold", "hoph", "trans", "null")]


#bifs_idd <- c() #Setting a vector for our identified bifurcation

#Seeing what bifurcation the classifier identifies (I am assumeing here it does not identify more then one at the same time)
if (bif_probs[1,"fold"] >= class_threh & bif_probs[1,"null"] < class_threh) { 

bifs_idd <- "fold"

} else if (bif_probs[1,"hoph"] >= class_threh & bif_probs[1,"null"] < class_threh) { 

bifs_idd <- "hoph"

} else if (bif_probs[1,"trans"] >= class_threh & bif_probs[1,"null"] < class_threh) { 

bifs_idd <- "trans"

} else {

	bifs_idd <- "null"

}

det_forh[j, "IDENTIFIED_BIFURCATION"] <- bifs_idd #Appending our detected bifurcation into each table 


#Identifying where our algorithm is correct
#is_hw
#is_cs

if (bifs_idd != "null" & is_hw == 1) { #True positive
	
	det_forh[j, "TRUE_POS"] <- 1
	det_forh[j, "TRUE_NEG"] <- 0
	det_forh[j, "FALSE_POS"] <- 0
	det_forh[j, "FALSE_NEG"] <- 0
	
	
} else if (bifs_idd != "null" & is_cs == 1) { #True positive 
	
	det_forh[j, "TRUE_POS"] <- 1
	det_forh[j, "TRUE_NEG"] <- 0
	det_forh[j, "FALSE_POS"] <- 0
	det_forh[j, "FALSE_NEG"] <- 0
	

} else if (bifs_idd == "null" & is_hw == 0) { #True negative
	
	det_forh[j, "TRUE_POS"] <- 0
	det_forh[j, "TRUE_NEG"] <- 1
	det_forh[j, "FALSE_POS"] <- 0
	det_forh[j, "FALSE_NEG"] <- 0

	
} else if (bifs_idd == "null" & is_cs == 0) { #True negative
	
	det_forh[j, "TRUE_POS"] <- 0
	det_forh[j, "TRUE_NEG"] <- 1
	det_forh[j, "FALSE_POS"] <- 0
	det_forh[j, "FALSE_NEG"] <- 0
	

} else if (bifs_idd != "null" & is_hw == 0) { #False positive 

	det_forh[j, "TRUE_POS"] <- 0
	det_forh[j, "TRUE_NEG"] <- 0
	det_forh[j, "FALSE_POS"] <- 1
	det_forh[j, "FALSE_NEG"] <- 0
	

} else if (bifs_idd != "null" & is_cs == 0) { #False positive 

	det_forh[j, "TRUE_POS"] <- 0
	det_forh[j, "TRUE_NEG"] <- 0
	det_forh[j, "FALSE_POS"] <- 1
	det_forh[j, "FALSE_NEG"] <- 0
	

} else if (bifs_idd == "null" & is_hw == 1) { #False negative
	
	det_forh[j, "TRUE_POS"] <- 0
	det_forh[j, "TRUE_NEG"] <- 0
	det_forh[j, "FALSE_POS"] <- 0
	det_forh[j, "FALSE_NEG"] <- 1
	

} else if (bifs_idd == "null" & is_cs == 1) { #False negative
	
	det_forh[j, "TRUE_POS"] <- 0
	det_forh[j, "TRUE_NEG"] <- 0
	det_forh[j, "FALSE_POS"] <- 0
	det_forh[j, "FALSE_NEG"] <- 1

	
} else {
	
	print("Something went wrong")
	
}

} #End of j loop 


#Generating a CSV for deth_forh
#write.csv(det_forh, paste("/Users/milsbeary/Desktop/Bif_450/", class_thr_list[m], "/station_info/", common_stats[i], "_all_info.csv", sep = ""))
write.csv(det_forh, paste("/home/mhkent/scratch/Bif_450/", class_thr_list[m], "/station_info/", common_stats[i], "_all_info.csv", sep = ""))


#Running calculations to get statistics

#Calculating the amount of each for our station
tru_pos = sum(det_forh$"TRUE_POS")
tru_neg = sum(det_forh$"TRUE_NEG")
fal_pos = sum(det_forh$"FALSE_POS")
fal_neg = sum(det_forh$"FALSE_NEG")

#Creating a table for statistics
stats <- c("Accuracy", "Precision", "Recall", "C_Recall", "F1", "Specificity", "Prevalence", "Kappa")
vals <- c(accuracy(tru_pos, tru_neg, fal_pos, fal_neg), precision(tru_pos, fal_pos), recall(tru_pos, fal_neg), crecall(tru_neg, fal_pos), f1_score(tru_pos, tru_neg, fal_pos, fal_neg), specificity(tru_neg, fal_pos), prevalence(tru_pos, tru_neg, fal_pos, fal_neg), kappa(tru_pos, tru_neg, fal_pos, fal_neg))
stat_tab <- data.frame(stats, vals)

#Generating a CSV for our stats
#write.csv(stat_tab, paste("/Users/milsbeary/Desktop/Bif_450/", class_thr_list[m], "/station_stats/", common_stats[i], "_station_stats.csv", sep = ""))
write.csv(stat_tab, paste("/home/mhkent/scratch/Bif_450/", class_thr_list[m], "/station_stats/", common_stats[i], "_station_stats.csv", sep = ""))

#Building some ROC curves
#probs_fold  <- as.numeric(det_forh$"PROB_FOLD")
#probs_hoph  <- as.numeric(det_forh$"PROB_HOPH")
#probs_trans <- as.numeric(det_forh$"PROB_TRANS")
#prob_null   <- as.numeric(det_forh$"PROB_NULL")

#actually_hw <- as.character(det_forh$"HEATWAVE")
#actually_cs <- as.character(det_forh$"COLDSNAP")

#sim_roc <- roc(response = actually_hw, 
#               predictor = prob_null,
#               levels = c("0", "1") 
#               )

#Printing data for station 

det_forh_tot <- rbind(det_forh_tot, det_forh) #attaching to the big guy

#det_forh_tot  <- det_forh #Set up to combine 

#return(det_forh_tot)

} #End of i loop 



#With Hoph
total_final_df <- det_forh_tot%>%na.omit()#%>%
                                 #filter(IDENTIFIED_BIFURCATION == "null" & COLDSNAP == 1 & TRUE_NEG == 1) #Used to check for errors
#summary(total_final_df)

#Get general values
tru_pos_wh = sum(total_final_df$"TRUE_POS")
tru_neg_wh = sum(total_final_df$"TRUE_NEG")
fal_pos_wh = sum(total_final_df$"FALSE_POS")
fal_neg_wh = sum(total_final_df$"FALSE_NEG")

stats <- c("Accuracy", "Precision", "Recall", "C_Recall", "F1", "Specificity", "Prevalence", "Kappa")
vals_wh <- c(accuracy(tru_pos_wh, tru_neg_wh, fal_pos_wh, fal_neg_wh), precision(tru_pos_wh, fal_pos_wh), recall(tru_pos_wh, fal_neg_wh), crecall(tru_neg_wh, fal_pos_wh), f1_score(tru_pos_wh, tru_neg_wh, fal_pos_wh, fal_neg_wh), specificity(tru_neg_wh, fal_pos_wh), prevalence(tru_pos_wh, tru_neg_wh, fal_pos_wh, fal_neg_wh), kappa(tru_pos_wh, tru_neg_wh, fal_pos_wh, fal_neg_wh))
stat_tab_wh <- data.frame(stats, vals_wh)

#write.csv(stat_tab_wh, paste("/Users/milsbeary/Desktop/Bif_450/", class_thr_list[m], "/total_station_stats.csv", sep = ""))
write.csv(stat_tab_wh, paste("/home/mhkent/scratch/Bif_450/", class_thr_list[m], "/total_station_stats.csv", sep = ""))

#Building some ROC curves
wh_probs_fold  <- as.numeric(total_final_df$"PROB_FOLD")
wh_probs_hoph  <- as.numeric(total_final_df$"PROB_HOPH")
wh_probs_trans <- as.numeric(total_final_df$"PROB_TRANS")
wh_prob_null   <- as.numeric(total_final_df$"PROB_NULL")

wh_actually_hw <- as.character(total_final_df$"HEATWAVE")
wh_actually_cs <- as.character(total_final_df$"COLDSNAP")

#Building total vectors
wh_bf_chance <- c()
wh_is_hw_cs <- c()
for (i in 1:nrow(total_final_df)) { 
	#print(i/nrow(total_final_df))
	wh_bf_chance <- append(wh_bf_chance, max(c(wh_probs_fold[i], wh_probs_hoph[i], wh_probs_trans[i])))
	wh_is_hw_cs <- append(wh_is_hw_cs, max(c(wh_actually_hw[i], wh_actually_cs[i])))
	}


sim_roc <- roc(response = wh_is_hw_cs, 
               predictor = wh_bf_chance,
               levels = c("0", "1") 
               )

#sink(paste("/Users/milsbeary/Desktop/Bif_450/", class_thr_list[m], "/ROC_WITH_HOPH_STATS.txt", sep = ""))
sink(paste("/home/mhkent/scratch/Bif_450/", class_thr_list[m], "/ROC_WITH_HOPH_STATS.txt", sep = ""))

#print(class_thr_list[m])
#print("ROC Stats With Hoph")
print(sim_roc)
sink() 
              

roc_wh <- ggroc(sim_roc, legacy.axes = TRUE) + 
	labs(x = "False-positive rate", y = "True-positive rate", title = "All Stations With Hoph") +
	theme_bw()
	
#Export as PDF
#pdf(paste("/Users/milsbeary/Desktop/Bif_450/", class_thr_list[m], "/ROC_WITH_HOPH.pdf", sep = ""))
pdf(paste("/home/mhkent/scratch/Bif_450/", class_thr_list[m], "/ROC_WITH_HOPH.pdf", sep = ""))
#ggroc(sim_roc, legacy.axes = TRUE) + 
#	labs(x = "False-positive rate", y = "True-positive rate", title = "All Stations With Hoph") +
#	theme_bw()
print(roc_wh)
dev.off()

#With hoph
total_final_wo_hoph <- total_final_df%>%na.omit()%>%
										filter(IDENTIFIED_BIFURCATION != "hoph")
#summary(total_final_wo_hoph)

tru_pos_wo = sum(total_final_wo_hoph$"TRUE_POS")
tru_neg_wo = sum(total_final_wo_hoph$"TRUE_NEG")
fal_pos_wo = sum(total_final_wo_hoph$"FALSE_POS")
fal_neg_wo = sum(total_final_wo_hoph$"FALSE_NEG")

#Creating a table for statistics
stats <- c("Accuracy", "Precision", "Recall", "C_Recall", "F1", "Specificity", "Prevalence", "Kappa")
vals_wo <- c(accuracy(tru_pos_wo, tru_neg_wo, fal_pos_wo, fal_neg_wo), precision(tru_pos_wo, fal_pos_wo), recall(tru_pos_wo, fal_neg_wo), crecall(tru_neg_wo, fal_pos_wo), f1_score(tru_pos_wo, tru_neg_wo, fal_pos_wo, fal_neg_wo), specificity(tru_neg_wo, fal_pos_wo), prevalence(tru_pos_wo, tru_neg_wo, fal_pos_wo, fal_neg_wo), kappa(tru_pos_wo, tru_neg_wo, fal_pos_wo, fal_neg_wo))
stat_tab_wo <- data.frame(stats, vals_wo)

#write.csv(stat_tab_wo, paste("/Users/milsbeary/Desktop/Bif_450/", class_thr_list[m],"/total_station_stats_without_hoph.csv", sep = ""))
write.csv(stat_tab_wo, paste("/home/mhkent/scratch/Bif_450/", class_thr_list[m],"/total_station_stats_with_hoph.csv", sep = ""))


#Building some ROC curves
wo_probs_fold  <- as.numeric(total_final_wo_hoph$"PROB_FOLD")
#wo_probs_hoph  <- as.numeric(total_final_wo_hoph$"PROB_HOPH")
wo_probs_trans <- as.numeric(total_final_wo_hoph$"PROB_TRANS")
wo_prob_null   <- as.numeric(total_final_wo_hoph$"PROB_NULL")

wo_actually_hw <- as.character(total_final_wo_hoph$"HEATWAVE")
wo_actually_cs <- as.character(total_final_wo_hoph$"COLDSNAP")

#Building total vectors
wo_bf_chance <- c()
wo_is_hw_cs <- c()
for (i in 1:nrow(total_final_wo_hoph)) { 
	#print(i/nrow(total_final_wo_hoph))
	wo_bf_chance <- append(wo_bf_chance, max(c(wo_probs_fold[i], wo_probs_trans[i])))
	wo_is_hw_cs <- append(wo_is_hw_cs, max(c(wo_actually_hw[i], wo_actually_cs[i])))
	}

#By bifurcation              
wo_sim_roc <- roc(response = wo_is_hw_cs, 
               predictor = wo_bf_chance,
               levels = c("0", "1") 
               )
               
#sink(paste("/Users/milsbeary/Desktop/Bif_450/", class_thr_list[m], "/ROC_WITHOUT_HOPH_STATS.txt", sep = ""))
sink(paste("/home/mhkent/scratch/Bif_450/", class_thr_list[m], "/#ROC_WITHOUT_HOPH_STATS.txt", sep = ""))               
#print(class_thr_list[m])
#print("ROC Stats Without Hoph")
print(wo_sim_roc)
sink()

roc_woh <- ggroc(wo_sim_roc, legacy.axes = TRUE) + 
	labs(x = "False-positive rate", y = "True-positive rate", title = "All Stations Without Hoph") +
	theme_bw()

  
#Export PDF
#pdf(paste("/Users/milsbeary/Desktop/Bif_450/", class_thr_list[m], "/ROC_WITHOUT_HOPH.pdf", sep = ""))
pdf(paste("/home/mhkent/scratch/Bif_450/", class_thr_list[m], "/ROC_WITHOUT_HOPH.pdf", sep = ""))
#ggroc(wo_sim_roc, legacy.axes = TRUE) + 
#	labs(x = "False-positive rate", y = "True-positive rate", title = "All Stations Without Hoph") +
#	theme_bw()
print(roc_woh)
dev.off()


} #End of function or initial loop 

print("Function Imported")

#Setting thresholds beforehand 
#class_thresh_list <- c(.5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1) #List of thresholds
#class_thr_list <- c("thre_50", "thre_55", "thre_60", "thre_65", "thre_70", "thre_80", "thre_85", "thre_90", "thre_95", "thre_100")
#bif_thresh <- 450

#class_thresh_list <- c(.6, .7, .8, .9) #List of thresholds
#class_thr_list <- c("thre_60", "thre_70", "thre_80", "thre_90")
#bif_thresh <- 450

#Running the ooga booga function 
foreach (m=1:length(class_thresh_list), .packages=c('data.table', 'dplyr', 'tidyverse', 'lubridate')) %dopar% threshold_ooga_booga(m) 

#mclapply(m=1:length(class_thresh_list), threshold_ooga_booga)

#for (f in 1:length(class_thresh_list)) {

#print(f)
#threshold_ooga_booga(f)

#}
