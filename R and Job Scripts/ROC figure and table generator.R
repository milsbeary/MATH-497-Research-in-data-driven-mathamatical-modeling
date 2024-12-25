#THIS SCRIPT GENERATES OUR ROC CURVES AND GENERAL RESULTS FOR THE HW PROJECT

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

#Vectors for thresholds and 
class_thresh_lis <- c(.5, .55, .6, .65, .7, .75, .8, .85, .9, .95, 1) #List of thresholds
class_thr_list <- c("thre_50", "thre_55", "thre_60", "thre_65", "thre_70", "thre_75", "thre_80", "thre_85", "thre_90", "thre_95", "thre_100")

#Uploading Results into dataframes
bif_450_wh_ic <- data.frame(data.table::fread(paste("/Users/milsbeary/Desktop/MATH 497 Assignment 3/Raw Outputs/Bif_450/", class_thr_list[1], "/total_station_stats.csv", sep = "")))
bif_450_wo_ic <- data.frame(data.table::fread(paste("/Users/milsbeary/Desktop/MATH 497 Assignment 3/Raw Outputs/Bif_450/",class_thr_list[1], "/total_station_stats_with_hoph.csv", sep = "")))
bif_400_wh_ic <- data.frame(data.table::fread(paste("/Users/milsbeary/Desktop/MATH 497 Assignment 3/Raw Outputs/Bif_400/",class_thr_list[1], "/total_station_stats.csv", sep = "")))
bif_400_wo_ic <- data.frame(data.table::fread(paste("/Users/milsbeary/Desktop/MATH 497 Assignment 3/Raw Outputs/Bif_400/",class_thr_list[1], "/total_station_stats_with_hoph.csv", sep = "")))

for (i in 2:length(class_thr_list)) {
	
print(i)

bif_450_wh_ic <- rbind(bif_450_wh_ic, data.frame(data.table::fread(paste("/Users/milsbeary/Desktop/MATH 497 Assignment 3/Raw Outputs/Bif_450/", class_thr_list[i], "/total_station_stats.csv", sep = ""))))

bif_450_wo_ic <- rbind(bif_450_wo_ic, data.frame(data.table::fread(paste("/Users/milsbeary/Desktop/MATH 497 Assignment 3/Raw Outputs/Bif_450/", class_thr_list[i], "/total_station_stats_with_hoph.csv", sep = ""))))

bif_400_wh_ic <- rbind(bif_400_wh_ic, data.frame(data.table::fread(paste("/Users/milsbeary/Desktop/MATH 497 Assignment 3/Raw Outputs/Bif_400/", class_thr_list[i], "/total_station_stats.csv", sep = ""))))

bif_400_wo_ic <- rbind(bif_400_wo_ic, data.frame(data.table::fread(paste("/Users/milsbeary/Desktop/MATH 497 Assignment 3/Raw Outputs/Bif_400/", class_thr_list[i], "/total_station_stats_with_hoph.csv", sep = ""))))

}


#Creating a dataframe for each parameter value for each thresholds 
par_colnames <- c("Thresholds", "400_w_h", "400_wo_h", "450_w_h", "450_wo_h") #Setting General Columnames
Accuracys <- data.frame(matrix(ncol = length(par_colnames), nrow = length(class_thresh_lis))) #Creates blank dataframe for parameter
Presisions <- data.frame(matrix(ncol = length(par_colnames), nrow = length(class_thresh_lis)))
Recalls <- data.frame(matrix(ncol = length(par_colnames), nrow = length(class_thresh_lis)))
F1s <- data.frame(matrix(ncol = length(par_colnames), nrow = length(class_thresh_lis)))
Specificities <- data.frame(matrix(ncol = length(par_colnames), nrow = length(class_thresh_lis)))
Prevalences <- data.frame(matrix(ncol = length(par_colnames), nrow = length(class_thresh_lis)))
C_Recalls <- data.frame(matrix(ncol = length(par_colnames), nrow = length(class_thresh_lis)))
Kappas <- data.frame(matrix(ncol = length(par_colnames), nrow = length(class_thresh_lis)))
colnames(Accuracys) <- par_colnames #Giving each dataframe columnmames
colnames(Presisions) <- par_colnames
colnames(Recalls) <- par_colnames
colnames(F1s) <- par_colnames
colnames(Specificities) <- par_colnames
colnames(Prevalences) <- par_colnames
colnames(C_Recalls) <- par_colnames
colnames(Kappas) <-  par_colnames

#Appending values for each 

#all_stats <- unique(bif_450_wh_ic$stats)

for (i in 1:length(class_thresh_lis)) {


Accuracys[i, 1]	<- class_thresh_lis[i] 
Accuracys[i, 2] <- bif_400_wh_ic[1+(8*(i-1)), "vals_wh"]
Accuracys[i, 3] <- bif_400_wo_ic[1+(8*(i-1)), "vals_wo"]
Accuracys[i, 4] <- bif_450_wh_ic[1+(8*(i-1)), "vals_wh"]
Accuracys[i, 5] <- bif_450_wo_ic[1+(8*(i-1)), "vals_wo"]

Presisions[i, 1] <- class_thresh_lis[i]
Presisions[i, 2] <- bif_400_wh_ic[2+(8*(i-1)), "vals_wh"]
Presisions[i, 3] <- bif_400_wo_ic[2+(8*(i-1)), "vals_wo"]
Presisions[i, 4] <- bif_450_wh_ic[2+(8*(i-1)), "vals_wh"]
Presisions[i, 5] <- bif_450_wo_ic[2+(8*(i-1)), "vals_wo"]

Recalls[i, 1] <- class_thresh_lis[i]
Recalls[i, 2] <- bif_400_wh_ic[3+(8*(i-1)), "vals_wh"]
Recalls[i, 3] <- bif_400_wo_ic[3+(8*(i-1)), "vals_wo"]
Recalls[i, 4] <- bif_450_wh_ic[3+(8*(i-1)), "vals_wh"]
Recalls[i, 5] <- bif_450_wo_ic[3+(8*(i-1)), "vals_wo"]

C_Recalls[i, 1] <- class_thresh_lis[i]
C_Recalls[i, 2] <- bif_400_wh_ic[4+(8*(i-1)), "vals_wh"]
C_Recalls[i, 3] <- bif_400_wo_ic[4+(8*(i-1)), "vals_wo"]
C_Recalls[i, 4] <- bif_450_wh_ic[4+(8*(i-1)), "vals_wh"]
C_Recalls[i, 5]	<- bif_450_wo_ic[4+(8*(i-1)), "vals_wo"]


F1s[i, 1] <- class_thresh_lis[i]
F1s[i, 2] <- bif_400_wh_ic[5+(8*(i-1)), "vals_wh"]
F1s[i, 3] <- bif_400_wo_ic[5+(8*(i-1)), "vals_wo"]
F1s[i, 4] <- bif_450_wh_ic[5+(8*(i-1)), "vals_wh"]
F1s[i, 5] <- bif_450_wo_ic[5+(8*(i-1)), "vals_wo"]

Specificities[i, 1] <- class_thresh_lis[i]
Specificities[i, 2] <- bif_400_wh_ic[6+(8*(i-1)), "vals_wh"]
Specificities[i, 3] <- bif_400_wo_ic[6+(8*(i-1)), "vals_wo"]
Specificities[i, 4] <- bif_450_wh_ic[6+(8*(i-1)), "vals_wh"]
Specificities[i, 5] <- bif_450_wo_ic[6+(8*(i-1)), "vals_wo"]

Prevalences[i, 1] <- class_thresh_lis[i]
Prevalences[i, 2] <- bif_400_wh_ic[7+(8*(i-1)), "vals_wh"]
Prevalences[i, 3] <- bif_400_wo_ic[7+(8*(i-1)), "vals_wo"]
Prevalences[i, 4] <- bif_450_wh_ic[7+(8*(i-1)), "vals_wh"]
Prevalences[i, 5] <- bif_450_wo_ic[7+(8*(i-1)), "vals_wo"]


Kappas[i, 1] <- class_thresh_lis[i]
Kappas[i, 2] <- bif_400_wh_ic[8+(8*(i-1)), "vals_wh"]
Kappas[i, 3] <- bif_400_wo_ic[8+(8*(i-1)), "vals_wo"]
Kappas[i, 4] <- bif_450_wh_ic[8+(8*(i-1)), "vals_wh"]
Kappas[i, 5] <- bif_450_wo_ic[8+(8*(i-1)), "vals_wo"]

}

#Making sure things are kosher and exporting the statistics we care about
Accuracys%>%mutate_if(is.numeric, round, digits=3)
Presisions%>%mutate_if(is.numeric, round, digits=3)
Recalls%>%mutate_if(is.numeric, round, digits=3)
C_Recalls%>%mutate_if(is.numeric, round, digits=3)
F1s%>%mutate_if(is.numeric, round, digits=3)
Specificities%>%mutate_if(is.numeric, round, digits=3)
Prevalences%>%mutate_if(is.numeric, round, digits=3)
Kappas%>%mutate_if(is.numeric, round, digits=3)

write.csv(Accuracys%>%mutate_if(is.numeric, round, digits=3), paste("/Users/milsbeary/Desktop/Accuracys.csv"))
write.csv(Presisions%>%mutate_if(is.numeric, round, digits=3), paste("/Users/milsbeary/Desktop/Presisions.csv"))
write.csv(Recalls%>%mutate_if(is.numeric, round, digits=3), paste("/Users/milsbeary/Desktop/Recalls.csv"))
write.csv(F1s%>%mutate_if(is.numeric, round, digits=3), paste("/Users/milsbeary/Desktop/F1s.csv"))
write.csv(Specificities%>%mutate_if(is.numeric, round, digits=3), paste("/Users/milsbeary/Desktop/Specificities.csv"))
write.csv(Prevalences%>%mutate_if(is.numeric, round, digits=3), paste("/Users/milsbeary/Desktop/Prevalences.csv"))
write.csv(Kappas%>%mutate_if(is.numeric, round, digits=3), paste("/Users/milsbeary/Desktop/Kappas.csv"))








#Manuel Input Result data 
perc <- c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
acc_450_wo <- c(0.745638509888088, 0.790205533062124, 0.828432133368635, 0.861734394926412, 0.891516017006913, 0.917844616856202, 0.941824520791576, 0.962066302859627,  0.980192231238746, 0.993919256520436, 0.999799697922813)*100 #450 without hoph
acc_450_wh <- c(0.234437340638043, 0.257363808121437, 0.280671932265416, 0.304759610439528, 0.330452412015959, 0.358927788394389, 0.391225144948327,  0.429455773842714, 0.479517759215249, 0.560499347664857, 0.999799697922813)*100 #450 with hoph
area_450_wo <- c(53.64, 53.64, 53.35, 53.5, 53.55, 53.57, 53.82, 53.97, 54.52, 54.35, 54.63)*.01 #Area under ROC
area_450_wh <- c(52.52, 52.52, 52.52, 52.52, 52.52, 52.52, 52.52, 52.52, 52.52, 52.52, 52.52)*.01 #Area under ROC

acc_400_wo <- c(0.749995207147102, 0.791817523533671, 0.828261315816484, 0.860417875798027, 0.890046208620559, 0.916768232192965, 0.941853732992973, 0.963915470494418,  0.981925584880255, 0.994837846565484, 0.999882128705422)*100
acc_400_wh <- c(0.232825901590009, 0.255065957766966, 0.277541756533079, 0.30137934493655, 0.327250840146461, 0.355785725033857, 0.388844861313136, 0.428116065606661, 0.478976275267091, 0.561874905953754, 0.999882128705422 )*100
area_400_wo <- c(53.69, 53.72, 53.38, 53.88, 54.17, 54.39, 54.67, 54.49, 55.32, 55.66, 54.66)*.01
area_400_wh <- c(53.6, 53.6, 53.6, 53.6, 53.6, 53.6, 53.6, 53.6, 53.6, 53.6, 53.6)*.01





#Accuracy plot
acc_df <- data.frame(perc, acc_450_wo, acc_450_wh, acc_400_wo, acc_400_wh)
p1 <- ggplot(data = acc_df) + 
	  geom_line(aes(x = perc, y = acc_450_wo, col="bf 450 wo Hoph")) +
	  geom_line(aes(x = perc, y = acc_450_wh, col="bf 450 w/ Hoph")) +
	  geom_line(aes(x = perc, y = acc_400_wo, col="bf 400 wo Hoph"), linetype = "dashed") +
	  geom_line(aes(x = perc, y = acc_400_wh, col="bf 400 w/ Hoph"), linetype = "dashed") +
	  scale_color_manual(values=c( "bf 450 wo Hoph" = "RED",
					               "bf 450 w/ Hoph" = "GREEN",
					               "bf 400 wo Hoph" = "BLACK",
					               "bf 400 w/ Hoph" = "BlUE"), 
					               name = "Group") +
	 xlab("Threshold (%)") + ylab("Model Accuracy (%)") + 
	 theme_classic()#+ 
	 #theme_bw()
p1
jpeg("/Users/milsbeary/Desktop/Presentation Photos/hw_accuracy.jpeg")
p1
dev.off()	  

#Area Plot
area_df <- data.frame(perc, area_450_wo, area_450_wh, area_400_wo, area_400_wh)
p2 <- ggplot(data = area_df) + 
	  geom_line(aes(x = perc, y = area_450_wo, col="bf 450 wo Hoph")) +
	  geom_line(aes(x = perc, y = area_450_wh, col="bf 450 w/ Hoph")) +
	  geom_line(aes(x = perc, y = area_400_wo, col="bf 400 wo Hoph"), linetype = "dashed") +
	  geom_line(aes(x = perc, y = area_400_wh, col="bf 400 w/ Hoph"), linetype = "dashed") +
	  scale_color_manual(values=c( "bf 450 wo Hoph" = "RED",
					               "bf 450 w/ Hoph" = "GREEN",
					               "bf 400 wo Hoph" = "BLACK",
					               "bf 400 w/ Hoph" = "BlUE"), 
					               name = "Group") +
	 xlab("Threshold (%)") + ylab("Area Under ROC Curve") + 
	 theme_classic()#+ 
	 #theme_bw()
p2	
jpeg("/Users/milsbeary/Desktop/Presentation Photos/hw_area.jpeg")
p2
dev.off()  



	 








