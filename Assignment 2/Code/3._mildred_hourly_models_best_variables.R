#THIS SCRIPT DETERMINES VARIABLE INPORTANCE USING THE METHOD FROM ZURRELL https://github.com/damariszurell/SSDM-JSDM/blob/master/Univariate_variable_importance_blockCV_select07.r AND SIMPLE REGRESSION


#Last modified by M.H.Kent on December 17th, 2023

#Define a full numerical dataset
Data_Species <- as.data.frame(Mildred_final_ill[,-c("V1", "IntervalStart", "IntervalEnd", "ProgramCode", "StationName", "StationID", "StationType", "Latitude", "Longitude",  "Date_Time", "Precip_Amount_Flag", "DewPoint", "Month")])
summary(Data_Species) 
my_preds <- colnames(Data_Species)
my_preds <- my_preds[-which(my_preds %in% c("Methane_ppm") )]
Data_Species <- Data_Species[,c("Methane_ppm",my_preds)]
my_preds


#scaled_center <- attributes(scale(Data_Species[, c("Methane_ppm",my_preds)], center=T, scale=T))$`scaled:center`
#scaled_scale <- attributes(scale(Data_Species[, c("Methane_ppm",my_preds)], center=T, scale=T))$`scaled:scale`

Data_Species_std <- as.data.frame(scale(Data_Species[, c("Methane_ppm",my_preds)], center=T, scale=T))
Data_Species_std <- cbind(Mildred_final_ill[,-c("V1", "IntervalStart", "IntervalEnd", "ProgramCode", "StationName", "StationID", "StationType", "Latitude", "Longitude",  "Date_Time", "Precip_Amount_Flag", "DewPoint", "Month")], Data_Species_std)
#PREDICTORS SELECTION : 

select07_Logit <- function(pred_names, response_name, data, threshold=thre){
  # Function for calculating AIC - we use univariate GLMs with linear and quadratic terms
  var.imp <- function (predictor, response){
    glm_var <- lm(response ~ predictor)
    AIC(glm_var)
  }         
  # Calculate AIC for all predictor variables
  aic_imp <- apply(data[pred_names], 2, var.imp, response= data[,response_name])
  # Names of sorted variables
  sort_imp <- names(sort(aic_imp))
  # Calculate correlation matrix if not provided in function call
  cor_mat <- cor(data[pred_names], method='pearson')
  # Identifies correlated variable pairs:
  diag(cor_mat)=NA
  pairs <- which(abs(cor_mat)>= threshold, arr.ind=T) 
  # Identify which variables should be excluded
  exclude <- NULL
  for (i in 1:length(sort_imp))
  {
    if ((sort_imp[i] %in% row.names(pairs))&
        ((sort_imp[i] %in% exclude)==F)) {
      cv <- cor_mat[base::setdiff(row.names(cor_mat),exclude),sort_imp[i]]
      cv <- cv[base::setdiff(names(cv),sort_imp[1:i])]
      exclude <- c(exclude,names(which((abs(cv)>=threshold)))) 
    }
  }
  
  # Select set of weakly correlated predictors:
  pred_sel <- sort_imp[!(sort_imp %in% exclude)]
  # Return list with AIC, correlation matrix, and final predictors:
  return(list(AIC=sort(aic_imp), cor_mat=cor_mat, pred_sel=pred_sel))  
}  

# 1 FILTER : VARIABLE SELECTION : COLONEARITY AND VARIABLE IMPORTANCE 
var_sel <- c()
var_sel <- select07_Logit(pred_names=my_preds, response_name="Methane_ppm", data=Data_Species, threshold=0.6)
my_preds_Logit<- c( var_sel[["pred_sel"]])
my_preds_Logit <- my_preds_Logit[1:10]
print(my_preds_Logit) #See the top ten most significant terms


#Testing some models

#Full model
ill_mfive <- lm(Methane_ppm ~ Wind_Speed_Std_Dev + Relative_Humidity_Per + Outdoor_Air_Temp_C + Ang_To_Stat_deg + I(Wind_Speed_kmhr^7) + I(Outdoor_Air_Temp_C^7) + I(Wind_Speed_Std_Dev^5) + I(Ang_To_Stat_deg^2) + I(Outdoor_Air_Temp_C^2) + I(Ang_To_Stat_deg^7) , data =  Mildred_final)
summary(ill_mfive)
vif(ill_mfive)
lm_AIC(ill_mfive)
RS_ME(ill_mfive)


#This model has only one term of each parameter
ill_mseven <- lm(Methane_ppm ~ Wind_Speed_Std_Dev + Relative_Humidity_Per + Outdoor_Air_Temp_C + Ang_To_Stat_deg + I(Wind_Speed_kmhr^7), data =  Mildred_final)
summary(ill_mseven)
vif(ill_mseven)
lm_AIC(ill_mseven)
RS_ME(ill_mseven)

#Signficant terms
ill_meight <- lm(Methane_ppm ~ Wind_Speed_Std_Dev + Relative_Humidity_Per + Outdoor_Air_Temp_C + Ang_To_Stat_deg + I(Wind_Speed_Std_Dev^5) + I(Ang_To_Stat_deg^2) + I(Ang_To_Stat_deg^7), data =  Mildred_final)
16
vif(ill_meight)
lm_AIC(ill_meight)
RS_ME(ill_meight)

#Two terms for each parameter
ll_msix <- lm(Methane_ppm ~ Wind_Speed_Std_Dev + Relative_Humidity_Per + Outdoor_Air_Temp_C + Ang_To_Stat_deg + I(Outdoor_Air_Temp_C^7) + I(Ang_To_Stat_deg^2) + I(Outdoor_Air_Temp_C^2) , data =  Mildred_final)
summary(ill_msix)
vif(ill_msix)
lm_AIC(ill_msix)
RS_ME(ill_msix)

#No most dependent term
ill_mone <- lm(Methane_ppm ~ Relative_Humidity_Per + Outdoor_Air_Temp_C + Ang_To_Stat_deg + I(Wind_Speed_kmhr^7) + I(Outdoor_Air_Temp_C^7) + I(Ang_To_Stat_deg^2) + I(Outdoor_Air_Temp_C^2) + I(Ang_To_Stat_deg^7), data =  Mildred_final)
summary(ill_mone)
vif(ill_mone)
lm_AIC(ill_mone)
RS_ME(ill_mone)





