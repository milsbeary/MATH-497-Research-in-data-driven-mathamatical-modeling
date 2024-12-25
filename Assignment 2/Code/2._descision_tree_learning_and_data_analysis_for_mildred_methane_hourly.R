#DECISION TREE BUILDING AND DATA VISUALIZATION FOR MILDRED METHANE DATA 

#Last modified by M.H.Kent Dec 25, 2022

#Run "mildred_hourly_methane_generator.R" Before running this script 

#Making the learning dataset 
mild_for_learning <- Mildred_final%>%dplyr::select(c("Outdoor_Air_Temp_C", "Relative_Humidity_Per", "Ang_To_Stat_deg", "Wind_Speed_Std_Dev", "Wind_Direction_Std_Dev",  "Methane_ppm", "Wind_Speed_kmhr", "Precip_Amount_mm")) #Selecting needed variables
#colnames(mild_for_learning) <- c("Methane_ppm", "Temprature_C", "Relative_Humidity_Percent", "Wind_Direction_C", "Wind Speed_km/h",  "Precip_Amount_mm", "Dew_Point_C") #Giving dataframe cillumns more reasonable names                           
summary(mild_for_learning) 

#Take a look at significant relationships

#Testing what realationships are significant
#plot(mild_for_learning$Temprature_C, mild_for_learning$Methane_ppm)  
#plot(mild_for_learning$Temprature_C, mild_for_learning$Methane_ppm) 
#plot(mild_for_learning$Wind_Direction_C, mild_for_learning$Methane_ppm)  
#hist(mild_for_learning$Methane_ppm)

#plot(mild_for_learning$"Wind Speed_km/h", mild_for_learning$Methane_ppm)  


###MACHINE LEARNING ANALYSIS###

##Classification Trees

#Loading some extra needed packages 
library('datasets')
library('caTools')
library('party')
library('magrittr')

#looking at variable catagories

#hist(mild_for_learning$Methane_ppm) #Tanking a look 

#length(sort(unique(mild_for_learning$Methane_ppm))) #Looking at raw groups

#Building catagorical variable schemes
mild_for_tree <- mild_for_learning%>%mutate(meth_cat_1 = "NA", meth_cat_2 = "NA", math_cat_3) 
summary(mild_for_tree)


#Building groups 

#0.7ppm bins
for (i in 1:length(mild_for_tree$meth_cat_1)) {
meth_mes <- mild_for_tree$Methane_ppm[i]
if (meth_mes < 2.1) {
mild_for_tree$meth_cat_1[i] <- "A"
} else if (meth_mes < 2.8) {
mild_for_tree$meth_cat_1[i] <- "B"
} else if (meth_mes < 3.5) {
mild_for_tree$meth_cat_1[i] <- "C"
} else {
mild_for_tree$meth_cat_1[i] <- "D"
}
}


#0.4ppm bins
for (i in 1:length(mild_for_tree$meth_cat_2)) {
meth_mes <- mild_for_tree$Methane_ppm[i]
if (meth_mes < 2.1) {
mild_for_tree$meth_cat_2[i] <- "A"
} else if (meth_mes < 2.5) {
mild_for_tree$meth_cat_2[i] <- "B"
} else if (meth_mes < 2.9) {
mild_for_tree$meth_cat_2[i] <- "C"
} else if (meth_mes < 3.3) {
mild_for_tree$meth_cat_2[i] <- "D" 
} else if (meth_mes < 3.7) {
mild_for_tree$meth_cat_2[i] <- "E" 
} else if (meth_mes < 4.1) {
mild_for_tree$meth_cat_2[i] <- "F" 
} else {
mild_for_tree$meth_cat_2[i] <- "G"
}
}
#summary(mild_for_tree)

unique(mild_for_tree$meth_cat_2) #Making sure we got 7 groups
unique(mild_for_tree$meth_cat_1) #Making sure we got 4 groups


#Creating training and test data sets

#1/9 test trainig split
tree_train_1 <- data.frame(matrix(ncol = length(mild_for_tree), nrow = 1)) #Training dataset 
colnames(tree_train_1) <- colnames(mild_for_tree)
tree_train_1

tree_test_1 <-  data.frame(matrix(ncol = length(mild_for_tree), nrow = 1)) #Test dataset
colnames(tree_test_1) <- colnames(mild_for_tree)
tree_test_1

#Building first split
tree_split <- 9 #Define the factor we are splitting by 

for (i in 1:length(mild_for_tree$meth_cat_1)) { #Appending what we need 

if ((i/tree_split) %% 1 == 0) { #Check to see if the training data is a multable of the factor 

tree_test_1 <- rbind(tree_test_1, mild_for_tree[i,]) #Bind the selected collumn to the whole dataframe 

} else {

tree_train_1 <- rbind(tree_train_1, mild_for_tree[i,]) #The remaining data is used as training data

} #End of if else statement 

} #End of for loop 
summary(tree_train_1) #Making sure its kosher
summary(tree_test_1)

#1/6 test training split
tree_train_2 <- data.frame(matrix(ncol = length(mild_for_tree), nrow = 1)) #Training dataset 
colnames(tree_train_2) <- colnames(mild_for_tree)
tree_train_2

tree_test_2 <-  data.frame(matrix(ncol = length(mild_for_tree), nrow = 1)) #Test dataset
colnames(tree_test_2) <- colnames(mild_for_tree)
tree_test_2


#Building seccond split

tree_split2 <- 6

for (i in 1:length(mild_for_tree$meth_cat_1)) { #Appending what we need 

if ((i/tree_split2) %% 1 == 0) { #Check to see if the training data is a multable of the factor 

tree_test_2 <- rbind(tree_test_2, mild_for_tree[i,]) #Bind the selected collumn to the whole dataframe 

} else {

tree_train_2 <- rbind(tree_train_2, mild_for_tree[i,]) #The remaining data is used as training data

} #End of if else statement 

} #End of for loop 
summary(tree_train_2)  #Keeping it kosher 
summary(tree_test_2)



#Finalizing and viewing training and test data 
tree_train1_fin <- tree_train_1%>%dplyr::select(-Methane_ppm)%>%
                               na.omit()%>% #Remove the singular NA row
					           mutate(meth_cat_1 = as.factor(meth_cat_1), 
					                  meth_cat_2 = as.factor(meth_cat_2))
#tree_train1_fin

tree_test1_fin <- tree_test_1%>%dplyr::select(-Methane_ppm)%>%
					   na.omit()%>%
					   mutate(meth_cat_1 = as.factor(meth_cat_1), 
					   		  meth_cat_2 = as.factor(meth_cat_2))
#tree_test1_fin

tree_train2_fin <- tree_train_2%>%dplyr::select(-Methane_ppm)%>%
                               na.omit()%>% #Remove the singular NA row
					           mutate(meth_cat_1 = as.factor(meth_cat_1), 
					                  meth_cat_2 = as.factor(meth_cat_2))
#tree_train2_fin

tree_test2_fin <- tree_test_2%>%dplyr::select(-Methane_ppm)%>%
					   na.omit()%>%
					   mutate(meth_cat_1 = as.factor(meth_cat_1), 
					   		  meth_cat_2 = as.factor(meth_cat_2))
#tree_test2_fin

raw_train1_fin <- tree_train_1%>%#dplyr::select(-Methane_ppm)%>%
                               na.omit()%>% #Remove the singular NA row
					           mutate(meth_cat_1 = as.factor(meth_cat_1), 
					                  meth_cat_2 = as.factor(meth_cat_2), 
					                  meth_raw_1 = as.factor(Methane_ppm))
#summary(raw_train1_fin)

raw_test1_fin <- tree_test_1%>%#dplyr::select(-Methane_ppm)%>%
					   na.omit()%>%
					   mutate(meth_cat_1 = as.factor(meth_cat_1), 
					   		  meth_cat_2 = as.factor(meth_cat_2),
					   		  meth_raw_1 = as.factor(Methane_ppm))
#summary(raw_test1_fin)

#Creating raw datasets
raw_train2_fin <- tree_train_2%>%#dplyr::select(-Methane_ppm)%>%
                               na.omit()%>% #Remove the singular NA row
					           mutate(meth_cat_1 = as.factor(meth_cat_1), 
					                  meth_cat_2 = as.factor(meth_cat_2),
					                  meth_raw_1 = as.factor(Methane_ppm))
#tree_train2_fin

raw_test2_fin <- tree_test_2%>%#dplyr::select(-Methane_ppm)%>%
					   na.omit()%>%
					   mutate(meth_cat_1 = as.factor(meth_cat_1), 
					   		  meth_cat_2 = as.factor(meth_cat_2),
					   		  meth_raw_1 = as.factor(Methane_ppm))


#Making and evaluating descision tree models
 
#GROUP 1
dec_model_11 <- ctree(meth_cat_1 ~ ., tree_train1_fin) #Defining a model 

dec_predict_11 <- predict(dec_model_11, tree_test1_fin) #Making predictions with our model 

dec_pred_tab_11 <- table(tree_test1_fin$meth_cat_1, dec_predict_11)

ac_test_11 <- sum(diag(dec_pred_tab_11))/sum(dec_pred_tab_11) 
print(paste('Accuracy for set one group one is found to be', ac_test_11)) 

#plot(dec_model_11) #Looking at model 

#jpeg("/Users/milsbeary/Desktop/Presentation Photos/basic_tree.jpeg")
#plot(dec_model_11)
#dev.off()


#GROUP 2
dec_model_21 <- ctree(meth_cat_2 ~ ., tree_train1_fin) #Defining a model 

dec_predict_21 <- predict(dec_model_21, tree_test1_fin) #Making predictions with our model 

dec_pred_tab_21 <- table(tree_test1_fin$meth_cat_2, dec_predict_21)

ac_test_21 <- sum(diag(dec_pred_tab_21))/sum(dec_pred_tab_21) 
print(paste('Accuracy for set two group one is found to be', ac_test_21)) 

#plot(dec_model_21)


#GROUP 3 
dec_model_12 <- ctree(meth_cat_1 ~ ., tree_train2_fin) #Defining a model 

dec_predict_12 <- predict(dec_model_12, tree_test2_fin) #Making predictions with our model 

dec_pred_tab_12 <- table(tree_test2_fin$meth_cat_1, dec_predict_12)

ac_test_12 <- sum(diag(dec_pred_tab_12))/sum(dec_pred_tab_12) 
print(paste('Accuracy for set two group one is found to be', ac_test_12)) 

#plot(dec_model_12)


#GROUP 4
dec_model_22 <- ctree(meth_cat_2 ~ ., tree_train2_fin) #Defining a model 

dec_predict_22 <- predict(dec_model_22, tree_test2_fin) #Making predictions with our model 

dec_pred_tab_22 <- table(tree_test2_fin$meth_cat_2, dec_predict_22)

ac_test_22 <- sum(diag(dec_pred_tab_22))/sum(dec_pred_tab_22) 
print(paste('Accuracy for set two group two is found to be', ac_test_22)) 

#plot(dec_model_22)

					   		  
#GROUP 5
raw_model_1 <- ctree(meth_raw_1 ~ ., raw_train1_fin) #Defining a model 

raw_predict_1 <- predict(raw_model_1, raw_test1_fin) #Making predictions with our model 

raw_pred_tab_1 <- table(raw_test1_fin$meth_raw_1, raw_predict_1)

raw_test_1 <- sum(diag(raw_pred_tab_1))/sum(raw_pred_tab_1) 
print(paste('Accuracy for raw one group one is found to be', raw_test_1)) 
#GROUP 6
raw_model_2 <- ctree(meth_raw_1 ~ ., raw_train2_fin) #Defining a model 

raw_predict_2 <- predict(raw_model_2, raw_test2_fin) #Making predictions with our model 

raw_pred_tab_2 <- table(raw_test2_fin$meth_raw_1, raw_predict_2) #Very bad

raw_test_2 <- sum(diag(raw_pred_tab_2))/sum(raw_pred_tab_2) 
print(paste('Accuracy for raw one group two is found to be', raw_test_2)) 	#oh noooo				
					
#Looking at all confusion matricies 
dec_pred_tab_11
dec_pred_tab_21
dec_pred_tab_12	   		  
dec_pred_tab_22
raw_pred_tab_1
raw_pred_tab_2

#Future work and some of thoughts

#try booted regression trees
#support vector trees 
#deep nural networks

#Observations from the map
#Max exposeure from wind can be conciderd anywhere from 0-90 degrees 
#Moderate exposure can be conciderd anywhere from 91-110 and 325-365 degrees
#Minumum exposure can be conciderd to be everyting else 





  