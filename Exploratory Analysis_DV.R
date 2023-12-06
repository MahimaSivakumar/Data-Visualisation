source("Rfunctions.R")
require(Information)
library(plotrix)                 
library(cluster)
library(greybox)
library(dbplyr)
library(corrplot)
library(cluster)
library(ggplot2)
library(plotly)
library(reshape2)
library(lattice)
library(arulesViz)
library("viridis")
library("tidyr")
library(scales)

data = read.csv("airlinesData96.csv" , stringsAsFactors = TRUE)
data[, c("Inflightwifiservice", "DepartureArrivaltimeconvenient", "EaseofOnlinebooking", "Gatelocation", "Foodanddrink", "Onlineboarding", "Seatcomfort", 
         "Inflightentertainment", "Onboardservice", "Legroomservice", "Baggagehandling", "Checkinservice", 
         "Inflightservice", "Cleanliness")] <- lapply(data[, c("Inflightwifiservice", "DepartureArrivaltimeconvenient", "EaseofOnlinebooking", "Gatelocation", "Foodanddrink", "Onlineboarding", "Seatcomfort", 
                                                                      "Inflightentertainment", "Onboardservice", "Legroomservice", "Baggagehandling", "Checkinservice", 
                                                                      "Inflightservice", "Cleanliness")], function(x) ifelse(x == 0, NA, x))
airline_data = na.omit(data)
str(airline_data)

#renaming it for summary purpose
satisfaction_levels = c("totally dissatisfied" , "somewhat dissatisfied" , "neutral" ,"somewhat satisfied" ,"totally satisfied")
for(col in c("Inflightwifiservice", "DepartureArrivaltimeconvenient", "EaseofOnlinebooking", "Gatelocation", "Foodanddrink", "Onlineboarding", "Seatcomfort", 
             "Inflightentertainment", "Onboardservice", "Legroomservice", "Baggagehandling", "Checkinservice", 
             "Inflightservice", "Cleanliness")) 
{
  airline_data[[col]] <- factor(airline_data[[col]], levels = 1:5, labels = satisfaction_levels)
}
summary(airline_data)
#initial analysis 
attach(airline_data)
#customer type vs age 
# create bar plot
ggplot(airline_data, aes(x = Age, fill = CustomerType)) +
  geom_bar(position = "dodge") +
  labs(x = "Age", y = "Frequency", fill = "Customer Type") + ggtitle("Customer Type Based on Age Factor")+
  scale_fill_viridis(discrete = TRUE , option = "D")+ theme_minimal()


#outliers
par(mfrow = c(2,2))
boxplot(FlightDistancemiles , main="Flight Distance in Miles",ylab="Distance in Miles",col="yellow")
boxplot(Age,main="Age",ylab="Age",col="yellow")
boxplot(DepartureDelayinMinutes,main="Departure Delay in Minutes", ylab="Delay in Minutes",col="yellow")
boxplot(ArrivalDelayinMinutes,main="Arrival Delay in Minutes",ylab="Delay in Minutes",col="yellow")

satisfied_pct <- sum(satisfaction == "satisfied")/nrow(airline_data)*100
neutral_pct <- sum(satisfaction == "neutral or dissatisfied")/nrow(airline_data)*100
ggplot() + 
  geom_bar(aes(x = c("Satisfied", "Neutral or dissatisfied"), y = c(satisfied_pct, neutral_pct)),stat="identity",fill=c("#ffd966","#2E045D")) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Satisfaction", y = "Percentage", title = "              Overall Customer Satisfaction")

ggplot(airline_data, aes(x = CustomerType, fill = satisfaction)) +
  geom_bar() +
  labs(title = "Satisfaction Level based on Customer Type ",
       x = "Customer", y = "Frequency") 

mean(airline_data$CustomerType =="Loyal Customer" & airline_data$satisfaction =="satisfied" ) # 39% of customer of loyal customers are satisified 
mean(airline_data$Age <18 & airline_data$TypeofTravel =="Business travel" ) #2% of the kids are travelling in business travel journey
mean(airline_data$DepartureDelayinMinutes==0)
mean(airline_data$ArrivalDelayinMinutes==0)

#Cross-Tabulation
Type_satisfaction=table(airline_data$CustomerType, airline_data$satisfaction)
Type_satisfactionper = round(prop.table(Type_satisfaction)*100, 2)
Type_satisfactionper

table(airline_data$Inflightwifiservice, airline_data$satisfaction)
Type_satisfactionpercent = round(prop.table(Type_satisfaction)*100, 2)
class_satisfaction = as.data.frame.table(Type_satisfactionpercent)
colnames(class_satisfaction)=c("Inflightwifiservice","TypeofTravel","Percent")
ggplot(class_satisfaction, aes(x = Inflightwifiservice, y = Percent, fill = TypeofTravel)) +
  geom_bar(stat="identity", position="dodge") + 
  labs(title = "SatisfactionLevel based on Class",
       x = "Inflightwifiservice", y = "Percentage",color="TypeofTravel") +
  theme_bw()




delay=table(airline_data$Inflightwifiservice,airline_data$satisfaction)
delaypercent = round(prop.table(delay)*100, 2)
delay_satisfaction = as.data.frame.table(delaypercent)
colnames(delay_satisfaction )=c("Inflightwifiservice","Satisfaction","Percent")
ggplot(delay_satisfaction, aes(x = Inflightwifiservice, y = Percent, fill = Satisfaction)) +
  geom_bar(stat="identity", position="dodge") + 
  labs(title = "Satisfaction Level based on Delay",
       x = "Inflightwifiservice", y = "Percentage",color="Satisfaction") +
  theme_bw()


binage <- data.frame(airline_data$Age,
                         bin=cut(airline_data$Age,
                                 c(0, 10, 20, 30,40,50,60,70,80,90),
                                 include.lowest=TRUE))
age = table(airline_data$satisfaction, binage$bin)
agepercent = round(prop.table(age)*100, 2)
age_satisfaction = as.data.frame.table(agepercent)
colnames(age_satisfaction )=c("AgeGroup","Satisfaction","Percent")
ggplot(age_satisfaction, aes(x = AgeGroup, y = Percent, fill =Satisfaction )) +
  geom_bar(stat="identity", position="dodge") + 
  labs(title = "                              Satisfaction Level based on Age Group",
       x = "Satisfaction", y = "Percentage") +
  theme_bw()
boxplot(airline_data$FlightDistancemiles)

hist(airline_data$Age ,breaks=10, xlab ="Age" , main="Frequency of Age")
hist(airline_data$Age ,breaks=15)
hist(airline_data$FlightDistancemiles, breaks = 10, xlab ="Distance in miles" , main="Frequency of Flight Distance")


# conditional probability according to flight delay and satisfaction
attach(airline_data)
sum(satisfaction == "satisfied" & Totaldelay == "No Delay")/sum(Totaldelay == "No Delay")
sum(satisfaction == "satisfied" & Totaldelay == "Late Arrival")/sum(Totaldelay == "Late Arrival")
sum(satisfaction == "satisfied" & Totaldelay == "Late Departure")/sum(Totaldelay == "Late Departure")
sum(satisfaction == "satisfied" & Totaldelay == "Late Departure,Early Arrival")/sum(Totaldelay == "Late Departure,Early Arrival")
sum(satisfaction == "satisfied" & Totaldelay == "Late Departure,Late Arrival")/sum(Totaldelay == "Late Departure,Late Arrival")
sum(satisfaction == "satisfied" & Totaldelay == " On-Time Arrival (Duration)")/sum(Totaldelay == " On-Time Arrival (Duration)")

# conditional probability for loyal customer with satisfaction
sum(satisfaction =="satisfied" & CustomerType == "Loyal Customer")/sum(CustomerType == "Loyal Customer")
sum(satisfaction =="satisfied" & CustomerType == "Loyal Customer")/sum(satisfaction =="satisfied")
#Plots
plot(airline_data$DepartureDelayinMinutes,airline_data$ArrivalDelayinMinutes) # to handle missing value 
thigmophobe.labels(airline_data$DepartureDelayinMinutes,airline_data$ArrivalDelayinMinutes, rownames(airline_data))
hist(airline_data$Age ,breaks=10, xlab ="Age" , main="Frequency of Age",col = "#9FC5E8")
ggplot(airline_data, aes(x = airline_data$Age)) + 
  geom_histogram(binwidth = 5, boundary = 10, fill = "#9FC5E8", color = "black") + 
  labs(x = "Age", y = "Frequency", title = "                                      Age Distribution")
hist(airline_data$Age ,breaks=15)
hist(airline_data$FlightDistancemiles, breaks = 10, xlab ="Distance in miles" , main="Frequency of Flight Distance")
#probability of being satisfied given that they are loyal customer 

par(mfrow = c(2,2))
cc_barplot(Data = airline_data, "Inflightwifiservice","satisfaction", freq = "relfreq",main = "Inflightwifiservice vs Satisfaction")
cc_barplot(Data = airline_data, "Onlineboarding","satisfaction", freq = "relfreq",main = "OnlineBoarding vs Satisfaction")
cc_barplot(Data = airline_data, "Inflightentertainment","satisfaction", freq = "relfreq",main = "InflightEntertainment vs Satisfaction")
#probability of loyal customer given that they are satisfied 
cc_barplot(Data = airline_data, "CustomerType","satisfaction", freq = "condprob")

cc_hist(airline_data,"Age", "satisfaction", breaks="Sturges")
cc_hist(airline_data,"FlightDistancemiles","satisfaction",  breaks="Sturges")
boxplot(Age, col = "green",notch = TRUE)
boxplot(FlightDistancemiles, col = "yellow",notch = TRUE)

cc_boxplot(airline_data, "Age", "satisfaction")
cc_boxplot(airline_data, "FlightDistancemiles", "satisfaction")
cc_boxplot(airline_data, "Onlineboarding", "satisfaction")
cc_boxplot(airline_data, "Checkinservice", "satisfaction")

densityplot(~ airline_data$Onlineboarding, data = airline_data, main="Probability(Online Boarding|Satisfaction)", groups = airline_data$satisfaction, auto.key=TRUE,xlab ="Online Boarding")

#for (i in colnames(airline_data)) {
 # cc_barplot(airline_data, i,"satisfaction", freq = "condprob")
#}

#scatterplot  waste
plot(airline_data$Inflightwifiservice,airline_data$satisfaction)

data_spread = data.frame(airline_data[,c("Gender","CustomerType","Age","TypeofTravel","Class","FlightDistancemiles","DepartureDelayinMinutes","ArrivalDelayinMinutes","satisfaction")])
greybox::spread(data_spread)
#WOE & IV
bolean= airline_data$satisfaction =="satisfied"
class(bolean)
bolean <- 1*bolean
class(bolean)
airline_data["Target"] = bolean
str(airline_data)
IV <- create_infotables(data=airline_data[,-ncol(airline_data) - 2], y="Target", bins = 5)
IV
p= ggplot(data = IV$Summary, aes(x = reorder(Variable, -IV), y = IV)) +
  geom_bar(stat = "identity", fill = "#2E045D") +
  labs(x = "Predictor Variable", y = "Information Value") +
  ggtitle(" Information Value Plot")+scale_fill_viridis(discrete = TRUE , option = "D")
p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_infotables(IV,"Onlineboarding",show_values = TRUE)
plot_infotables(IV,"Inflightwifiservice",show_values = TRUE)
plot_infotables(IV,"Class",show_values = TRUE)
plot_infotables(IV,"TypeofTravel",show_values = TRUE)
plot_infotables(IV,"Inflightentertainment",show_values = TRUE)
plot_infotables(IV,"Gatelocation",show_values = TRUE)
plot_infotables(IV,"FlightDistancemiles",show_values = TRUE)




#MDS Implementation 
Mds_data = data[,-ncol(data)]
Mds_data = na.omit(Mds_data)

data_spread = data.frame(Mds_data[,c("Gender","CustomerType","Age","TypeofTravel","Class","FlightDistancemiles","DepartureDelayinMinutes","ArrivalDelayinMinutes","satisfaction")])
greybox::spread(data_spread)
str(Mds_data)
par(mfrow = c(1,1))
#Correlation 
corr_airline =  data.frame(assoc(x = Mds_data)[[1]])
corrplot(association(Mds_data)$value)

# to separate the variable whose correlation >0.5
impcorr = data.frame(rowno = 'i',columnno = 'j',cor = 0)
for (i in 1:23) {
  for (j in 1:23) {
    if (corr_airline[i,j] > 0.5 & i!=j) {
      impcorr = rbind(impcorr,c(colnames(Mds_data)[i],colnames(Mds_data)[j],corr_airline[i,j]))
    }
  }
}




#Mds dismilarities matrix 
set.seed(2582)
airline_matrix = daisy(Mds_data, metric="gower")
airline_MDS <- cmdscale(airline_matrix, k=2)
colnames(airline_MDS) <- c("D1","D2")
plot(airline_MDS)

# Create a data frame from the MDS output
airlinesMDS = data.frame(airline_MDS)
# Add row names to the data frame
airlinesMDS$ID = rownames(airlinesMDS)
# Plot the data using ggplot2
ggplot(airlinesMDS, aes(x = D1, y = D2, label = ID)) +
  geom_text() +
  labs(title = "Airline Data MDS Plot", x = "Dimension 1", y = "Dimension 2") +
  theme_minimal()
nDimensions <- ncol(airlinesMDS)
airlinesStress <- vector("numeric",nDimensions)
for(i in 1:nDimensions){
  # Do MDS
  airlinesMDSTest <- cmdscale(airline_matrix,k=i)
  # Produce dissimilarities matrix for the new dimensions
  airlinesMDSDist <- daisy(airlinesMDSTest,"gower")
  # Calculate stress metrics
  airlinesStress[i] <- sqrt(sum((airline_matrix -
                                   airlinesMDSDist)^2)/sum(airlinesDissimMatrix^2))
}


Mds_data = data[,-ncol(data)]
Mds_data = na.omit(Mds_data)
ncol(Mds_data)
#Mds dismilarities matrix 
set.seed(2582)
airline_matrix = daisy(Mds_data, metric="gower")
airline_MDS <- cmdscale(airline_matrix, k=3)
colnames(airline_MDS) <- c("D1","D2","D3")
plot(airline_MDS)
# Create a data frame from the MDS output
airlinesMDS = data.frame(airline_MDS)

fig <- plot_ly(data = data.frame(airlinesMDS), x = ~D1, y = ~D2, z = ~D3, size = 1,
               color = factor(Mds_data[,ncol(Mds_data)]), colors = c("#C39BD3", "#F7DC6F"),
               type = "scatter3d", mode = "markers")
fig

MDS_flight <- cbind(as.data.frame(airlinesMDS),Mds_data)
greybox::assoc(MDS_flight)$value
spread(MDS_flight)

#Stepwise 
Mds_step = stepwise(MDS_flight,ic="AICc")
response <- Mds_data[,ncol(Mds_data)]
predictors <- data[,1]
Mds_step <- stepAIC(lm(D1 ~ ., data = MDS_flight), direction = "both")
