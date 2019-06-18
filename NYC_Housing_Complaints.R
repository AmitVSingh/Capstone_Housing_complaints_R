## Library

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet")
if(!require(randomForest)) install.packages("randomForest")


library(tidyverse)
library(ggplot2)
library(caret)
library(lubridate)
library(leaflet)
library(randomForest)


##  **\textcolor{red}{Problem 1: Which type of complaint should the Department of Housing 
##Preservation and Development of New York City focus on first?}**

dl <- tempfile()

# download file. it may take few minutes (fileSize = 2.57GB, nrows ~ 5800000).
url   <- "https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?$limit=100000000&
Agency=HPD&$select=created_date,unique_key,complaint_type,incident_zip,incident_
address,street_name,address_type,city,resolution_description,borough,latitude,
longitude,closed_date,location_type,status"
download.file(url, dl)

df_NYC = read.csv(dl) 

# get the idea of dataframe's number of rows and columns
dim(df_NYC)

rm(dl)


## Basic Exploratory Analysis and Summary Statistics
# 7 rows of the dataset with header
head(df_NYC)

# columns names 
colnames(df_NYC)

# datatype of columns
sapply(df_NYC, class)

# basic summary statistics
summary(df_NYC)


## Number of Housing Complaints 
df_NYC%>%
  group_by(complaint_type) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


## we can visulize the complaints type and number of complainted in the bar plot

df_NYC %>% ggplot(aes(complaint_type))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## After reading the New york city data file, one can see that HEAT/HOT WATER complaint column has been merged with HEATING. Similarily for complaint type PAINT - PLASTER. Lets merge them together.

df_NYC$complaint_type[df_NYC$complaint_type %in% "HEAT/HOT WATER"] <- "HEATING"
df_NYC$complaint_type[df_NYC$complaint_type %in% "PAINT - PLASTER"] <- "PAINT/PLASTER"

df_NYC %>% ggplot(aes(complaint_type))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Temoral Evolution of Complaints Type
# Convert `timestamp to`POSIXct`
dt <- as.POSIXct(df_NYC$created_date)
df_NYC <- df_NYC %>% mutate(year = format(dt, "%Y"), month = format(dt, "%m"))

rm(dt)

complaint_year <- df_NYC %>%
  na.omit() %>% # omit missing values
  #select(year, complaint_type) %>% # select columns we are interested in
  mutate(year = as.factor(year)) %>% # turn year in factors
  mutate(year = as.numeric(levels(year))[year]) %>%
  filter(year < 2019) %>%
  group_by(year, complaint_type) %>% # group data by year and complaint_Type
  summarise(number = n())  # count


complaint_year %>%
  filter(complaint_type %in% c("HEATING", "PLUMBING", "GENERAL CONSTRUCTION", "PAINT/PLASTER")) %>%
  ggplot(aes(x = year, y = number)) +
  geom_line(aes(color=complaint_type)) +
  scale_fill_brewer(palette = "Paired")



## Problem 2: Should the Department of Housing Preservation and Development of New York City
##focus on any particular set of boroughs, ZIP codes, or street (where the complaints are severe)
##for the specific type of complaints you identified in response to Question 1?

# number of complaints for each borough
df_NYC %>%
  na.omit() %>% # omit missing values
  #select(year, complaint_type) %>% # select columns we are interested in
  group_by(borough) %>% # group data by year and complaint_Type
  summarise(number = n())  # count

# bar plot for complaints in each borough
df_NYC %>% ggplot(aes(borough))+
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




## This chunk of code produces an interactive map of NYC housing complain area

lat <- df_NYC$latitude %>% na.omit()  
lng <- df_NYC$longitude %>% na.omit()  

df_geo <- data.frame(lat = runif(1000, min = min(lat), max = max(lat)),
lng = runif(1000, min = min(lng), max = max(lng)))

# The interactive map shows cluster of complaint prone area.

df_geo %>% leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())




## Problem 3: Does the Complaint Type that you identified in response to question 1 have an
##obvious relationship with any particular characteristic or characteristics of the houses or 
##buildings?**



# THE ZIP FILE CAN BE DOWNLOADED FROM THE FOLLOWING LINK: "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_18v1.zip" 

## download file. it may take couple of minutes (fileSize = 46MB).
#dl <- tempfile()
#zip.file.location <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_18v1.zip"
#download.file(zip.file.location, dl)
#BK_18v1 <- read.csv(unzip(dl,PLUTO_for_WEB/BK_18v1.csv))

#rm(dl)

# I am using my local directory to access the PLUTO files
BK_18v1 <- read.csv('./PLUTO_for_WEB/BK_18v1.csv')
BX_18v1 <- read.csv('./PLUTO_for_WEB/BX_18v1.csv')
MN_18v1 <- read.csv('./PLUTO_for_WEB/MN_18v1.csv')
QN_18v1 <- read.csv('./PLUTO_for_WEB/QN_18v1.csv')

# dimension of data frame
dim(BK_18v1)



# The recommended fields are Address, BldgArea, BldgDepth, BuiltFAR, CommFAR, FacilFAR, Lot, LotArea, LotDepth, NumBldgs, NumFloors, OfficeArea, ResArea, ResidFAR, RetailArea, YearBuilt, YearAlter1, ZipCode, YCoord, and XCoord.


df_BK <- BK_18v1 %>% select('Address', 'BldgArea', 'BldgDepth', 'BuiltFAR', 'CommFAR', 'FacilFAR', 'Lot', 'LotArea', 'LotDepth', 'NumBldgs', 'NumFloors', 'OfficeArea', 'ResArea', 'ResidFAR', 'RetailArea', 'YearBuilt', 'YearAlter1', 'ZipCode', 'YCoord', 'XCoord')

df_BX <- BX_18v1 %>% select('Address', 'BldgArea', 'BldgDepth', 'BuiltFAR', 'CommFAR', 'FacilFAR', 'Lot', 'LotArea', 'LotDepth', 'NumBldgs', 'NumFloors', 'OfficeArea', 'ResArea', 'ResidFAR', 'RetailArea', 'YearBuilt', 'YearAlter1', 'ZipCode', 'YCoord', 'XCoord')

df_MN <- MN_18v1 %>% select('Address', 'BldgArea', 'BldgDepth', 'BuiltFAR', 'CommFAR', 'FacilFAR', 'Lot', 'LotArea', 'LotDepth', 'NumBldgs', 'NumFloors', 'OfficeArea', 'ResArea', 'ResidFAR', 'RetailArea', 'YearBuilt', 'YearAlter1', 'ZipCode', 'YCoord', 'XCoord')

df_QN <- QN_18v1 %>% select('Address', 'BldgArea', 'BldgDepth', 'BuiltFAR', 'CommFAR', 'FacilFAR', 'Lot', 'LotArea', 'LotDepth', 'NumBldgs', 'NumFloors', 'OfficeArea', 'ResArea', 'ResidFAR', 'RetailArea', 'YearBuilt', 'YearAlter1', 'ZipCode', 'YCoord', 'XCoord')

# new data frame with smaller features
dim(df_BK)

# Merge all data frames by rows
df_pluto = rbind(df_BK, df_BX, df_MN, df_QN)

identical(nrow(df_pluto), nrow(df_BK)+nrow(df_BX)+nrow(df_MN)+nrow(df_QN))




### Exploratory Analysis

# print the column names 
print(colnames(df_NYC))
print(colnames(df_pluto))

# merge complaint types which were renamed e.g. "HEAT/HOT WATER" to "HEATING" and "PAINT - PLASTER" to "PAINT/PLASTER"
df_NYC$complaint_type[df_NYC$complaint_type %in% "HEAT/HOT WATER"] <- "HEATING"
df_NYC$complaint_type[df_NYC$complaint_type %in% "PAINT - PLASTER"] <- "PAINT/PLASTER"

# remove NA entries
df_NYC %>% na.omit()





### Target defnition: Pluto dataset has all houses information for the given borrows. Some houses are register more complain more often. These particular houses have features that can help in predicting future complaints. 

df_target <- as.numeric(df_pluto$Address %in% df_NYC$incident_address)


df_pluto['target'] <- df_target

colnames(df_pluto)

# remove Address column
df_pluto <- df_pluto[-1]
colnames(df_pluto)

# to make sure every column has numeric/integer class
sapply(df_pluto, class)
```

### Pearson correlation matrix heatmap


# correlation matrix
cormat <- round(cor(df_pluto),2)
head(cormat)

# define a function that may help to remove the redundancy in the correlation matrix

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# use the above defined function to set redundant entries to NA
upper_tri <- get_upper_tri(cormat)
upper_tri


# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()







#Selecting correlated features. Here I set the threshold to be 0.12
cor_target = abs(cormat[,"target"])
cor_target<-cor_target[!is.na(cor_target)]
relevant_features = cor_target[cor_target>0.12]
print(relevant_features)


# let's find the features which are highly correlated among themselves. If so, we may use only one of them.
df_corr_2features <- df_pluto%>% select("ResidFAR","FacilFAR")
round(cor(df_corr_2features),2)

# This implies we can drop FacilFAR feature and keep ResiFAR feature as they are highly correlated. So the important features are 'BldgDepth', 'ResiFAR', 'NumFloors', 'ZipCode'. The above analysis gives a rough idea about the data and its general statistics. This type of exploratory analysis helps to better understand feature sets and decide which algorithm may better suit for our problem.


```

### Random forrest method for feature selection

# Here we will drop only three features ("target", "XCoord", "YCoord") and train the model on rest of the features
# data for features set and target

drops <- c("XCoord", "YCoord")
df_PLUTO <- df_pluto[ , !(names(df_pluto) %in% drops)]

df_PLUTO <- df_pluto[ , !(names(df_pluto) %in% drops)]

# Validation set will be 30% of pluto data
test_index <- createDataPartition(y = df_pluto$target, times = 1, p = 0.3, list = FALSE)

train_set <- df_PLUTO[-test_index,]
test_set <- df_PLUTO[test_index,]

# test dataset can be further modified by removing target column
test_set_CM <- test_set
test_set <- test_set %>% select(-target)


# In case of only 2 classifier, one can use linear regression, but here I am using Random Forest method beacause of its general applicability.

# convert target as factor
train_set$target <- as.character(train_set$target)
train_set$target <- as.factor(train_set$target)


set.seed(1)
model<- randomForest(target~.,train_set,ntree=tree_count,importance=TRUE,na.action=na.omit)



# convert all iterations into matrix form
imp_score_matrix <- importance(model)
imp_score_matrix


### The Ransom forest method provides a table of feature importance. It shows two varible 'MeanDecreaseAccuracy', 'MeanDecreaseGini'. Larger the numbers are, greater their feature importance is. A cursory look at the table reveals that features like 'Lot', 'BuiltFAR', 'BldgArea', 'ResArea', 'NumFloors' are most important one.




## Problem 4: Can a predictive model be built for a future prediction of the possibility of 
##complaints of the type that you have identified in response to question 1?


## So far, we have pointed out the important features in the pluto data set and did some exploratory analysis. Problem 4 poses a new set of challenge. It asks to predict the future. I don't know who can be well suited for the job 'Prophet', 'Philosopher', or 'Professor'. I beleive everyone will look for 'history' or in simple words time dependent feature sets.
##However our analysis shows that features are static in nature. To predict the HEATING complaints, we may need additional data from external sources that has some temporal dependencies e.g. weather dataset over years.
##With the given dataset, I think it would be good to try our hand over 'Time series analysis' and get a rough future estimate about number of complaints. 

### **Time Series Analysis**


colnames(df_NYC)
dt <- as.POSIXct(df_NYC$created_date)
df_NYC <- df_NYC %>% mutate(year_month = format(dt, "%Y-%m"))

rm(dt)

df_TS <- df_NYC %>% 
  select('year_month', 'unique_key')%>%                    
  filter(year_month<2019)
# below chunk of codes have not been varified so I have not put them in my Pdf file
df_TS %>%  sort(df_TS$year_month, decreasing = FALSE)

TS_complaint <- df_TS %>%
  na.omit() %>% # omit missing values
  #select(year, complaint_type) %>% # select columns we are interested in
  mutate(year_month = as.factor(year_month)) %>% # turn year in factors
  mutate(year_month = as.numeric(levels(year_month))[year_month]) %>%
  group_by(year, unique_key) %>% # group data by year and complaint_Type
  summarise(number = n())  # count


TS_complaint %>%
  ggplot(aes(x = year_month, y = number)) +
  geom_line() 

# Trend, Seasonality and error
decomposedRes <- decompose(tsData, type="mult") # use type = "additive" for additive components
plot (decomposedRes) # see plot below
stlRes <- stl(tsData, s.window = "periodic")













