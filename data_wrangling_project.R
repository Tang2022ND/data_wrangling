library(dplyr)
library(ggplot2)
library(radiant)
library(maps)
library(ggmap)

housing <- read.delim("California_Houses.csv",header = T, sep = ",")
housing02 <- read.delim("housing.csv",header = T, sep = ",")




housing <- housing %>% mutate(
  room_per_household = Tot_Rooms/Households
)
housing$Ocean_Proxy <- as.factor(housing02$ocean_proximity)
save(housing, file = "housing.rda")
load("housing.rda")
hq <- quantile(housing$Median_House_Value)
price <- housing$Median_House_Value
housing$Price_Category <- ifelse(price <= hq[2] , "very_low", 
                                 ifelse(price > hq[2] & price <= hq[3], "low", 
                                        ifelse(price > hq[3] & price <= hq[4], "average", 
                                               ifelse(price > hq[4] & price <= 400000, "high", "very_high"))))

housing$Median_Income <- housing$Median_Income * 10000
housing$affordability <- housing$Median_House_Value/housing$Median_Income




register_google("AIzaSyAVJQUd1mGICFac5zKE_r055727U5MJO9s")
CAmap <- get_map(location ='california',zoom = 6, maptype = "roadmap", color = "bw")
LAmap <- get_map(location ='Los Angles',zoom = 10, maptype = "roadmap", color = "bw")
SFmap <- get_map(location ='San Francisco',zoom = 10, maptype = "roadmap", color = "bw")
SJmap <- get_map(location ='San Jose',zoom = 10, maptype = "roadmap", color = "bw")


#Median House Value Map
CA_housing_price <- ggmap(CAmap, base_layer = ggplot(aes(x = Longitude, y= Latitude), data = housing)) +
  geom_point(data = housing, aes(color = Median_House_Value, alpha = 0.75))
ggplotly(CA_housing_price) #CA

LA_housing_price <- ggmap(LAmap, base_layer = ggplot(aes(x = Longitude, y= Latitude), data = housing)) +
  geom_point(data = housing, aes(color = Median_House_Value, alpha = 0.75))
ggplotly(LA_housing_price) #LA 

SF_housing_price <- ggmap(SFmap, base_layer = ggplot(aes(x = Longitude, y= Latitude), data = housing)) +
  geom_point(data = housing, aes(color = Median_House_Value, alpha = 0.75))
ggplotly(SF_housing_price) #SF

SJ_housing_price <- ggmap(SJmap, base_layer = ggplot(aes(x = Longitude, y= Latitude), data = housing)) +
  geom_point(data = housing, aes(color = Median_House_Value, alpha = 0.75))
ggplotly(SJ_housing_price) #SJ






#Affordability Map
housing_afford <- housing %>% filter(affordability <= 12)
CA_affordability <- ggmap(CAmap, base_layer = ggplot(aes(x = Longitude, y= Latitude), data = housing_afford)) +
  geom_point(data = housing_afford, aes(color = affordability, alpha = 0.75)) +
  scale_color_gradient2(low = "green", high = "black", mid = "blue", midpoint = 6.9)
ggplotly(CA_affordability) #CA

LA_affordability <- ggmap(LAmap, base_layer = ggplot(aes(x = Longitude, y= Latitude), data = housing_afford)) +
  geom_point(data = housing_afford, aes(color = affordability, alpha = 0.75))  +
  scale_color_gradient2(low = "green", high = "black", mid = "blue", midpoint = 6.9)
ggplotly(LA_affordability) #LA 

SF_affordability <- ggmap(SFmap, base_layer = ggplot(aes(x = Longitude, y= Latitude), data = housing_afford)) +
  geom_point(data = housing_afford, aes(color = affordability, alpha = 0.75)) +
  scale_color_gradient2(low = "green", high = "black", mid = "blue", midpoint = 6.9)
ggplotly(SF_affordability) #SF

SJ_affordability <- ggmap(SJmap, base_layer = ggplot(aes(x = Longitude, y= Latitude), data = housing_afford)) +
  geom_point(data = housing_afford, aes(color = affordability, alpha = 0.75)) +
  scale_color_gradient2(low = "green", high = "black", mid = "blue", midpoint = 6.9)
ggplotly(SJ_affordability) #SJ



#Comfort Map
housing_afford$comfort <- sqrt(housing_afford$room_per_household)/sqrt(housing_afford$affordability)
housing_comfort <- housing_afford %>% filter(comfort <= 2)


CA_comfort <- ggmap(CAmap, base_layer = ggplot(aes(x = Longitude, y= Latitude), data = housing_afford)) +
  geom_point(data = housing_comfort, aes(color = comfort, alpha = 0.75)) +
  scale_color_gradient2(low = "red", high = "green", mid = "blue", midpoint = 1.2)
ggplotly(CA_comfort) #CA

LA_comfort <- ggmap(LAmap, base_layer = ggplot(aes(x = Longitude, y= Latitude), data = housing_afford)) +
  geom_point(data = housing_comfort, aes(color = comfort, alpha = 0.75))  +
  scale_color_gradient2(low = "red", high = "green", mid = "blue", midpoint = 1.2)
ggplotly(LA_comfort) #LA 

SF_comfort <- ggmap(SFmap, base_layer = ggplot(aes(x = Longitude, y= Latitude), data = housing_afford)) +
  geom_point(data = housing_comfort, aes(color = comfort, alpha = 0.75)) +
  scale_color_gradient2(low = "red", high = "green", mid = "blue", midpoint = 1.2)
ggplotly(SF_comfort) #SF

SJ_comfort <- ggmap(SJmap, base_layer = ggplot(aes(x = Longitude, y= Latitude), data = housing_afford)) +
  geom_point(data = housing_comfort, aes(color = comfort, alpha = 0.75)) +
  scale_color_gradient2(low = "red", high = "green", mid = "blue", midpoint = 1.2)
ggplotly(SJ_comfort) #SJ





  



































housing_model <- housing[c(1001:10000, 11001:19000),]
housing_test <- housing[-c(1001:10000, 11001:19000),]

cor_test <- correlation(
  housing_model,
  var = c(
    "Median_Income"         
    ,"Median_Age","Tot_Rooms"               
    , "Tot_Bedrooms","Population"              
    , "Households","Latitude"                
    , "Longitude","Distance_to_coast"       
    , "Distance_to_LA","Distance_to_SanDiego"    
    , "Distance_to_SanJose","Distance_to_SanFrancisco"
    ,"room_per_household","Median_House_Value"    
  ), 
  
)
summary(cor_test)


result <- regress(
  housing_model, 
  rvar = "Median_House_Value", 
  evar = c(
    "Median_Income"           
    ,"Median_Age","Tot_Rooms"               
    , "Tot_Bedrooms","Population"              
    , "Households","Distance_to_coast"       
  ), 
 
)
summary(result, sum_check = "vif")
pred <- predict(result, pred_data = housing_test)
print(pred, n = 10)
housing_result <- cbind(pred, actual_value =housing_test$Median_House_Value)
housing_result$test <- ifelse(housing_result$actual_value >= housing_result$`2.5%` & 
                                housing_result$actual_value <= housing_result$`97.5%`, "T", "F")

length(housing_result$test[housing_result$test == "T"])/length(housing_result$test)
