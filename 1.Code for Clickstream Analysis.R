ep <- read.csv("/Users/caesarchan/Desktop/MSBA/Winter/BANA 277 Web and Social Analytics/Expedia Project/expedia.csv")

head(ep)

str(ep)

colSums(is.na(ep))

library("sqldf")

#Data Summary 

#consumer_id
num_consumer <- sqldf("select count(distinct(consumer_id)) from ep")
num_consumer

#search_id
num_search <- sqldf("select consumer_id,count(consumer_id) from ep group by consumer_id")
head(num_search)
names(num_search)[2] = "search_cnt"
data_summary <- function(x){c(mean(x),sd(x),median(x),min(x),max(x))}
data_summary(num_search$search_cnt)
boxplot(num_search$search_cnt, ylim = c(0,300))
hist(num_search$search_cnt)

#trip_roomN
roomN <- sqldf("select consumer_id,trip_roomN from ep group by consumer_id")
head(roomN)
data_summary(roomN$trip_roomN)
h1 = hist(roomN$trip_roomN)
h1$density = h1$counts/sum(h1$counts)
plot(h1, freq = FALSE, ylim = c(0,1))

#trip_peopleN
peopleN <- sqldf("select consumer_id,trip_peopleN from ep group by consumer_id")
head(peopleN)
data_summary(peopleN$trip_peopleN)
h2 = hist(peopleN$trip_peopleN)
h2$density = h2$counts/sum(h2$counts)
plot(h2, freq = FALSE, ylim = c(0,1))

#search_minstar
search_star <- sqldf("select consumer_id,search_minstar from ep group by consumer_id")
head(search_star)
data_summary(search_star$search_minstar)
h3 = hist(search_star$search_minstar,ylim = c(0,200))
h3$density = h3$counts/sum(h3$counts)
plot(h3, freq = FALSE, ylim = c(0,1))
#Zeros mostly, remove.

#webpage_number,too many na, remove.

#click_datetime
class(ep$click_datetime)
ep$click_datetime <- as.numeric(ep$click_datetime)
is.na(ep$click_datetime) <- ep$click_datetime == ""
ep$click_datetime[ep$click_datetime > 0] <- 1
ep$click_datetime[is.na(ep$click_datetime)] <- 0
head(ep$click_datetime)
head(ep)
click <- sqldf("select consumer_id,sum(click_datetime) from ep group by consumer_id")
head(click)
names(click)[2] = "click_cnt"
data_summary(click$click_cnt)
h4 = hist(click$click_cnt)
h4$density = h4$counts/sum(h4$counts)
plot(h4, freq = FALSE, ylim = c(0,1))

#hotel_id
num_hotel <- sqldf("select count(distinct(hotel_id)) from ep")
num_hotel
hotel <- sqldf("select hotel_id,count(hotel_id) from ep group by hotel_id")
head(hotel)
str(hotel)
names(hotel)[2] = "hotel_cnt"
data_summary(hotel$hotel_cnt)
h5 = hist(hotel$hotel_cnt)
boxplot(hotel$hotel_cnt,ylim = c(0,300))

#webpage_position
data_summary(ep$webpage_position)
hist(ep$webpage_position)

#hotel_star
hotel_star <- sqldf("select hotel_id,hotel_star from ep group by hotel_id")
head(hotel_star)
class(hotel_star$hotel_star)
h6 = hist(hotel_star$hotel_star)

#hotel_price
hotel_price <- sqldf("select hotel_id,hotel_price from ep group by hotel_id")
head(hotel_price)
h7 = hist(hotel_price$hotel_price,breaks = 80,xlim = c(0,1000))
h7$density = h7$counts/sum(h7$counts)
plot(h7, freq = FALSE, xlim = c(0,1000),ylim = c(0,0.3))

#hotel_discounted
hotel_discount <- sqldf("select hotel_id,hotel_discounted from ep group by hotel_id")
head(hotel_discount)
hotel_discount$hotel_discounted <- as.numeric(hotel_discount$hotel_discounted)
hotel_discount$hotel_discounted[hotel_discount$hotel_discounted == 3] <- 1
hotel_discount$hotel_discounted[hotel_discount$hotel_discounted == 2] <- 0
n1 <- sqldf("select count(hotel_id) from hotel_discount where hotel_discounted==1")
n1
n2 <- sqldf("select count(hotel_id) from hotel_discount where hotel_discounted==0")
n2
barplot(c(483,1969))

#trip_begin,trip_end,search_datetime -> trip_month,trip_length,trip_plan
class(ep$trip_begin)
ep$trip_begin <- as.Date(ep$trip_begin,"%d-%B-%y")
head(ep$trip_begin)
ep$trip_end <- as.Date(ep$trip_end,"%d-%B-%y")
head(ep$trip_end)
ep$search_datetime <- as.Date(ep$search_datetime,"%d-%B-%y")
head(ep$search_datetime)

ep$trip_month = format(ep$trip_begin,"%m")
head(ep)

trip_m <- sqldf("select consumer_id,trip_month from ep group by consumer_id")
head(trip_m)
colSums(is.na(trip_m))
trip_m$trip_month = as.numeric(trip_m$trip_month)
h8 = hist(trip_m$trip_month)

ep$trip_length <- ep$trip_end - ep$trip_begin
head(ep$trip_length)
ep$trip_length = as.numeric(ep$trip_length)
trip_l <- sqldf("select consumer_id,trip_length from ep group by consumer_id")
head(trip_l)
colSums(is.na(trip_l))
boxplot(trip_l$trip_length,ylim = c(0,10))
h9 = hist(trip_l$trip_length,breaks = 80,xlim = c(0,15))
h9$density = h9$counts/sum(h8$counts)
plot(h7, freq = FALSE, xlim = c(0,15),ylim = c(0,0.3))

ep$trip_plan <- ep$trip_begin - ep$search_datetime
head(ep$trip_plan)
ep$trip_plan = as.numeric(ep$trip_plan)
trip_p <- sqldf("select consumer_id,trip_plan from ep group by consumer_id")
head(trip_p)
colSums(is.na(trip_p))
boxplot(trip_p$trip_plan,ylim = c(0,200))
h10 = hist(trip_p$trip_plan,breaks = 80,xlim = c(0,200))
h10$density = h10$counts/sum(h10$counts)
plot(h8, freq = FALSE, xlim = c(0,200),ylim = c(0,0.3))

#click_for_details,click_for_purchase -> click_d_rate,click_p_rate
search_click <- sqldf("select consumer_id,count(search_id),sum(click_for_details),sum(click_for_purchase) from ep group by consumer_id")
head(search_click)
search_click$click_d_rate = search_click$`sum(click_for_details)`/search_click$`count(search_id)`
search_click$click_p_rate = search_click$`sum(click_for_purchase)`/search_click$`sum(click_for_details)`
search_click$click_p_rate[is.nan(search_click$click_p_rate)] <- 0
head(search_click)
boxplot(search_click$click_d_rate,ylim = c(0,0.2))
h11 = hist(search_click$click_d_rate,breaks = 50,xlim = c(0,0.2))
h11$density = h9$counts/sum(h9$counts)
plot(h11, freq = FALSE, xlim = c(0,0.1),ylim = c(0,1))
boxplot(search_click$click_p_rate,ylim = c(0,0.1))
h12 = hist(search_click$click_p_rate,breaks = 1000,xlim = c(0,0.01))
#Too few purchases,the variable of click_p_rate is meaningless.
num_purchase <- sqldf("select sum(click_for_purchase) from ep")
num_purchase

#sort_by_distance,sort_by_star,sort_by_price -> sort_d_rate,sort_s_rate,sort_p_rate
search_sort <- sqldf("select consumer_id,count(consumer_id),sum(sort_by_distance),sum(sort_by_star),sum(sort_by_price) from ep group by consumer_id")
head(search_sort)
search_sort$sort_d_rate = search_sort$`sum(sort_by_distance)`/search_sort$`count(consumer_id)`
search_sort$sort_s_rate = search_sort$`sum(sort_by_star)`/search_sort$`count(consumer_id)`
search_sort$sort_p_rate = search_sort$`sum(sort_by_price)`/search_sort$`count(consumer_id)`
head(search_sort)
boxplot(search_sort$sort_d_rate)
h13 = hist(search_sort$sort_d_rate,breaks = 20,ylim = c(0,100))
boxplot(search_sort$sort_s_rate)
h14 = hist(search_sort$sort_s_rate,breaks = 20,ylim = c(0,100))
boxplot(search_sort$sort_p_rate)
h15 = hist(search_sort$sort_p_rate)


#Data Exploration

#Define 7 objective metrics.
#Hotel:1.click_for_purchase 2.click_datetime 5.click_for_details
#Consumer:3.click_cnt 4.click_rate 6.click_details_rate 7.num_search

#Correlation identification
data_hotel <- data.frame(ep$click_for_purchase,ep$click_datetime,ep$click_for_details,ep$webpage_position,ep$hotel_star,ep$hotel_price,ep$hotel_discounted,ep$sort_by_distance,ep$sort_by_star,ep$sort_by_price)

cor(data_hotel)

data_consumer <- data.frame(ep$click_cnt,ep$click_rate,ep$click_details_rate,ep$num_search,ep$trip_roomN,ep$trip_peopleN,ep$trip_length,ep$trip_plan,ep$sort_d_rate,ep$sort_s_rate,ep$sort_p_rate) 

cor(data_consumer)

#1.click_for_purchase - hotel

boxplot(webpage_position ~ click_for_purchase, data = ep, ylim = c(0,25))
t.test(ep$webpage_position ~ ep$click_for_purchase)

boxplot(hotel_star ~ click_for_purchase, data = ep)
t.test(ep$hotel_star ~ ep$click_for_purchase)

boxplot(hotel_price ~ click_for_purchase, data = ep,ylim = c(0,800))
t.test(ep$hotel_price ~ ep$click_for_purchase)

class(ep$hotel_discounted)
ep$hotel_discounted <- as.numeric(ep$hotel_discounted)
head(ep$hotel_discounted)
ep$hotel_discounted[ep$hotel_discounted == 3] <- 1
ep$hotel_discounted[ep$hotel_discounted == 2] <- 0
head(ep$hotel_discounted)
t.test(ep$hotel_discounted ~ ep$click_for_purchase)

t.test(ep$sort_by_distance ~ ep$click_for_purchase)

t.test(ep$sort_by_star ~ ep$click_for_purchase)

t.test(ep$sort_by_price ~ ep$click_for_purchase)
#There's no model because the purchase records are too few.

#2.click_datetime - hotel

boxplot(webpage_position ~ click_datetime, data = ep, ylim = c(0,25))
t.test(ep$webpage_position ~ ep$click_datetime)

boxplot(hotel_star ~ click_datetime, data = ep)
t.test(ep$hotel_star ~ ep$click_datetime)

boxplot(hotel_price ~ click_datetime, data = ep, ylim = c(0,1000))
t.test(ep$hotel_price ~ ep$click_datetime)

t.test(ep$hotel_discounted ~ ep$click_datetime)
t.test(ep$sort_by_distance ~ ep$click_datetime)
t.test(ep$sort_by_star ~ ep$click_datetime)
t.test(ep$sort_by_price ~ ep$click_datetime)

ep$city1[is.na(ep$city1)] <- 0
ep$city2[is.na(ep$city2)] <- 0
ep$city3[is.na(ep$city3)] <- 0
ep$city4[is.na(ep$city4)] <- 0
t.test(ep$city1 ~ ep$click_datetime)
t.test(ep$city2 ~ ep$click_datetime)
t.test(ep$city3 ~ ep$click_datetime)
t.test(ep$city4 ~ ep$click_datetime)

reg1 <- glm(click_datetime ~ webpage_position + hotel_star + hotel_price + hotel_discounted + sort_by_distance + sort_by_star + sort_by_price + city1 + city2 + city3, family = binomial(), data = ep)
summary(reg1)

exp(reg1$coefficients) - 1

#3.click_cnt - consumer

head(click)
ep <- left_join(ep,click,by = c("consumer_id" = "consumer_id"))
head(ep)
head(search_sort)
ep <- left_join(ep,search_sort,by = c("consumer_id" = "consumer_id"))
head(ep)

plot(click_cnt ~ trip_roomN, data = ep)
t.test(ep$trip_roomN,ep$click_cnt)

plot(click_cnt ~ trip_peopleN, data = ep)
t.test(ep$trip_peopleN,ep$click_cnt)

plot(click_cnt ~ trip_length, data = ep)
t.test(ep$trip_length,ep$click_cnt)

plot(click_cnt ~ trip_plan, data = ep)
t.test(ep$trip_plan,ep$click_cnt)

plot(click_cnt ~ sort_d_rate, data = ep)
t.test(ep$sort_d_rate,ep$click_cnt)

plot(click_cnt ~ sort_s_rate, data = ep)
t.test(ep$sort_s_rate,ep$click_cnt)

plot(click_cnt ~ sort_p_rate, data = ep)
t.test(ep$sort_p_rate,ep$click_cnt)

ep <- left_join(ep,num_search,by = c("consumer_id" = "consumer_id"))
head(ep)
names(ep)[36] = "num_search"
plot(click_cnt ~ num_search, data = ep)
t.test(ep$num_search,ep$click_cnt)

reg2 <- glm(click_cnt ~ trip_roomN + trip_peopleN + trip_length + trip_plan + sort_d_rate + sort_s_rate + sort_p_rate + num_search, family = poisson(), data = ep)
summary(reg2)

exp(reg2$coefficients) - 1

#4.click_rate - consumer

ep$click_rate <- ep$click_datetime/ep$num_search
head(ep)
data_summary(ep$click_rate)
hist(ep$click_rate,breaks = 10000,xlim = c(0,0.001),ylim = c(0,20))
boxplot(ep$click_rate,ylim = c(0,0.00001))

plot(click_rate ~ trip_roomN, data = ep)
t.test(ep$trip_roomN,ep$click_rate)

plot(click_rate ~ trip_peopleN, data = ep)
t.test(ep$trip_peopleN,ep$click_rate)

plot(click_rate ~ trip_length, data = ep)
t.test(ep$trip_length,ep$click_rate)

plot(click_rate ~ trip_plan, data = ep)
t.test(ep$trip_plan,ep$click_rate)

plot(click_rate ~ sort_d_rate, data = ep)
t.test(ep$sort_d_rate,ep$click_rate)

plot(click_rate ~ sort_s_rate, data = ep)
t.test(ep$sort_s_rate,ep$click_rate)

plot(click_rate ~ sort_p_rate, data = ep)
t.test(ep$sort_p_rate,ep$click_rate)

plot(click_rate ~ num_search, data = ep)
t.test(ep$num_search,ep$click_rate)

reg3 <- glm(click_rate ~ trip_roomN + trip_peopleN + trip_length + trip_plan + sort_d_rate + sort_s_rate + sort_p_rate + num_search, data = ep)
summary(reg3)

#Most variables of this model are statistically insignificant,
#so we categorized the click_rate by a certain level and built another model.
ep$click_rate_good <- ifelse(ep$click_rate>=0.0005,1,0)
head(ep$click_rate_good)
summary(ep$click_rate_good)
t.test(ep$trip_roomN ~ ep$click_rate_good)
t.test(ep$trip_peopleN ~ ep$click_rate_good)
t.test(ep$trip_length ~ ep$click_rate_good)
t.test(ep$trip_plan ~ ep$click_rate_good)
t.test(ep$sort_d_rate ~ ep$click_rate_good)
t.test(ep$sort_s_rate ~ ep$click_rate_good)
t.test(ep$sort_p_rate ~ ep$click_rate_good)
t.test(ep$num_search ~ ep$click_rate_good)

reg4 <- glm(click_rate_good ~ trip_peopleN + trip_plan + sort_d_rate + sort_p_rate + num_search, family = binomial(),data = ep)
summary(reg4)

exp(reg4$coefficients) - 1

#5.click_for_details - hotel

boxplot(webpage_position ~ click_for_details, data = ep, ylim = c(0,25))
t.test(ep$webpage_position ~ ep$click_for_details)

boxplot(hotel_star ~ click_for_details, data = ep)
t.test(ep$hotel_star ~ ep$click_for_details)

boxplot(hotel_price ~ click_for_details, data = ep, ylim = c(0,1000))
t.test(ep$hotel_price ~ ep$click_for_details)

t.test(ep$hotel_discounted ~ ep$click_for_details)
t.test(ep$sort_by_distance ~ ep$click_for_details)
t.test(ep$sort_by_star ~ ep$click_for_details)
t.test(ep$sort_by_price ~ ep$click_for_details)
t.test(ep$city1 ~ ep$click_for_details)
t.test(ep$city2 ~ ep$click_for_details)
t.test(ep$city3 ~ ep$click_for_details)
t.test(ep$city4 ~ ep$click_for_details)

reg5 <- glm(click_for_details ~ webpage_position + hotel_star + hotel_price + hotel_discounted + sort_by_distance + sort_by_star + sort_by_price + city1 + city2 + city3 + city4, family = binomial(), data = ep)
summary(reg5)

exp(reg5$coefficients) - 1

#6.click_details_rate - consumer

ep$click_details_rate <- ep$click_for_details/ep$num_search
head(ep)
data_summary(ep$click_details_rate)
hist(ep$click_details_rate)

plot(click_details_rate ~ trip_roomN, data = ep)
t.test(ep$trip_roomN,ep$click_details_rate)

plot(click_details_rate ~ trip_peopleN, data = ep)
t.test(ep$trip_peopleN,ep$click_details_rate)

plot(click_details_rate ~ trip_length, data = ep)
t.test(ep$trip_length,ep$click_details_rate)

plot(click_details_rate ~ trip_plan, data = ep)
t.test(ep$trip_plan,ep$click_details_rate)

plot(click_details_rate ~ sort_d_rate, data = ep)
t.test(ep$sort_d_rate,ep$click_details_rate)

plot(click_details_rate ~ sort_s_rate, data = ep)
t.test(ep$sort_s_rate,ep$click_details_rate)

plot(click_details_rate ~ sort_p_rate, data = ep)
t.test(ep$sort_p_rate,ep$click_details_rate)

plot(click_details_rate ~ num_search, data = ep)
t.test(ep$num_search,ep$click_details_rate)

reg6 <- glm(click_details_rate ~ trip_roomN + trip_peopleN + trip_length + trip_plan + sort_d_rate + sort_s_rate + sort_p_rate + num_search, data = ep)
summary(reg6)

#Most variables of this model are statistically insignificant,
#so we categorized the click_details_rate by a certain level and built another model.
ep$click_details_rate_good <- ifelse(ep$click_details_rate>=0.0003,1,0)
head(ep)
summary(ep$click_details_rate_good)

t.test(ep$trip_roomN ~ ep$click_details_rate_good)
t.test(ep$trip_peopleN ~ ep$click_details_rate_good)
t.test(ep$trip_length ~ ep$click_details_rate_good)
t.test(ep$trip_plan ~ ep$click_details_rate_good)
t.test(ep$sort_d_rate ~ ep$click_details_rate_good)
t.test(ep$sort_s_rate ~ ep$click_details_rate_good)
t.test(ep$sort_p_rate ~ ep$click_details_rate_good)
t.test(ep$num_search ~ ep$click_details_rate_good)

reg7 <- glm(click_details_rate_good ~ trip_peopleN + trip_plan + sort_d_rate + sort_p_rate + num_search, family = binomial(),data = ep)
summary(reg7)

exp(reg7$coefficients) - 1

#7.num_search - consumer

plot(num_search ~ trip_roomN, data = ep)
t.test(ep$trip_roomN,ep$num_search)

plot(num_search ~ trip_peopleN, data = ep)
t.test(ep$trip_peopleN,ep$num_search)

plot(num_search ~ trip_length, data = ep)
t.test(ep$trip_length,ep$num_search)

plot(num_search ~ trip_plan, data = ep)
t.test(ep$trip_plan,ep$num_search)

plot(num_search ~ sort_d_rate, data = ep)
t.test(ep$sort_d_rate,ep$num_search)

plot(num_search ~ sort_s_rate, data = ep)
t.test(ep$sort_s_rate,ep$num_search)

plot(num_search ~ sort_p_rate, data = ep)
t.test(ep$sort_p_rate,ep$num_search)

reg8 <- glm(num_search ~ trip_roomN + trip_peopleN + trip_length + trip_plan + sort_d_rate + sort_s_rate + sort_p_rate, family = poisson(), data = ep)
summary(reg8)

exp(reg8$coefficients) - 1


t.test(ep$trip_roomN,ep$trip_plan)
t.test(ep$trip_peopleN,ep$trip_plan)
t.test(ep$trip_length,ep$trip_plan)
t.test(ep$sort_d_rate,ep$trip_plan)
t.test(ep$sort_s_rate,ep$trip_plan)
t.test(ep$sort_p_rate,ep$trip_plan)

reg9 <- lm(trip_plan ~ trip_roomN + trip_peopleN + trip_length + sort_d_rate + sort_s_rate + sort_p_rate, data = ep)
summary(reg9)

t.test(ep$hotel_star,ep$webpage_position)
t.test(ep$hotel_price,ep$webpage_position)
t.test(ep$hotel_discounted,ep$webpage_position)
t.test(ep$sort_by_distance,ep$webpage_position)
t.test(ep$sort_by_star,ep$webpage_position)
t.test(ep$sort_by_price,ep$webpage_position)

reg10 <- lm(webpage_position ~ hotel_star + hotel_price + hotel_discounted + sort_by_distance +sort_by_star + sort_by_price, data = ep)
summary(reg10)

boxplot(ep$hotel_star ~ ep$webpage_position,xlim = c(0,25))
boxplot(ep$hotel_price ~ ep$webpage_position,xlim = c(0,25),ylim = c(0,400))
boxplot(ep$hotel_price ~ ep$webpage_position,xlim = c(0,25),ylim = c(50,250))
