
library(tidyverse)
library(data.table)
library(knitr)
library(rmarkdown)
library(rfm)
library(ggplot2)
library(eeptools)
library(ggthemes)
library(ggpubr)

#Data Load

library(readxl)
transactions <- read_excel("C:/Users/User/Desktop/KPMG/KPMG DATA.xlsx", 
                        sheet = "Transactions")
customer_demographic <- read_excel("C:/Users/User/Desktop/KPMG/KPMG DATA.xlsx", 
                           sheet = "Customer Demographic")

new_customer_list<-read_excel("C:/Users/User/Desktop/KPMG/KPMG DATA.xlsx", 
                              sheet = "New Customer List")


str(transactions)

#changing the data type

transactions$customer_id<-as.factor(transactions$customer_id)
transactions$online_order<-as.factor(transactions$online_order)
transactions$order_status<-as.factor(transactions$order_status)
transactions$brand<-as.factor(transactions$brand)
transactions$product_line<-as.factor(transactions$product_line)
transactions$product_class<-as.factor(transactions$product_class)
transactions$product_size<-as.factor(transactions$product_size)
transactions$transaction_date<-as.Date(transactions$transaction_date,'%m/%d/%Y')
transactions$product_first_sold_date<-as.Date(transactions$product_first_sold_date)
transactions$transaction_id<-as.factor(transactions$transaction_id)

#missing value checking
sapply(transactions,function(x)sum(is.na(x)))

glimpse(transactions)

str(customer_demographic)

#changing data type
customer_demographic$gender<-as.factor(customer_demographic$gender)
customer_demographic$DOB<-as.Date(customer_demographic$DOB)
customer_demographic$job_industry_category<-as.factor(customer_demographic$job_industry_category)
customer_demographic$wealth_segment<-as.factor(customer_demographic$wealth_segment)
customer_demographic$owns_car<-as.factor(customer_demographic$owns_car)
customer_demographic$state<-as.factor(customer_demographic$state)

#adding age column
customer_demographic$age<-as.integer(age_calc(customer_demographic$DOB,enddate=Sys.Date(),units='years'))

#missing value treatment
sapply(customer_demographic,function(x)sum(is.na(x)))


str(new_customer_list)
#changing the data type
new_customer_list$gender<-as.factor(new_customer_list$gender)
new_customer_list$DOB<-as.Date(new_customer_list$DOB)
new_customer_list$job_industry_category<-as.factor(new_customer_list$job_industry_category)
new_customer_list$wealth_segment<-as.factor(new_customer_list$wealth_segment)
new_customer_list$owns_car<-as.factor(new_customer_list$owns_car)
new_customer_list$state<-as.factor(new_customer_list$state)
new_customer_list$past_3_years_bike_related_purchases<-as.numeric(new_customer_list$past_3_years_bike_related_purchases)

#adding age column
new_customer_list$age<-(age_calc(new_customer_list$DOB,enddate=Sys.Date(),units='years'))

#missing value treatment
sapply(new_customer_list,function(x)sum(is.na(x)))
which(is.na(new_customer_list$DOB))
new_customer_list$DOB[is.na(new_customer_list$DOB)]<-mean(new_customer_list$DOB,na.rm=TRUE)



summary(transactions)
summary(customer_demographic)
summary(new_customer_list)

#visualization on=>
# Age
# Owning cars by State
# Wealth_segment
#bike related purchase based on gender
# Job industry Category

# Since we have large dataset and our insights will get derived from 
# customer details, rather than adding features in dataset we can 
# group them while doing the viz's

#Visualization:
############ Counting customers in age ranges

# first we make an age range in customer demographic and built viz on age range
customer_demographic$age_group<-cut(customer_demographic$age,breaks<-c(seq(17,88,by<-10),Inf))
customer_demographic$age_group<-as.factor(customer_demographic$age_group)

df<- customer_demographic %>%
      group_by(age_group) %>%
        summarise(counts=n())
head(df,8)
ggplot(df,aes(x=age_group,y=counts))+geom_bar(stat="identity",fill="tomato3",width=0.7)+theme(axis.text.x = element_text(hjust = 0.5))+
  geom_text(aes(label=counts),vjust=-0,colour="Dark Blue")+theme_pubclean()+labs(title = "Age Distributions of Old Customers",plot.title=element_text(hjust=0.9),
                                                                x="Age Range", y="Number of Customers",caption = "Source: Customer Demographic")

#age range in new customer list
new_customer_list$age_group<-cut(new_customer_list$age,breaks<-c(seq(17,88,by<-10),Inf))
new_customer_list$age_group<-as.factor(new_customer_list$age_group)

df1<- new_customer_list %>%
  group_by(age_group) %>%
  summarise(counts=n())
head(df1,7)
ggplot(df1,aes(x=age_group,y=counts))+geom_bar(stat="identity",fill="tomato3",width=0.7)+theme(axis.text.x = element_text(hjust = 0.5))+
  geom_text(aes(label=counts),vjust=-0,colour="Dark Blue")+theme_pubclean()+labs(title = "Age Distributions of New Customers",plot.title=element_text(hjust=0.9),
                                                                                 x="Age Range", y="Number of Customers",caption = "Source: New Customer List")
########### Wealth Segment of customers by Age category

#old

df2<-customer_demographic %>%
      group_by(age_group,wealth_segment) %>%
      summarise(counts=n())
head(df2,4)


ggplot(df2,aes(x=age_group,y=counts))+geom_bar(aes(fill=wealth_segment),stat="identity")+
  labs(title = "Wealth segment of old customers by age range",x="Age Range",y="Number of customers",caption = "Source: Customer Demographic")+
  scale_color_manual(values = c("#00AFBB", "#E7B900", "#FC4E08"))+
  scale_fill_manual(values = c("#00AFBB", "#E7B900", "#FC4E08"))+theme_tufte()

#new
df3<-new_customer_list %>%
  group_by(age_group,wealth_segment) %>%
  summarise(counts=n())
head(df3,4)


ggplot(df3,aes(x=age_group,y=counts))+geom_bar(aes(fill=wealth_segment),stat="identity")+
  labs(title = "Wealth segment of new customers by age range",x="Age Range",y="Number of customers",caption = "Source: New Customer List")+
  scale_color_manual(values = c("#00AFBB", "#E7B900", "#FC4E08"))+
  scale_fill_manual(values = c("#00AFBB", "#E7B900", "#FC4E08"))+theme_tufte()

############# Job related category
#old
table(customer_demographic$job_industry_category)

df4<- customer_demographic %>%
        group_by(job_industry_category) %>%
        summarise(counts=n()) %>%
        mutate(percentage=round(counts*100/sum(counts),1))
head(df4,10)


ggpie( df4,x="percentage", label = "percentage",lab.pos = "out", lab.font = list(color = "white"),fill = "job_industry_category", color = "white",palette = "jco")+
  theme(legend.position = "right")+labs(fill="Job Industry")+labs(title="Percentage of old customers in each job industry",caption ="Source: Customer Demographic" )

df5<- new_customer_list %>%
  group_by(job_industry_category) %>%
  summarise(counts=n()) %>%
  mutate(percentage=round(counts*100/sum(counts),1))
head(df5,10)

ggpie( df5,x="percentage", label = "percentage",lab.pos = "out", lab.font = list(color = "white"),fill = "job_industry_category", color = "white",palette = "jco")+
  theme(legend.position = "right")+labs(fill="Job Industry")+labs(title="Percentage of new customers in each job industry",caption ="Source: New Customer List" )

############# Owing Car By state

ggplot(customer_demographic,aes(state))+geom_bar(aes(fill=owns_car),position = "dodge")+
  scale_fill_manual(values =c("Red3","Blue1"))+theme_tufte()+labs(title = "How many of old customers own car?",x="State",y="Number of Customers",fill="Owns Car",caption = "Source: Customer Demographic")

ggplot(new_customer_list,aes(state))+geom_bar(aes(fill=owns_car),position = "dodge")+
  scale_fill_manual(values =c("Red3","Blue1"))+theme_tufte()+labs(title = "How many of new customers own car?",x="State",y="Number of Customers",fill="Owns Car",caption = "Source: New Customer List")

############ Past 3 year bike purchases group by gender

df6<-customer_demographic %>%
      group_by(gender)%>%
      summarise(total=sum(past_3_years_bike_related_purchases,na.rm = TRUE))%>%
      mutate(percentage=round(((total*100)/sum(total)),2))

head(df6,2)

ggplot(df6,aes(x=gender,y=total))+geom_bar(stat="identity",width = 0.5,fill="sky blue")+
  geom_text(aes(label=percentage),vjust=-0)+theme_tufte()+labs(title = "Total percentage of bike related purchases gender wise",x="Gender",y="Total Number of Bike Related Purchases",caption = "Source: Customer Demographic")



############ RFM analysis
#recency=> days since last purchase
#frequency=>total number of purchases
#monetary value=> total money the customer spent


attach(transactions)

tab1<-transactions %>%
  group_by(customer_id)%>%
  summarise(lastcustomeractivity=max(transaction_date))%>%
  mutate(last_transaction=max(lastcustomeractivity))
glimpse(tab1)

tab1$Recency<- round(as.numeric(difftime
                               (tab1$last_transaction, 
                                tab1$lastcustomeractivity , 
                                  units = c("days"))))
tab1
tab1<-tab1 %>% select(customer_id,Recency)
  


tab2<-transactions %>%
  group_by(customer_id)%>%
  summarise(Frequency=n())
glimpse(tab2)
summary(tab2)

tab3<-transactions %>%
  group_by(customer_id) %>%
  summarise(Monetary=sum(profit))
summary(tab3)

rfmtable<-merge(tab1,tab2)
rfmtable<-merge(rfmtable,tab3)

rfmtable
summary(rfmtable)

#scoring 
#R score

rfmtable$R_Score[rfmtable$Recency>88]<-1
rfmtable$R_Score[rfmtable$Recency>45 & rfmtable$Recency<=88]<-2
rfmtable$R_Score[rfmtable$Recency>18 & rfmtable$Recency<=45]<-3
rfmtable$R_Score[rfmtable$Recency<=18]<-4

#F score

rfmtable$F_Score[rfmtable$Frequency>7]<-4
rfmtable$F_Score[rfmtable$Frequency>5 & rfmtable$Frequency<=7]<-3
rfmtable$F_Score[rfmtable$Frequency>4 & rfmtable$Frequency<=5]<-2
rfmtable$F_Score[rfmtable$Frequency<=4]<-1

#M score

rfmtable$M_Score[rfmtable$Monetary<=1790.96]<-1
rfmtable$M_Score[rfmtable$Monetary>1790.96 & rfmtable$Monetary<=2816.25]<-2
rfmtable$M_Score[rfmtable$Monetary>2816.25 & rfmtable$Monetary<=4116.47]<-3
rfmtable$M_Score[rfmtable$Monetary>4116.2]<-4

#RFM Score

rfmtable<- rfmtable %>%
  mutate(RFM_Score=100*R_Score+10*F_Score+M_Score)

rfmtable

summary(rfmtable$RFM_Score)


rfmtable$segmentRFM<-NULL
champions <- c(444)
loyal_customers <- c(334, 342, 343, 344, 433, 434, 443)
potential_loyalist <- c(332,333,341,412,413,414,431,432,441,442,421,422,423,424)
recent_customers <- c(411)
promising <- c(311, 312, 313, 331)
needing_attention <- c(212,213,214,231,232,233,241,314,321,322,323,324)
about_to_sleep <- c(211)
at_risk <- c(112,113,114,131,132,133,142,124,123,122,121,224,223,222,221)
cant_lose <- c(134,143,144,234,242,243,244)
hibernating <- c(141)
lost <- c(111)

rfmtable$segmentRFM[which(rfmtable$RFM_Score %in% champions)] = "Champions"
rfmtable$segmentRFM[which(rfmtable$RFM_Score %in% loyal_customers)] = "Loyal Customers"
rfmtable$segmentRFM[which(rfmtable$RFM_Score %in% potential_loyalist)] = "Potential Loyalist"
rfmtable$segmentRFM[which(rfmtable$RFM_Score %in% recent_customers)] = "Recent customers"
rfmtable$segmentRFM[which(rfmtable$RFM_Score %in% promising)] = "Promising"
rfmtable$segmentRFM[which(rfmtable$RFM_Score %in% needing_attention)] = "Customer Needing Attention"
rfmtable$segmentRFM[which(rfmtable$RFM_Score %in% about_to_sleep)] = "About to Sleep"
rfmtable$segmentRFM[which(rfmtable$RFM_Score %in% at_risk)] = "At Risk"
rfmtable$segmentRFM[which(rfmtable$RFM_Score %in% cant_lose)] = "Can't Lose Them"
rfmtable$segmentRFM[which(rfmtable$RFM_Score %in% hibernating)] = "Hibernating"
rfmtable$segmentRFM[which(rfmtable$RFM_Score %in% lost)] = "Lost"

rfmtable$segmentRFM
view(rfmtable)
write_excel_csv(rfmtable,"KPMG_Segment.csv")

rfm_seg<-rfmtable %>%
  count(segmentRFM) %>%
  arrange(desc(n)) %>%
  rename(Segments= segmentRFM, TotalCustomers=n)
rfm_seg
View(rfm_seg)


rec<-ggplot(rfmtable,aes(segmentRFM))+geom_bar(aes(fill=Recency),width = 0.4,fill="#E7B800")+coord_flip()+
  theme_cleveland()+labs(title = "Customer Segments based on Recency Score",y="Total number of customers",x="Customer Segments")

freq<-ggplot(rfmtable,aes(segmentRFM))+geom_bar(aes(fill=Frequency),width = 0.4,fill="#FC4E07")+coord_flip()+
  theme_cleveland()+labs(title = "Customer Segments based on Frequency Score",y="Total number of customers",x="Customer Segments")

mone<-ggplot(rfmtable,aes(segmentRFM))+geom_bar(aes(fill=Monetary),width = 0.4,fill="#00AFBB")+coord_flip()+
  theme_cleveland()+labs(title = "Customer Segments based on Monetary Score",y="Total number of customers",x="Customer Segments")

figure<-ggarrange(rec,freq,mone,ncol = 1,nrow = 3)
figure
ggexport(figure,filename = "segments.png")

ggplot(rfmtable,aes(x=segmentRFM))+geom_bar(aes(fill=segmentRFM))+coord_flip()+
  scale_fill_brewer(palette = "Spectral")+
  labs(title = "Customer Segments",y="Total Number of Customers",
                                                             fill="Segments")+theme_cleveland()

library(RColorBrewer)
display.brewer.all()

ggplot(rfmtable,aes(y=Monetary,x=Recency))+geom_jitter(color="#E7B800")+
  labs(title = "Recency against Monetary",y="Monetary($)")+theme_tufte()

ggplot(rfmtable,aes(y=Frequency,x=Recency))+geom_point(color="#FC4E07")+
  labs(title = "Recency against Frequency",y="Frequency")+theme_tufte()

ggplot(rfmtable,aes(x=Frequency,y=Monetary))+geom_point(color="#00AFBB")+
  labs(title = "Monetary against Frequency")+theme_tufte()

set.seed(555)
clusters<-kmeans(scale(rfmtable[,2:4]),3,nstart = 1)
rfmtable$clusters<-as.factor(clusters$cluster)
head(rfmtable)
