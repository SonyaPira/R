#فراخوانی فایل  دیتا فریم با فرمت CSV
setwd("D:/Data Science/6-R/Project")
mysum<-read.csv("Samdata.csv",header=T) 
mysum

class(mysum)
#تعداد رکوردها و ستون ها
dim(mysum)

#نمایش نام ستونها 
names(mysum)

#نمایش 6رکورد اول
head(mysum)

#نمایش ساختار دیتاست (3136 obs of 22 variables)
str(mysum)

#نمایش اطلاعات آماری داده ها
summary(mysum)

#تغییر نام ستون Repair_Action_Desc
names(mysum)[16] <- "Action"
names(mysum)

#make dataframe
samdf <- data.frame(mysum)
head(samdf)


#جایگزین کردن ستون های خالی با NA
sum(is.na(samdf))
samdf[samdf==""]<-NA        # به جای سلول های خالی ، NA قرار بده
summary(samdf)

#نمایش تعداد مقادیر NA
colSums(is.na(samdf))

#بکاپ از دیتافریم
backsam <- samdf

#حذف رکورهایی که servic no is null 
samdf <- subset(samdf, !is.na(samdf$Serial_No))
samdf <- subset(samdf, !is.na(samdf$Product_Group))
str(samdf)
colSums(is.na(samdf))

#نمایش تعداد NA
MisProduct_Date <- sum(is.na(samdf$Product_Date))
MisProduct_Date

#نمایش درصد NA
percent_MisProduct_Date<- (MisProduct_Date / length(samdf$Product_Date)) * 100
percent_MisProduct_Date


# حذف فیلد در صورتی که بالای 50 درصد NA است
if (percent_MisProduct_Date > 50) 
  samdf <- samdf[, !colnames(samdf) %in% "Product_Date"] 
names(samdf)

#میانگین زمان تعمیر از زمان پذیرش تا اقدام به تعمیر
mntat1 <- mean(samdf$TAT01)
c("میانگین زمان تعمیر از زمان پذیرش تا اقدام به تعمیر:" , mntat1)

#میانگین زمان تعمیر از زمان پذیرش تا اتمام تعمیر
mntat2 <- mean(samdf$TAT02)
c("میانگین زمان تعمیر از زمان پذیرش تا اتمام تعمیر:", mntat2)


#نمایش تعداد داده های تکراری منظور تعداد محصولاتی که بیش از یک بار پذیرش شده اند
dupl1 <- samdf[duplicated(samdf$Serial_No),]
dupl1

dupl2 <- dupl1[duplicated(dupl1$Serial_No),]
dupl2

dupl3 <- dupl2[duplicated(dupl2$Serial_No),]
dupl3

#نرخ تکرار
rat_dupl <- (nrow(dupl1) + nrow(dupl2) + nrow(dupl3)) / nrow(samdf) * 100
rat_dupl


#نمایش وضعیت گارانتی بر روی بارپلات
barcost <- table(samdf$Cost_Type)
barplot(barcost[order(-barcost)], col = c("blue","green","red"), main="Frequency of Cost_Type")

#روش دوم نمایش وضعیت گارانتی بر روی بارپلات
library(ggplot2)
ggplot(samdf, aes(x = Cost_Type, fill = Cost_Type)) + geom_bar(width = 1) + xlab("نوع گارانتی") + ylab("تعداد") +
  ggtitle("Distribution of Cost Type")

#انکدینگ فیچرهای غیرعددی
c<- samdf
names(samdf)

col.names <- c('Cost_Type','Product_Group','City','Defect_Des','Symptom_Desc','Action','Labor_Charge_Desc','Engineer')
samdf[col.names] <- do.call(cbind.data.frame, lapply(samdf[col.names], as.factor))
samdf[col.names] <- do.call(cbind.data.frame, lapply(samdf[col.names], as.numeric))

str(samdf)
dim(backsam)


## تعیین ضریب همبستگی -----------

#حذف فیچرهای Serial_No,"Receipt_Date","Appoint_Date","Complete_Date"
samdf<-samdf[,-c(1,5,7:9)]
names(samdf)

#هیستورگرام فیچرها به غیر از  Serial_No,"Receipt_Date","Appoint_Date","Complete_Date"
par(mfrow = c(4, 4))  

for (i in 1:16) {
  hist(samdf[,i], xlab = "", main = paste( names(samdf)[i]))
}

#Scale داده ها
scl_samdf <- round(scale(samdf),3)


scl_samdf <- data.frame(scl_samdf)
View(scl_samdf)

#رسم هیستوگرام فیچرها بعد از هم مقیاس سازی
for (i in 1:16) {
  hist(scl_samdf[,i], xlab = "", main = paste( names(samdf)[i]))
}

par("mar")
par(mar=c(1,1,1,1))

# نمایش حالت عادی
par(mfrow = c(1,1))

# نمایش جدول میزان  همبستگی هریک از معیارها
cor_df <- round(cor(scl_samdf),2)
View(cor_df) 
names(samdf)

#رابطه خطی بین Cost_Type و Total_Amount_Invoice بر روی دیتا فریم پیش از هم مقیاس سازی
cor(samdf$Total_Invoice_Amount, samdf$Cost_Type , method = "pearson")
plot(samdf$Total_Invoice_Amount, samdf$Cost_Type) 

#رابطه خطی بین Cost_Type و Total_Amount_Invoice بر روی دیتا فریم پس از هم مقیاس سازی
cor(scl_samdf$Total_Invoice_Amount, scl_samdf$Cost_Type , method = "pearson")
plot(scl_samdf$Total_Invoice_Amount, scl_samdf$Cost_Type) 

# رابطه خطی بین Total_Invoice_Amount و Cost_Type وجود دارد با توجه به سه ستاره شدن فیچر و مقدار آر2 که کمتر از 0.05 است
m1 <- lm(Total_Invoice_Amount ~ Cost_Type, data = samdf)
summary(m1)

# رابطه خطی بین Total_Invoice_Amount و Cost_Type وجود دارد با توجه به سه ستاره شدن فیچر و مقدار آر2 که کمتر از 0.05 است
m2 <- lm(Total_Invoice_Amount ~ Cost_Type, data = scl_samdf) 
summary(m2)

m3 <- lm(Total_Invoice_Amount ~ ., data = scl_samdf)
summary(m3)


#VIF برای مشخص شدن چالش هم خطی
car :: vif(m3) 
# با استفاده از کوریلیشن و شاخص وی آی اف چالش همخطی بین فیچرهای Discount_Amount و Parts_Amount وجود دارد

#تبدیل فیچرهای عددی  Labor_Charge_Amount و Parts_Amount و Discount_Amount به یک فیلد
samdf$cal_amnt <- (samdf$Labor_Charge_Amount + samdf$Parts_Amount) - samdf$Discount_Amount

#Labor_Charge_Amount و Parts_Amount و Discount_Amount حذف فیچرهای
newdf <- samdf[,-c(13:15)]
names(newdf)
View(newdf)

#Scale داده ها
scl_newdf <- round(scale(newdf),3)
scl_newdf <- data.frame(scl_newdf)
View(scl_newdf)

m4 <- lm(Total_Invoice_Amount ~ ., data = scl_newdf)
summary(m4)

#VIF برای مشخص شدن چالش هم خطی
car :: vif(m4)

m5 <- lm(Total_Invoice_Amount ~ Cost_Type + Product_Group + City + TAT01 + Action + Labor_Charge_Desc + cal_amnt, data = scl_newdf)
summary(m5)

#قوانین انجمنی

install.packages("arules")
install.packages("tidyverse")
install.packages("Igraph")
install.packages("knitr")
library(arules)
library(tidyverse)
library(Matrix)
library(igraph) 
library(knitr)


# Assuming columns
samdf$Product_Group <- as.factor(samdf$Product_Group)
samdf$Defect_Des <- as.factor(samdf$Defect_Des)

# transactions
transactions <- as(samdf[c("Product_Group", "Defect_Des")], "transactions")

# قوانین انجمنی
rules <- apriori(transactions, parameter = list(support = 0.1, confidence = 0.7))

# چک کردن قوانین انجمنی
if (length(rules) > 0) {
  
  # فیلترینگ
  min_support <- 0.1
  min_confidence <- 0.7
  min_lift <- 1.0
  
  filtered_rules <- subset(rules, 
                           support >= min_support & 
                             confidence >= min_confidence & 
                             lift >= min_lift)
  
  if (length(filtered_rules) > 0) {
    
    # مرتب سازی
    
    sorted_rules <- sort(filtered_rules, by = "lift", decreasing = TRUE)
    
    #نمایش قوانین تاپ
    top_rules <- head(sorted_rules, 10)
    inspect(top_rules)
    
    #نمایش با فرمت جدول
    top_rules_df <- as(top_rules, "data.frame")
    kable(top_rules_df, format = "markdown")
    
    #نمایش در بار چارت
    bar_data <- data.frame(Metric = c("Support", "Confidence", "Lift"),
                           Value = c(top_rules@quality$support[1], 
                                     top_rules@quality$confidence[1], 
                                     top_rules@quality$lift[1]))
    
    ggplot(bar_data, aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Top Association Rule Metrics")
  } else {
    print("No association rules found after filtering.")
  }
} else {
  print("No association rules found.")
}


# تبدیل قوانین به دیتا فریم
rule_data <- as(top_rules, "data.frame")

# ایجاد گراف
rule_graph <- graph.data.frame(rule_data, directed = TRUE)


library(cluster)

df <- read.csv("Samdata.csv")

# انتخاب فیلدهای مربوطه
selected_cols <- c("Service_type", "Cost_Type", "TAT01", "Product_Group", "City")

selected_df <- df[, selected_cols]

# هندل کردن میس ها
selected_df[is.na(selected_df)] <- 0  

# استاندارد کردن فیلد عددی
numeric_cols <- c("TAT01")  
selected_df[numeric_cols] <- scale(selected_df[numeric_cols])

selected_df[is.na(selected_df)] <- 0  

numeric_cols <- c("TAT01")

# حذف میس ها 
selected_df <- selected_df[complete.cases(selected_df[numeric_cols]), ]

selected_df[numeric_cols] <- scale(selected_df[numeric_cols])

#بهینه سازی تعداد خوشه ها
set.seed(123)  
wcss <- vector()
for (i in 1:10) {
  tryCatch({
    # جدا کردن فیلدهای غیرعددی
    numeric_selected_df <- selected_df[, sapply(selected_df, is.numeric)]
    
    kmeans_model <- kmeans(numeric_selected_df, centers = i)
    wcss[i] <- kmeans_model$tot.withinss
  }, error = function(e) {
    # هندلینگ خطاها
    wcss[i] <- NA
  })
}

# پلات کردن
plot(1:10, wcss, type = "b", xlab = "Number of Clusters", ylab = "WCSS")

# انتخاب تعداد خوشه بهینه
k <- 3 

# اجرای الگوریتمkmeans
kmeans_model <- kmeans(numeric_selected_df, centers = k)

#لیبل خوشه ها
df$Cluster <- kmeans_model$cluster
df$Cluster <- kmeans_model$cluster

# رسم اسکتر پلات
ggplot(df, aes(x = TAT01, y = Cost_Type, color = factor(Cluster))) +
  geom_point() +
  labs(title = "K-means Clustering Results",
       x = "TAT01",
       y = "Cost_Type") +
  scale_color_discrete(name = "Cluster")

