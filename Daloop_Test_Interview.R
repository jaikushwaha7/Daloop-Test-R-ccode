distr_table <- data.frame(
  X = rep(c(0, 1), each = 2),
  Y = rep(c(1, 2), times = 2),
  pr = c(0.3, 0.25, 0.15, 0.3)
)


df <- distr_table
df['probX']<- table(df$X)/length(df$X)
head(df)
df['probX']<- table(df$X)
table(df$X)
table(df$Y)

by_x<- df   %>% group_by(X,pr)%>% summarise(n =n())
by_x
by_x1<- by_x %>% summarise(n =sum(pr))
by_x1 <- data.frame(by_xy1)
by_x1
df1<- merge(df,by_xy1, by = c('X','X'))
df1<- subset(df1, select = c(X, Y, pr, n))
df1
names(df1)[4]<- 'probX'
df1

by_y<- df   %>% group_by(Y,pr)%>% summarise(n =n())
by_y
by_y1<- by_y %>% summarise(n =sum(pr))
by_y1 <- data.frame(by_y1)
by_y1
df2<- merge(df1,by_y1, by = c('Y','Y'))
df2
df2<- subset(df2, select = c(X, Y, pr,probX, n))
df2
names(df2)[5]<- 'probY'
df2
df2['XandY']<- df2$probX*df2$probY
df2$are_independent<- ifelse(df2$pr==df2$XandY, T, F)
df2
df2 %>% arrange(X)


df = distr_table
df["X-Xmean"]= df['X']-mean(df[['X']])
df['Y-Ymean'] = df['X']- mean(df[['X']])
df['XandY'] = df['X-Xmean']*df['Y-Ymean']*df$pr
df
covXY = sum(df$XandY)#/length(df$X)
covXY
#are_independent = ifelse(sum(df$XandY)==0, True,False)
cov(df$X,df$Y)
df['probX'] = table(df$X)/length(df$X)
df['probY'] = table(df$Y)/length(df$Y)
#df$are_independent = ifelse(df$pr==(df$probX*df$probY), True, False)
df['probXY'] = df$probX * df$probY
df['are_independent'] = ifelse(df$probXY == df$pr, T, F)
df2

check_independence <- function(distr_table) {
  # write your solution here
  df = distr_table
  
  #are_independent = ifelse(sum(df$XandY)==0, True,False)
  
  by_x<- df   %>% group_by(X,pr)%>% summarise(n =n())
  by_x
  by_x1<- by_x %>% summarise(n =sum(pr))
  by_x1 <- data.frame(by_xy1)
  by_x1
  df1<- merge(df,by_xy1, by = c('X','X'))
  df1<- subset(df1, select = c(X, Y, pr, n))
  #df1
  names(df1)[4]<- 'probX'
  #df1
  
  by_y<- df   %>% group_by(Y,pr)%>% summarise(n =n())
  by_y
  by_y1<- by_y %>% summarise(n =sum(pr))
  by_y1 <- data.frame(by_y1)
  by_y1
  df2<- merge(df1,by_y1, by = c('Y','Y'))
  #df2
  df2<- subset(df2, select = c(X, Y, pr,probX, n))
  df2
  names(df2)[5]<- 'probY'
  df2
  df2['XandY']<- df2$probX*df2$probY
  df2$are_independent<- ifelse(df2$pr==df2$XandY, T, F)
  df2 <- df2 %>% arrange(X)
  are_independent = all(df2$are_independent)
  
  df2['probXX']<- df2$X* df2$probX
  df2['probYY']<- df2$Y*df2$probY
  
  df["X-Xmean"]= df['X']-(mean(df2[['probXX']]))
  df['Y-Ymean'] = df['Y']- (mean(df2[['probYY']]))
  df['XandY'] = df['X-Xmean']*df['Y-Ymean']*df$pr
  covXY = sum(df$XandY)#/length(df$X)
  
  df['stdX'] = df2$probX*df['X-Xmean']*df['X-Xmean']
  df['stdY'] = df2$probY*df['Y-Ymean']*df['Y-Ymean']
  stdX = sqrt(sum(df$stdX))
  stdY = sqrt(sum(df$stdY))
  corr = covXY/(stdX*stdY)
  list1 <- list(are_independent,covXY,corr)
  return (list1)
  }

check_independence <- function(distr_table) {
  # write your solution here
  df<- distr_table
  by_x<- df %>% group_by(X,pr) %>% summarise(n=n())
  by_x1<- by_x %>% summarise(n=sum(n))
  by_x1<- data.frame(by_x1)
  df1<- merge(df, by_x1, by = c('X','X'))
  df1 <- subset(df1, select = c(X,Y,pr,n))
  names(df1)[4]<- 'probX'
  
  by_y<- df %>% group_by(Y,pr) %>% summarise(n=n())
  by_y1<- by_y %>% summarise(n=sum(n))
  by_y1<- data.frame(by_y1)
  df2<- merge(df1,by_y1, by= c('Y','Y'))
  df2<- df2 %>% arrange(X)
  names(df2)[5]<- 'probY'
  df2['XandY']<- df2$probX*df2$probY
  df2$are_independent<- ifelse(df2$pr==df2$XandY, T, F)
  are_independent<- all(df2$are_independent)
  
  df2$probXX<- df2$X*df2$probX
  df2$probYY<- df2$Y*df2$probY
  
  df2$X-Xmean<- df2$X- mean(df2$probXX)
  df2$Y-Ymean<- df2$Y - mean(df2$probYY)
  cov<- sum(df2$pr*df2$X-Xmean*df2$Y-Ymean)
  
  df2$sdX<- df2$probX*df2$X-Xmean*df2$X-Xmean
  df2$sdY<- df2$probY*df2$Y-Ymean*df2$Y-Ymean
  sdX<- sqrt(sum(df2$sdX))
  sdY<- sqrt(sum(df2$sdY))
  corr<- cov/(sdX*sdY)
  list1<- list(are_independent,cov,corr)
  
  return (list1)
  
}

check_independence <- function(distr_table) {
  # write your solution here
  df<- distr_table
  by_x<- df %>% group_by(X,pr) %>% summarise(n=n())
  by_x1<- by_x %>% summarise(n=sum(n))
  by_x1<- data.frame(by_x1)
  df1<- merge(df, by_x1, by = c('X','X'))
  df1 <- subset(df1, select = c(X,Y,pr,n))
  names(df1)[4]<- 'probX'
  
  by_y<- df %>% group_by(Y,pr) %>% summarise(n=n())
  by_y1<- by_y %>% summarise(n=sum(n))
  by_y1<- data.frame(by_y1)
  df2<- merge(df1,by_y1, by= c('Y','Y'))
  df2<- df2 %>% arrange(X)
  names(df2)[5]<- 'probY'
  df2['XandY']<- df2$probX*df2$probY
  df2$are_independent<- ifelse(df2$pr==df2$XandY, T, F)
  are_independent<- all(df2$are_independent)
  
  df2$probXX<- df2$X*df2$probX
  df2$probYY<- df2$Y*df2$probY
  
  df2['X-Xmean']<- df2$X- sum(df2$probXX)
  df2['Y-Ymean']<- df2$Y - sum(df2$probYY)
  cov<- sum(df2$pr*df2['X-Xmean']*df2['Y-Ymean'])
  
  df2$sdX<- df2$probX*df2['X-Xmean']*df2['X-Xmean']
  df2$sdY<- df2$probY*df2['Y-Ymean']*df2['Y-Ymean']
  sdX<- sqrt(sum(df2$sdX))
  sdY<- sqrt(sum(df2$sdY))
  corr<- cov/(sdX*sdY)
  list1<- list(are_independent,cov,corr)
  
  return (list1)
  
}

check_independence(distr_table)
cov(df$X,df$Y)
cor(df$X,df$Y)

sd(df$X)
sd(df$Y)
df<- read.csv('realest.csv', header = T)
head(df)

nrow(df)
nrow(df1)
df1<- df[!is.na(df)]
mean(df1$Tax)

anyNA(df)
sum(is.na(df))

df <- na.omit(df)
nrow(df)

df1<- df[which(df$Bathroom==2 | df$Bedroom==4),]
summary(df1)
nrow(df1)

mean(df1$Tax)
sd(df1$Tax)
statistics<- c(mean(df1$Tax), sd(df1$Tax), median(df1$Tax), min(df1$Tax), max(df1$Tax))
statistics

library(dplyr)
data_frame<- df[df$Space>800,]
nrow(data_frame)
data_frame <- data_frame %>% arrange(desc(Price))
head(data_frame)


quantile(df$Lot, probs = c(0, 0.25, 0.5, 0.75, 1))
quantile(df$Lot, probs = c(0.08, 0.25, 0.5, 0.75, 1))


number_of_observation<- nrow(df[which(df$Lot>= quantile(df$Lot, probs = c(0.08))),])
number_of_observation
summary_list<- c(statistics,data_frame,number_of_observation)
summary_list<- list(statistics,data_frame,number_of_observation)
summary_list

model<- lm(Price~., data= df)
model_parameters<- model$coefficient
model_parameters

Bedroom<- c(3)  
Space<- c(1500)
Room<- c(8)
Lot<- c(40)
Tax<- c(1000) 
Bathroom<- c(2)
Garage<- c(1)
Condition<- c(0)
var <- data.frame( Bedroom , Space,Room, Lot , Tax,  Bathroom , Garage, Condition )
price_prediction<- predict(model, var)
price_prediction
regression_list<- list(model_parameters, price_prediction)
regression_list


analyse_and_fit_lrm <- function(file_path = 'realest.csv') 
  {
  # write your solution here
  df<- read.csv(file_path, header =T)
  df<- na.omit(df)
  df1<- df[which(df$Bathroom==2 | df$Bedroom==4),]
  statistics<- list(mean(df1$Tax), sd(df1$Tax), median(df1$Tax), min(df1$Tax), max(df1$Tax))
  
  data_frame<- df[df$Space>800,]
  data_frame <- data_frame %>% arrange(desc(Price))
  
  number_of_observation<- nrow(df[which(df$Lot>= quantile(df$Lot, probs = c(0.08))),])
  
  summary_list<- list(statistics,data_frame,number_of_observation)
  
  model<- lm(Price~., data= df)
  model_parameters<- model$coefficient
  
  Bedroom<- c(3)  
  Space<- c(1500)
  Room<- c(8)
  Lot<- c(40)
  Tax<- c(1000) 
  Bathroom<- c(2)
  Garage<- c(1)
  Condition<- c(0)
  var <- data.frame( Bedroom , Space,Room, Lot , Tax,  Bathroom , Garage, Condition )
  price_prediction<- predict(model, var)
  
  regression_list<- list(model_parameters, price_prediction)
  
  return(list(summary_list, regression_list))
  
  
}

file_path='realest.csv'
df<- read.csv(file_path, header =T)
df<- na.omit(df)
df1<- df[which(df$Bathroom==2 | df$Bedroom==4 ),]
df1
df1<- na.omit(df1)
df1[is.na(df1)]<- 0

#statistics<- list(mean(df1$Tax), sd(df1$Tax), median(df1$Tax), min(df1$Tax), max(df1$Tax))
statistics<- c(as.numeric(mean(df1$Tax)), as.numeric(sd(df1$Tax)), as.numeric(median(df1$Tax)), as.numeric(min(df1$Tax)),
               as.numeric(max(df1$Tax)))
round(statistics)

df<- read.csv(file_path, header =T)
nrow(df)
df<- df[df[,'Space']>=800,]
nrow(df)
df<- df %>% filter(Space>800)
nrow(df)


df<- na.omit(df)
nrow(df)
data_frame<- df
data_frame <- data_frame %>% arrange(desc(Price))
data_framre<- data.frame(data_frame)
str(data_frame)


df<- read.csv(file_path, header =T)
df<- na.omit(df)
number_of_observation<- nrow(df[df$Lot>= quantile(df$Lot, probs = c(0.08)),])
number_of_observation

x = quantile(df$Lot, probs = c(0.08), na.rm = T)

number_of_observation<- nrow(df[df$Lot>= x,])

df<- read.csv(file_path, header =T)
#df<- na.omit(df)
x = quantile(df$Lot, probs = c(0.08), na.rm = T)
number_of_observation<- nrow(df[which(df$Lot>= x),])
number_of_observation

analyse_and_fit_lrm <- function (file_path='realest.csv') {
  
  # a path to a dataset is "./data/realest.csv"
  # dataset can be loaded by uncommenting the line bellow
  # df <- read.csv(file_path)
  
  # write your solution here
  df<- read.csv(file_path, header =T)
  df<- na.omit(df)
  df1<- df[which(df$Bathroom==2 | df$Bedroom==4),]
  statistics<- c(round(mean(df1$Tax)), round(sd(df1$Tax)), round(median(df1$Tax)), round(min(df1$Tax)),
                 round(max(df1$Tax)))
  
  df<- read.csv(file_path, header =T)
  data_frame<- df[df$Space>800,]
  data_frame <- data_frame %>% arrange(desc(Price))
  data_framre<- data.frame(data_frame)
  
  df<- read.csv(file_path, header =T)
  number_of_observation<- nrow(df[which(df$Lot>= quantile(df$Lot, probs = c(0.08))),])
  
  summary_list<- list(statistics,data_frame,number_of_observation)
  
  model<- lm(Price~., data= df)
  model_parameters<- model$coefficient
  
  Bedroom<- c(3)  
  Space<- c(1500)
  Room<- c(8)
  Lot<- c(40)
  Tax<- c(1000) 
  Bathroom<- c(2)
  Garage<- c(1)
  Condition<- c(0)
  var <- data.frame( Bedroom , Space,Room, Lot , Tax,  Bathroom , Garage, Condition )
  price_prediction<- predict(model, var)
  
  regression_list<- list(model_parameters, price_prediction)
  list1<-list(summary_list, regression_list) 
  return(list1)
  
}


analyse_and_fit_lrm()
