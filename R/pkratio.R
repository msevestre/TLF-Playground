library("ggplot2")


ages <- seq(20,30) #create a sequence of ages (independent variable)
#ages #print ages
all_age_vec <- vector() #initiate vector of all ages
all_heights <- vector() #initiate vector of all ages
all_genders <- vector() #initiate vector of all ages

for ( n in ages ){
  number_of_measurements_for_age = sample(0:20, 1)  #set a random number of measurements for each age
  heights = rnorm(number_of_measurements_for_age,5,0.2) #for each measurement, assume 
  genders = sample(c("female","male"),size = number_of_measurements_for_age,replace = TRUE)
  age_vec = n*rep(1,number_of_measurements_for_age)
  all_age_vec <-c(all_age_vec,age_vec)  
  all_heights <-c(all_heights,heights)  
  all_genders <-c(all_genders,genders)  
}

all_age_vec
all_heights
all_genders  

df = data.frame(all_age_vec,all_genders,all_heights)
colnames(df)<-c("ages","genders","heights")
df$genders<-as.factor(df$genders)
df
#pure data generation above ^^^^

pkrp <-ggplot(df, aes(x=df$ages, y=df$heights , color =  df$genders)) + geom_point()+ labs(title = "heights for each age", x="ages" , y = "heights" , color = "gender")  
pkrp <-pkrp + geom_hline(yintercept=5, linetype="solid", color = "black", size = 2)
pkrp <-pkrp + geom_hline(yintercept=5.5, linetype="dashed", color = "blue")
pkrp <-pkrp + geom_hline(yintercept=6, linetype="dashed", color = "blue")
pkrp <-pkrp + geom_hline(yintercept=4.5, linetype="dashed", color = "red")
pkrp <-pkrp + geom_hline(yintercept=4, linetype="dashed", color = "red")
pkrp
