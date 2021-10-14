library(data.table)
Survey = fread('/Users/ryadav54/Downloads/patientsurveyhcahps-state-pmrdotcf-tn3xzzur.csv')
Survey$`HCAHPS Answer Percent` = as.numeric(str_replace_all(Survey$`HCAHPS Answer Percent`, "%", ""))

summary(Survey$`HCAHPS Answer Percent`)

Survey = Survey[!is.na(Survey$`HCAHPS Answer Percent`),]


library(ggplot2)
den1 = ggplot(Survey, aes(x=`HCAHPS Answer Percent`)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

qq1 = ggplot(Survey,aes(sample=`HCAHPS Answer Percent`))+stat_qq()

library(moments)

transformed <- abs(Survey$`HCAHPS Answer Percent` - mean(Survey$`HCAHPS Answer Percent`,na.rm = TRUE))
shapiro.test(transformed)
skewness(transformed)
transformed = as.data.frame(transformed)
# Histogram with density plot
den2 = ggplot(transformed, aes(x=transformed)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

qq2 = ggplot(transformed,aes(sample=transformed))+stat_qq()

transformed = log(Survey$`HCAHPS Answer Percent`)
shapiro.test(transformed)
skewness(transformed)
transformed = as.data.frame(transformed)
# Histogram with density plot
den3 = ggplot(transformed, aes(x=transformed)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

qq3 = ggplot(transformed,aes(sample=transformed))+stat_qq()

transformed = (Survey$`HCAHPS Answer Percent`)^2
shapiro.test(transformed)
skewness(transformed)
transformed = as.data.frame(transformed)

# Histogram with density plot
den4 = ggplot(transformed, aes(x=transformed)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

qq4 = ggplot(transformed,aes(sample=transformed))+stat_qq()
#Arrange all the plots onto one page
library(cowplot)
plot_grid(den1, den2, den3,den4, qq1, qq2, qq3,qq4 ,
          labels=c("Original-den", "Mean Deviation -den", "Logarithm-den", "Square-den", "Original-qq", "Mean Deviation -qq", "Logarithm-qq", "Square-qq"),
          ncol=4, nrow=2)

RI = subset(Survey,State == "RI")
sum(is.na(RI$`HCAHPS Answer Percent`))
