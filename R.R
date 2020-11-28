library(data.table)
library(tidyverse)
Day<-c("26/fev", "29/fev", "04/mar", "05/mar", 
       "06/mar", "07/mar", "08/mar", "10/mar", "11/mar", "12/mar", "13/mar", 
       "14/mar", "15/mar", "16/mar", "17/mar", "18/mar", "19/mar", "20/mar", 
       "21/mar", "22/mar", "23/mar", "24/mar", "25/mar")
Contaminated<-c(1L, 2L, 3L, 8L, 13L, 19L, 25L, 34L, 52L, 81L, 98L, 121L, 176L, 234L, 291L, 428L, 621L, 904L, 1128L, 1546L, 1891L, 2201L, 2433L)
data<-data.frame(Day,Contaminated)
setDT(data)
data[,Day:= as.Date(Day, "%d/%b")]
data[,Int := as.integer(Day)-min(as.integer(Day))]
modelo<-nls(formula = Contaminated ~ a * Int ^ b, data,start=list(a=1,b=1))
ggplot(data, aes(x = Day, y = Contaminated)) +
  geom_point() +
  geom_line(aes(x = Day, y = predict(modelo, data$Int))) +
  theme_bw()
predict(modelo, data.frame(Int =as.integer(as.Date("2020-03-26"))-min(as.integer(data$Day))))