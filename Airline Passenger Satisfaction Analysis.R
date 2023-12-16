#colors: #CEDFEF,#92C2DD,#4995C6,#1663A9,#B4B4D5,#8481BA,#614099

train = read.csv('D://22Fall//CDA//report//airplane satisfaction//train.csv')
test = read.csv('D://22Fall//CDA//report//airplane satisfaction//test.csv')
data = rbind(train,test)

library(ggplot2)
par(mfrow = c(1,1))
slices.1 = as.numeric(table(data$satisfaction))
labels.1 = c("neutral or dissatisfied","satisfied")
prop.1 = prop.table(table(data$satisfaction))
pie(slices.1,labels = c(paste(labels.1,":",round(prop.1*100,2),"%")), main="Pie Chart of Satisfaction",col = c('#CEDFEF','#92C2DD'))

par(mfrow=c(2,2),mar = c(1,1,1,1))

slices.2 = as.numeric(table(data$Gender))
labels.2 = c("Male","Female")
prop.2 = prop.table(table(data$Gender))
pie(slices.2,labels = c(paste(labels.2,":",round(prop.2*100,2),"%")), main="Gender",col = c('#CEDFEF','#92C2DD'))

slices.3 = as.numeric(table(data$Customer.Type))
labels.3 = c("Loyal Customer","disloyal Customer")
prop.3 = prop.table(table(data$Customer.Type))
pie(slices.3,labels = c(paste(labels.3,":",round(prop.3*100,2),"%")), main="Customer Type",col = c('#CEDFEF','#92C2DD'))

slices.4 = as.numeric(table(data$Type.of.Travel))
labels.4 = c("Personal Travel","Business Travel")
prop.4 = prop.table(table(data$Type.of.Travel))
pie(slices.4,labels = c(paste(labels.4,":",round(prop.4*100,2),"%")), main="Type of Travel",col = c('#CEDFEF','#92C2DD'))

slices.5 = as.numeric(table(data$Class))
labels.5 = c("Eco","Eco Plus","Business")
prop.5 = prop.table(table(data$Class))
pie(slices.5,labels = c(paste(labels.5,":",round(prop.5*100,2),"%")), main="Class",col = c('#CEDFEF','#92C2DD','#4995C6'))

par(mfrow = c(1,1),mar = c(5,4,4,2))

barplot(table(data$satisfaction,data$Class),col=c('#CEDFEF','#92C2DD'),xlab="Class",ylab="Count",beside=TRUE,las=1)
legend("topright",legend=c("Neutral or Dissatisfied","Satisfied"),fill=c('#CEDFEF','#92C2DD'))

par(mfrow=c(3,5),mar = c(1,1,1,1))

slices.6 = as.numeric(table(data$Inflight.wifi.service))
labels.6 = names(table(data$Inflight.wifi.service))
prop.6 = prop.table(table(data$Inflight.wifi.service))
pie(slices.6,labels = c(paste(labels.6,":",round(prop.6*100,2),"%")), 
    main="Inflight wifi service",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.7 = as.numeric(table(data$Departure.Arrival.time.convenient))
labels.7 = names(table(data$Departure.Arrival.time.convenient))
prop.7 = prop.table(table(data$Departure.Arrival.time.convenient))
pie(slices.7,labels = c(paste(labels.7,":",round(prop.7*100,2),"%")), 
    main="Departure Arrival time convenient",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.8 = as.numeric(table(data$Ease.of.Online.booking))
labels.8 = names(table(data$Ease.of.Online.booking))
prop.8 = prop.table(table(data$Ease.of.Online.booking))
pie(slices.8,labels = c(paste(labels.8,":",round(prop.8*100,2),"%")), 
    main="Ease of Online booking",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.9 = as.numeric(table(data$Gate.location))
labels.9 = names(table(data$Gate.location))
prop.9 = prop.table(table(data$Gate.location))
pie(slices.9,labels = c(paste(labels.9,":",round(prop.9*100,2),"%")), 
    main="Gate location",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.10 = as.numeric(table(data$Food.and.drink))
labels.10 = names(table(data$Food.and.drink))
prop.10 = prop.table(table(data$Food.and.drink))
pie(slices.10,labels = c(paste(labels.10,":",round(prop.10*100,2),"%")), 
    main="Food and drink",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.11 = as.numeric(table(data$Online.boarding))
labels.11 = names(table(data$Online.boarding))
prop.11 = prop.table(table(data$Online.boarding))
pie(slices.11,labels = c(paste(labels.11,":",round(prop.11*100,2),"%")), 
    main="Online boarding",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.12 = as.numeric(table(data$Seat.comfort))
labels.12 = names(table(data$Seat.comfort))
prop.12 = prop.table(table(data$Seat.comfort))
pie(slices.12,labels = c(paste(labels.12,":",round(prop.12*100,2),"%")), 
    main="Seat comfort",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.13 = as.numeric(table(data$Inflight.entertainment))
labels.13 = names(table(data$Inflight.entertainment))
prop.13 = prop.table(table(data$Inflight.entertainment))
pie(slices.13,labels = c(paste(labels.13,":",round(prop.13*100,2),"%")), 
    main="Inflight entertainment",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.14 = as.numeric(table(data$On.board.service))
labels.14 = names(table(data$On.board.service))
prop.14 = prop.table(table(data$On.board.service))
pie(slices.14,labels = c(paste(labels.14,":",round(prop.14*100,2),"%")), 
    main="On-board service",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.15 = as.numeric(table(data$Leg.room.service))
labels.15 = names(table(data$Leg.room.service))
prop.15 = prop.table(table(data$Leg.room.service))
pie(slices.15,labels = c(paste(labels.15,":",round(prop.15*100,2),"%")), 
    main="Leg room service",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.16 = as.numeric(table(data$Baggage.handling))
labels.16 = names(table(data$Baggage.handling))
prop.16 = prop.table(table(data$Baggage.handling))
pie(slices.16,labels = c(paste(labels.16,":",round(prop.16*100,2),"%")), 
    main="Baggage handling",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.17 = as.numeric(table(data$Checkin.service))
labels.17 = names(table(data$Checkin.service))
prop.17 = prop.table(table(data$Checkin.service))
pie(slices.17,labels = c(paste(labels.17,":",round(prop.17*100,2),"%")), 
    main="Checkin service",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.18 = as.numeric(table(data$Inflight.service))
labels.18 = names(table(data$Inflight.service))
prop.18 = prop.table(table(data$Inflight.service))
pie(slices.18,labels = c(paste(labels.18,":",round(prop.18*100,2),"%")), 
    main="Inflight service",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

slices.19 = as.numeric(table(data$Cleanliness))
labels.19 = names(table(data$Cleanliness))
prop.19 = prop.table(table(data$Cleanliness))
pie(slices.19,labels = c(paste(labels.19,":",round(prop.19*100,2),"%")), 
    main="Cleanliness",col = c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))

par(mfrow=c(1,1),mar = c(5,4,4,2))

plot(data$Departure.Delay.in.Minutes,data$Arrival.Delay.in.Minutes,col = '#92C2DD',
     xlab = 'Departure Delay in Minutes',ylab = 'Arrival Delay in Minutes')

library(GGally)
ggcorr(data[,c(3,6,21,22)], nbreaks = 8,low = 'red', mid = 'white', high = 'blue',method = c("everything", "pearson"))

library(plotrix)
histStack(Age~satisfaction,data = data,xlab="Age",ylab="Count",ylim=c(0,20000),col=c('#CEDFEF','#92C2DD'),legend.pos="topright")
histStack(Age~Customer.Type,data = data,xlab="Age",ylab="Count",ylim=c(0,20000),col=c('#CEDFEF','#92C2DD'),legend.pos="topright")
histStack(Age~Class,data = data,xlab="Age",ylab="Count",ylim=c(0,20000),col=c('#CEDFEF','#92C2DD','#4995C6'),legend.pos="topright")

boxplot(Age~satisfaction,data = data,xlab="Satisfaction",ylab="Age",col=c('#CEDFEF','#92C2DD'))
boxplot(Age~Customer.Type,data = data,xlab="Customer Type",ylab="Age",col=c('#CEDFEF','#92C2DD'))
boxplot(Age~Class,data = data,xlab="Class",ylab="Age",col=c('#CEDFEF','#92C2DD','#4995C6'))
boxplot(Flight.Distance~Class,data = data,xlab="Class",ylab="Flight Distance",col=c('#CEDFEF','#92C2DD','#4995C6'))
boxplot(Flight.Distance~Leg.room.service,data = data,xlab="Leg room service",ylab="Flight Distance",col=c('#CEDFEF','#92C2DD','#4995C6','#1663A9','#B4B4D5','#8481BA'))
boxplot(Flight.Distance~satisfaction,data = data,xlab="Satisfaction",ylab="Flight Distance",col=c('#CEDFEF','#92C2DD'))

summary(lm(data$Arrival.Delay.in.Minutes~data$Departure.Delay.in.Minutes))
data$Arrival.Delay.in.Minutes[is.na(data$Arrival.Delay.in.Minutes)] = 0.757464 + 0.978849 * data$Departure.Delay.in.Minutes[is.na(data$Arrival.Delay.in.Minutes)]

t.test(data$Age[data$satisfaction == 0],data$Age[data$satisfaction == 1])
t.test(data$Flight.Distance[data$satisfaction == 0],data$Flight.Distance[data$satisfaction == 1])
t.test(data$Departure.Delay.in.Minutes[data$satisfaction == 0],data$Departure.Delay.in.Minutes[data$satisfaction == 1])
t.test(data$Arrival.Delay.in.Minutes[data$satisfaction == 0],data$Arrival.Delay.in.Minutes[data$satisfaction == 1])

library(gmodels)
library(epiDisplay)

table.1 = table(data$satisfaction,data$Gender)
addmargins(table.1)
CrossTable(data$satisfaction,data$Gender, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Gender)
cs(cctable=table.1)
cc(cctable=table.1)

table.2 = table(data$satisfaction,data$Customer.Type)
addmargins(table.2)
CrossTable(data$satisfaction,data$Customer.Type, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Customer.Type)
cs(cctable=table.2)
cc(cctable=table.2)

table.3 = table(data$satisfaction,data$Type.of.Travel)
addmargins(table.3)
CrossTable(data$satisfaction,data$Type.of.Travel, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Type.of.Travel)
cs(cctable=table.3)
cc(cctable=table.3)

table.4 = table(data$satisfaction,data$Class)
addmargins(table.4)
CrossTable(data$satisfaction,data$Class, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Class)
cs(cctable=table.4)
cc(cctable=table.4)

table.5 = table(data$satisfaction,data$Inflight.wifi.service)
addmargins(table.5)
CrossTable(data$satisfaction,data$Inflight.wifi.service, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Inflight.wifi.service)
cs(cctable=table.5)
cc(cctable=table.5)

table.6 = table(data$satisfaction,data$Departure.Arrival.time.convenient)
addmargins(table.6)
CrossTable(data$satisfaction,data$Departure.Arrival.time.convenient, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Departure.Arrival.time.convenient)
cs(cctable=table.6)
cc(cctable=table.6)

table.7 = table(data$satisfaction,data$Ease.of.Online.booking)
addmargins(table.7)
CrossTable(data$satisfaction,data$Ease.of.Online.booking, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Ease.of.Online.booking)
cs(cctable=table.7)
cc(cctable=table.7)

table.8 = table(data$satisfaction,data$Gate.location)
addmargins(table.8)
CrossTable(data$satisfaction,data$Gate.location, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Gate.location)
cs(cctable=table.8)
cc(cctable=table.8)

table.9 = table(data$satisfaction,data$Food.and.drink)
addmargins(table.9)
CrossTable(data$satisfaction,data$Food.and.drink, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Food.and.drink)
cs(cctable=table.9)
cc(cctable=table.9)

table.10 = table(data$satisfaction,data$Online.boarding)
addmargins(table.10)
CrossTable(data$satisfaction,data$Online.boarding, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Online.boarding)
cs(cctable=table.10)
cc(cctable=table.10)

table.11 = table(data$satisfaction,data$Seat.comfort)
addmargins(table.11)
CrossTable(data$satisfaction,data$Seat.comfort, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Seat.comfort)
cs(cctable=table.11)
cc(cctable=table.11)

table.12 = table(data$satisfaction,data$Inflight.entertainment)
addmargins(table.12)
CrossTable(data$satisfaction,data$Inflight.entertainment, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Inflight.entertainment)
cs(cctable=table.12)
cc(cctable=table.12)

table.13 = table(data$satisfaction,data$On.board.service)
addmargins(table.13)
CrossTable(data$satisfaction,data$On.board.service, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$On.board.service)
cs(cctable=table.13)
cc(cctable=table.13)

table.14 = table(data$satisfaction,data$Leg.room.service)
addmargins(table.14)
CrossTable(data$satisfaction,data$Leg.room.service, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Leg.room.service)
cs(cctable=table.14)
cc(cctable=table.14)

table.15 = table(data$satisfaction,data$Baggage.handling)
addmargins(table.15)
CrossTable(data$satisfaction,data$Baggage.handling, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Baggage.handling)
cs(cctable=table.15)
cc(cctable=table.15)

table.16 = table(data$satisfaction,data$Checkin.service)
addmargins(table.16)
CrossTable(data$satisfaction,data$Checkin.service, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Checkin.service)
cs(cctable=table.16)
cc(cctable=table.16)

table.17 = table(data$satisfaction,data$Inflight.service)
addmargins(table.17)
CrossTable(data$satisfaction,data$Inflight.service, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Inflight.service)
cs(cctable=table.17)
cc(cctable=table.17)

table.18 = table(data$satisfaction,data$Cleanliness)
addmargins(table.18)
CrossTable(data$satisfaction,data$Cleanliness, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq=FALSE)
tabpct(data$satisfaction,data$Cleanliness)
cs(cctable=table.18)
cc(cctable=table.18)

#logistic
data$Gender[data$Gender == 'Male'] = 0;data$Gender[data$Gender == 'Female'] = 1
data$Customer.Type[data$Customer.Type == 'Loyal Customer'] = 0;data$Customer.Type[data$Customer.Type == 'disloyal Customer'] = 1
data$Type.of.Travel[data$Type.of.Travel == 'Personal Travel'] = 0;data$Type.of.Travel[data$Type.of.Travel == 'Business travel'] = 1
data$satisfaction[data$satisfaction == 'neutral or dissatisfied'] = 0;data$satisfaction[data$satisfaction == 'satisfied'] = 1
data$Eco[data$Class == 'Eco'] = 1;data$Eco[data$Class != 'Eco'] = 0
data$Business[data$Class == 'Business'] = 1;data$Business[data$Class != 'Business'] = 0
data$Inflight.wifi.service.1[data$Inflight.wifi.service == 1] = 1;data$Inflight.wifi.service.1[data$Inflight.wifi.service != 1] = 0
data$Inflight.wifi.service.2[data$Inflight.wifi.service == 2] = 1;data$Inflight.wifi.service.2[data$Inflight.wifi.service != 2] = 0
data$Inflight.wifi.service.3[data$Inflight.wifi.service == 3] = 1;data$Inflight.wifi.service.3[data$Inflight.wifi.service != 3] = 0
data$Inflight.wifi.service.4[data$Inflight.wifi.service == 4] = 1;data$Inflight.wifi.service.4[data$Inflight.wifi.service != 4] = 0
data$Inflight.wifi.service.5[data$Inflight.wifi.service == 5] = 1;data$Inflight.wifi.service.5[data$Inflight.wifi.service != 5] = 0
data$Departure.Arrival.time.convenient.1[data$Departure.Arrival.time.convenient == 1] = 1;data$Departure.Arrival.time.convenient.1[data$Departure.Arrival.time.convenient != 1] = 0
data$Departure.Arrival.time.convenient.2[data$Departure.Arrival.time.convenient == 2] = 1;data$Departure.Arrival.time.convenient.2[data$Departure.Arrival.time.convenient != 2] = 0
data$Departure.Arrival.time.convenient.3[data$Departure.Arrival.time.convenient == 3] = 1;data$Departure.Arrival.time.convenient.3[data$Departure.Arrival.time.convenient != 3] = 0
data$Departure.Arrival.time.convenient.4[data$Departure.Arrival.time.convenient == 4] = 1;data$Departure.Arrival.time.convenient.4[data$Departure.Arrival.time.convenient != 4] = 0
data$Departure.Arrival.time.convenient.5[data$Departure.Arrival.time.convenient == 5] = 1;data$Departure.Arrival.time.convenient.5[data$Departure.Arrival.time.convenient != 5] = 0
data$Ease.of.Online.booking.1[data$Ease.of.Online.booking == 1] = 1;data$Ease.of.Online.booking.1[data$Ease.of.Online.booking != 1] = 0
data$Ease.of.Online.booking.2[data$Ease.of.Online.booking == 2] = 1;data$Ease.of.Online.booking.2[data$Ease.of.Online.booking != 2] = 0
data$Ease.of.Online.booking.3[data$Ease.of.Online.booking == 3] = 1;data$Ease.of.Online.booking.3[data$Ease.of.Online.booking != 3] = 0
data$Ease.of.Online.booking.4[data$Ease.of.Online.booking == 4] = 1;data$Ease.of.Online.booking.4[data$Ease.of.Online.booking != 4] = 0
data$Ease.of.Online.booking.5[data$Ease.of.Online.booking == 5] = 1;data$Ease.of.Online.booking.5[data$Ease.of.Online.booking != 5] = 0
data$Gate.location.1[data$Gate.location == 1] = 1;data$Gate.location.1[data$Gate.location != 1] = 0
data$Gate.location.2[data$Gate.location == 2] = 1;data$Gate.location.2[data$Gate.location != 2] = 0
data$Gate.location.3[data$Gate.location == 3] = 1;data$Gate.location.3[data$Gate.location != 3] = 0
data$Gate.location.4[data$Gate.location == 4] = 1;data$Gate.location.4[data$Gate.location != 4] = 0
data$Gate.location.5[data$Gate.location == 5] = 1;data$Gate.location.5[data$Gate.location != 5] = 0
data$Food.and.drink.1[data$Food.and.drink == 1] = 1;data$Food.and.drink.1[data$Food.and.drink != 1] = 0
data$Food.and.drink.2[data$Food.and.drink == 2] = 1;data$Food.and.drink.2[data$Food.and.drink != 2] = 0
data$Food.and.drink.3[data$Food.and.drink == 3] = 1;data$Food.and.drink.3[data$Food.and.drink != 3] = 0
data$Food.and.drink.4[data$Food.and.drink == 4] = 1;data$Food.and.drink.4[data$Food.and.drink != 4] = 0
data$Food.and.drink.5[data$Food.and.drink == 5] = 1;data$Food.and.drink.5[data$Food.and.drink != 5] = 0
data$Online.boarding.1[data$Online.boarding == 1] = 1;data$Online.boarding.1[data$Online.boarding != 1] = 0
data$Online.boarding.2[data$Online.boarding == 2] = 1;data$Online.boarding.2[data$Online.boarding != 2] = 0
data$Online.boarding.3[data$Online.boarding == 3] = 1;data$Online.boarding.3[data$Online.boarding != 3] = 0
data$Online.boarding.4[data$Online.boarding == 4] = 1;data$Online.boarding.4[data$Online.boarding != 4] = 0
data$Online.boarding.5[data$Online.boarding == 5] = 1;data$Online.boarding.5[data$Online.boarding != 5] = 0
data$Seat.comfort.1[data$Seat.comfort == 1] = 1;data$Seat.comfort.1[data$Seat.comfort != 1] = 0
data$Seat.comfort.2[data$Seat.comfort == 2] = 1;data$Seat.comfort.2[data$Seat.comfort != 2] = 0
data$Seat.comfort.3[data$Seat.comfort == 3] = 1;data$Seat.comfort.3[data$Seat.comfort != 3] = 0
data$Seat.comfort.4[data$Seat.comfort == 4] = 1;data$Seat.comfort.4[data$Seat.comfort != 4] = 0
data$Seat.comfort.5[data$Seat.comfort == 5] = 1;data$Seat.comfort.5[data$Seat.comfort != 5] = 0
data$Inflight.entertainment.1[data$Inflight.entertainment == 1] = 1;data$Inflight.entertainment.1[data$Inflight.entertainment != 1] = 0
data$Inflight.entertainment.2[data$Inflight.entertainment == 2] = 1;data$Inflight.entertainment.2[data$Inflight.entertainment != 2] = 0
data$Inflight.entertainment.3[data$Inflight.entertainment == 3] = 1;data$Inflight.entertainment.3[data$Inflight.entertainment != 3] = 0
data$Inflight.entertainment.4[data$Inflight.entertainment == 4] = 1;data$Inflight.entertainment.4[data$Inflight.entertainment != 4] = 0
data$Inflight.entertainment.5[data$Inflight.entertainment == 5] = 1;data$Inflight.entertainment.5[data$Inflight.entertainment != 5] = 0
data$On.board.service.1[data$On.board.service == 1] = 1;data$On.board.service.1[data$On.board.service != 1] = 0
data$On.board.service.2[data$On.board.service == 2] = 1;data$On.board.service.2[data$On.board.service != 2] = 0
data$On.board.service.3[data$On.board.service == 3] = 1;data$On.board.service.3[data$On.board.service != 3] = 0
data$On.board.service.4[data$On.board.service == 4] = 1;data$On.board.service.4[data$On.board.service != 4] = 0
data$On.board.service.5[data$On.board.service == 5] = 1;data$On.board.service.5[data$On.board.service != 5] = 0
data$Leg.room.service.1[data$Leg.room.service == 1] = 1;data$Leg.room.service.1[data$Leg.room.service != 1] = 0
data$Leg.room.service.2[data$Leg.room.service == 2] = 1;data$Leg.room.service.2[data$Leg.room.service != 2] = 0
data$Leg.room.service.3[data$Leg.room.service == 3] = 1;data$Leg.room.service.3[data$Leg.room.service != 3] = 0
data$Leg.room.service.4[data$Leg.room.service == 4] = 1;data$Leg.room.service.4[data$Leg.room.service != 4] = 0
data$Leg.room.service.5[data$Leg.room.service == 5] = 1;data$Leg.room.service.5[data$Leg.room.service != 5] = 0
data$Baggage.handling.2[data$Baggage.handling == 2] = 1;data$Baggage.handling.2[data$Baggage.handling != 2] = 0
data$Baggage.handling.3[data$Baggage.handling == 3] = 1;data$Baggage.handling.3[data$Baggage.handling != 3] = 0
data$Baggage.handling.4[data$Baggage.handling == 4] = 1;data$Baggage.handling.4[data$Baggage.handling != 4] = 0
data$Baggage.handling.5[data$Baggage.handling == 5] = 1;data$Baggage.handling.5[data$Baggage.handling != 5] = 0
data$Checkin.service.1[data$Checkin.service == 1] = 1;data$Checkin.service.1[data$Checkin.service != 1] = 0
data$Checkin.service.2[data$Checkin.service == 2] = 1;data$Checkin.service.2[data$Checkin.service != 2] = 0
data$Checkin.service.3[data$Checkin.service == 3] = 1;data$Checkin.service.3[data$Checkin.service != 3] = 0
data$Checkin.service.4[data$Checkin.service == 4] = 1;data$Checkin.service.4[data$Checkin.service != 4] = 0
data$Checkin.service.5[data$Checkin.service == 5] = 1;data$Checkin.service.5[data$Checkin.service != 5] = 0
data$Inflight.service.1[data$Inflight.service == 1] = 1;data$Inflight.service.1[data$Inflight.service != 1] = 0
data$Inflight.service.2[data$Inflight.service == 2] = 1;data$Inflight.service.2[data$Inflight.service != 2] = 0
data$Inflight.service.3[data$Inflight.service == 3] = 1;data$Inflight.service.3[data$Inflight.service != 3] = 0
data$Inflight.service.4[data$Inflight.service == 4] = 1;data$Inflight.service.4[data$Inflight.service != 4] = 0
data$Inflight.service.5[data$Inflight.service == 5] = 1;data$Inflight.service.5[data$Inflight.service != 5] = 0
data$Cleanliness.1[data$Cleanliness == 1] = 1;data$Cleanliness.1[data$Cleanliness != 1] = 0
data$Cleanliness.2[data$Cleanliness == 2] = 1;data$Cleanliness.2[data$Cleanliness != 2] = 0
data$Cleanliness.3[data$Cleanliness == 3] = 1;data$Cleanliness.3[data$Cleanliness != 3] = 0
data$Cleanliness.4[data$Cleanliness == 4] = 1;data$Cleanliness.4[data$Cleanliness != 4] = 0
data$Cleanliness.5[data$Cleanliness == 5] = 1;data$Cleanliness.5[data$Cleanliness != 5] = 0

train = data[1:103904,-c(1,2,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
train[,1] = as.numeric(unlist(train[,1]))
train[,2] = as.numeric(unlist(train[,2]))
train[,4] = as.numeric(unlist(train[,4]))
train[,8] = as.numeric(unlist(train[,8]))
test = data[103905:129880,-c(1,2,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
test[,1] = as.numeric(unlist(test[,1]))
test[,2] = as.numeric(unlist(test[,2]))
test[,4] = as.numeric(unlist(test[,4]))
test[,8] = as.numeric(unlist(test[,8]))

fit = glm(satisfaction ~ ., data = train, family = binomial(link = 'logit'))
summary(fit)

fit2 = glm(satisfaction ~ Gender + Customer.Type + Age + Type.of.Travel + 
             Departure.Delay.in.Minutes + Eco + Business + 
             Departure.Arrival.time.convenient.1 + Departure.Arrival.time.convenient.2 + 
             Departure.Arrival.time.convenient.3 + Departure.Arrival.time.convenient.4 + 
             Departure.Arrival.time.convenient.5 + Ease.of.Online.booking.1 + 
             Ease.of.Online.booking.2 + Ease.of.Online.booking.3 + Ease.of.Online.booking.4 + 
             Ease.of.Online.booking.5 + Online.boarding.1 + Online.boarding.2 + 
             Online.boarding.3 + Online.boarding.4 + Online.boarding.5 + 
             Leg.room.service.1 + Leg.room.service.2 + Leg.room.service.3 + 
             Leg.room.service.4 + Leg.room.service.5 + Baggage.handling.2 + 
             Baggage.handling.3 + Baggage.handling.4 + Baggage.handling.5 + 
             Checkin.service.1 + Checkin.service.2 + Checkin.service.3 + 
             Checkin.service.4 + Checkin.service.5 + Inflight.service.1 + 
             Inflight.service.2 + Inflight.service.3 + Inflight.service.4 + 
             Inflight.service.5 + Cleanliness.1 + Cleanliness.2 + Cleanliness.3 + 
             Cleanliness.4 + Cleanliness.5,data = train, family = binomial(link = 'logit'))
summary(fit2)
step(fit2)

fit3 = glm(satisfaction ~ Gender + Customer.Type + Age + Type.of.Travel + 
             Departure.Delay.in.Minutes + Eco + Business + 
             Departure.Arrival.time.convenient.1 + Departure.Arrival.time.convenient.2 + 
             Departure.Arrival.time.convenient.3 + Departure.Arrival.time.convenient.4 + 
             Departure.Arrival.time.convenient.5 + Ease.of.Online.booking.1 + 
             Ease.of.Online.booking.2 + Ease.of.Online.booking.3 + Ease.of.Online.booking.4 + 
             Ease.of.Online.booking.5 + Online.boarding.1 + Online.boarding.2 + 
             Online.boarding.3 + Online.boarding.4 + Online.boarding.5 + 
             Leg.room.service.1 + 
             Leg.room.service.2 + Leg.room.service.3 + Leg.room.service.4 + 
             Leg.room.service.5 + Baggage.handling.2 + Baggage.handling.3 + 
             Baggage.handling.5 + Checkin.service.1 + 
             Checkin.service.2 + Checkin.service.3 + Checkin.service.4 + 
             Inflight.service.1 + Inflight.service.2 + 
             Inflight.service.3 + Inflight.service.4 + Inflight.service.5 + 
             Cleanliness.1 + Cleanliness.2 + Cleanliness.3 + Cleanliness.4 + 
             Cleanliness.5,data = train, family = binomial(link = 'logit'))
summary(fit3)

library(pROC)
real = test$satisfaction
logisticpred = predict.glm(fit3, type = 'response', newdata = test)
logisticroc=roc(as.numeric(real),as.numeric(exp(logisticpred)/(1+exp(logisticpred))))
plot(logisticroc, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.2, 0.2), 
     grid.col = c("red"), max.auc.polygon = TRUE,auc.polygon.col = "skyblue", print.thres = TRUE)
pred_logistic = ifelse(logisticpred>0.5,1,0)
mtr_logistic = confusionMatrix(as.factor(pred_logistic),as.factor(test$satisfaction),positive = "1")
fourfoldplot(mtr_logistic$table, color = c("#8481BA", "#CEDFEF"),
             conf.level = 0, margin = 1, main = "Confusion Matrix of Logistic Regression")


train = data[1:103904,-c(1,2,7)]
train[,1] = as.numeric(unlist(train[,1]))
train[,2] = as.numeric(unlist(train[,2]))
train[,4] = as.numeric(unlist(train[,4]))
train[,22] = as.numeric(unlist(train[,22]))
test = data[103905:129880,-c(1,2,7)]
test[,1] = as.numeric(unlist(test[,1]))
test[,2] = as.numeric(unlist(test[,2]))
test[,4] = as.numeric(unlist(test[,4]))
test[,22] = as.numeric(unlist(test[,22]))

library(randomForest)
mod_rf1 = randomForest(formula = satisfaction~.,data = train)
rfpred = predict.glm(mod_rf1, type = 'response', newdata = test)
rfroc=roc(as.numeric(real),rfpred)
plot(rfroc, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.2, 0.2), 
     grid.col = c("red"), max.auc.polygon = TRUE,auc.polygon.col = "skyblue", print.thres = TRUE)
pred_rf = ifelse(rfpred>0.5,1,0)
mtr_rf = confusionMatrix(as.factor(pred_rf),as.factor(test$satisfaction),positive = "1")
fourfoldplot(mtr_rf$table, color = c("#8481BA", "#CEDFEF"),
             conf.level = 0, margin = 1, main = "Confusion Matrix of Random Forest")
varImpPlot(mod_rf1, main = "variable importance")

library(class)
library(kknn)
library(caret)
grid1 = expand.grid(.k = seq(2,25,by = 1))
control = trainControl(method = 'cv')
knn.train = train(satisfaction~.,data = train,method = 'knn',trControl = control,tuneGrid = grid1)
knn.train
knn.test = knn(train[,-22],test[,-22],train[,22],k = 21)
real = test$satisfaction
knnpred = predict.glm(knn.test, type = 'response', newdata = test)
knnroc=roc(as.numeric(real),as.numeric(knnpred))
plot(knnroc, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.2, 0.2), 
     grid.col = c("red"), max.auc.polygon = TRUE,auc.polygon.col = "skyblue", print.thres = TRUE)
pred_knn = ifelse(knnpred>0.5,1,0)
mtr_knn = confusionMatrix(as.factor(pred_knn),as.factor(test$satisfaction),positive = "1")
fourfoldplot(mtr_knn$table, color = c("#8481BA", "#CEDFEF"),
             conf.level = 0, margin = 1, main = "Confusion Matrix of KNN")

write.table(train,"D://22Fall//CDA//report//train2.csv",row.names=FALSE,col.names=TRUE,sep=",")
write.table(test,"D://22Fall//CDA//report//test2.csv",row.names=FALSE,col.names=TRUE,sep=",")
knn_pred = read.csv("D://22Fall//CDA//report//knn.csv")
rf_pred = read.csv("D://22Fall//CDA//report//rf.csv")

knnroc=roc(as.numeric(real),as.numeric(knn_pred[,2]))
plot(knnroc, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.2, 0.2), 
     grid.col = c("red"), max.auc.polygon = TRUE,auc.polygon.col = "skyblue", print.thres = TRUE)
mtr_knn = confusionMatrix(as.factor(knn_pred[,2]),as.factor(test$satisfaction),positive = "1")
fourfoldplot(mtr_knn$table, color = c("#8481BA", "#CEDFEF"),
             conf.level = 0, margin = 1, main = "Confusion Matrix of KNN")

rfroc=roc(as.numeric(real),as.numeric(rf_pred[,2]))
plot(rfroc, print.auc = TRUE, auc.polygon = TRUE, grid = c(0.2, 0.2), 
     grid.col = c("red"), max.auc.polygon = TRUE,auc.polygon.col = "skyblue", print.thres = TRUE)
mtr_rf = confusionMatrix(as.factor(rf_pred[,2]),as.factor(test$satisfaction),positive = "1")
fourfoldplot(mtr_rf$table, color = c("#8481BA", "#CEDFEF"),
             conf.level = 0, margin = 1, main = "Confusion Matrix of Random Forest")

importance = c(0.00482032,0.03227201,0.03159351,0.09187604,0.03704992,0.13811307,
               0.01478481,0.03634779,0.01630682,0.0124857,0.17693591,0.03328677,
               0.05556377,0.0323539,0.03625547,0.02468789,0.02368379,0.02222845,
               0.0263565,0.01119717,0.01252045,0.05720661,0.07207335)
lb = c('Gender','Customer Type','Age','Type of Travel','Flight Distance','Inflight wifi service',
           'Departure/Arrival time convenient',	'Ease of Online booking',	'Gate location',
           'Food and drink',	'Online boarding',	'Seat comfort',	
           'Inflight entertainment',	'On-board service',	'Leg room service',	'Baggage handling',
           'Checkin service',	'Inflight service',	'Cleanliness',	'Departure Delay in Minutes',
           'Arrival Delay in Minutes','Eco','Business')
barplot(importance,names = lb,horiz = T,col = "#CEDFEF",main = 'Importance of Random Forest',xlab = 'Importance')
