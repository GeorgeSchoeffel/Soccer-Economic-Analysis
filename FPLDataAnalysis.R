#George Schoeffel
#Economics Project regarding willingness to spend for players in the Premier League

# Read CSV into R

setwd()
getwd()
fpl <- read.csv(file="FPL_data2.csv", header=TRUE, sep=",")

fpl$goalie = (fpl$Position == 'Goalie')
fpl$nongoalie = (fpl$Position != 'Goalie')
fpl$defender = (fpl$Position == 'Defender')
fpl$midfield = (fpl$Position == 'Midfielder')
fpl$forward = (fpl$Position == 'Forward')
fpl$costnew = (fpl$Cost * 10)

fpl$goalsqu = fpl$Goals^2

attach(fpl)

r1 <- lm(Cost ~ Goals, data=subset(fpl, nongoalie==TRUE))
summary(r1)

with(fpl[fpl$X > 1,], plot(Goals, Cost, pch='*', xlab = 'Goals in 2017', ylab = 'Cost', main = "Goals vs Cost in 2017"))

r2 <- lm(log(Cost) ~ log(Goals), data=subset(fpl, forward==TRUE))
summary(r2)

with(fpl[fpl$X > 3,], plot(Goals, Cost, pch='*', xlab = 'Goals in 2017', ylab = 'Cost', main = "Goals vs Cost in 2017 for Fowards"))

r3 <- lm(Cost ~ Goals, data=subset(fpl, defender==TRUE))
summary(r3)

fpl$ga = (fpl$Goals + fpl$Assists)

with(fpl[fpl$X > 1,], plot(ga, Cost, pch='*', xlab = 'Goals in 2017', ylab = 'Cost', main = "Goals vs Cost in 2017"))

with(fpl[fpl$X > 3,], plot(Cost, Goals))

with(fpl[(fpl$X < 3) & (fpl$X > 1),], plot(Cost, Goals))

fpl$goalssqu = (fpl$Goals^2)

r3 <- lm(Cost ~ Goals + goalssqu, data=subset(fpl, forward==TRUE))
summary(r3)

fpl$goalz = (fpl$Goals + 1)

r4 <- lm(log(costnew) ~ log(goalz), data=subset(fpl, forward==TRUE))
summary(r4)