library(ISLR2) # includes BrainCancer {ISLR2}	  Brain Cancer Data
str(BrainCancer)
head(BrainCancer)
summary(BrainCancer)

hist(BrainCancer$time)
hist(BrainCancer$time[BrainCancer$status==0])
hist(BrainCancer$time[BrainCancer$status==1])

table(BrainCancer$status)
#table(BrainCancer$time > 20, BrainCancer$status)
addmargins(table(BrainCancer$time > 20, BrainCancer$status))
length(BrainCancer$status)
sum(BrainCancer$status==0)
sum(BrainCancer$status==1)
sum(BrainCancer$time > 20)
sum(!BrainCancer$time > 20)
sum( (BrainCancer$status==0)&(BrainCancer$time > 20) )
sum( (BrainCancer$status==0)&(!BrainCancer$time > 20) )
sum(BrainCancer$time < 20)
sum( (BrainCancer$time < 20) & (BrainCancer$status==0) )


(BrainCancer$time < 20)


