
## This script supports the NBI power and sample size course
## January 2020
## George Savva (QIB)  george.savva@quadram.ac.uk

######################
##### TASK 3.1 #######
######################



######################
##### TASK 2 #########
######################

library(pwr)
help(pwr.t.test)

## Use the parameters below or calculate d based on your own estimate above.
## remember d is the ratio of the effect size to the outcome standard deviation
pwr.t.test(  n = NULL,
             d = 20/60,
             power=0.8,
             alternative = "greater")

## Changing the standard deviation to 30:
pwr.t.test(n = NULL,
             d = 20/30,
             power=0.8,
             alternative = "greater")

## What is the smallest d we could detect with only 10 per group?
pwr.t.test(n = 10,
           d = NULL,
           power=0.8,
           alternative = "greater")
## d=1.15 with 10 per group, which corresponds to a difference of 1.15*60= 69 units.

## What would the power be with 10 participants per group, to detect a difference of 50 in iAUC between breads?
pwr.t.test(n = 10,
           d = 50/56.56,
           power=NULL,
           alternative = "two.sided")

# How does this correspond to our power cal from simulation above?



#########################
#####  TASK 3.3  ########
#########################

results2 <- simUnpairedBreadExperiment(100)
results2

boxplot(iAUC ~ treatment, data=results2, col="green")
points(results2$treatment,results2$iAUC,pch=20)

t.test(iAUC ~ treatment, var.equal=TRUE, data=results2)
summary(lm(iAUC ~ treatment, data=results2))


### Demo, run this if you want to...
### Let's get the power to detect an effect size of 20 by simulating lots of experiments..
### This is the same code as earlier, but now we fix the effect size to 20.
### Also make sure we're running a one-sided test.

pvals <- replicate(1000, {
  results <- simUnpairedBreadExperiment(100,
                                        treatmenteffect = 20,
                                        betweenpatientsd = 40,
                                        withinpatientsd = 40)
  t.test(iAUC ~ treatment, var.equal=TRUE, data=results, alternative="greater")$p.value
})
mean(pvals<0.05)



######################
#####   TASK 3.4   ###
######################



## Here's our pilot study
results3 <- simPairedBreadExperiment(5)
t.test(results3$test, results3$control, paired = TRUE, alternative="greater")


results3$diff <- results3$test - results3$control

## This is sqrt(2) * standard deviation within participants
sd(results3$diff)

## Now do the power calculation
pwr.t.test(n=NULL,
           d = 20 / 56,
           power = 0.8,
           alternative="greater",
           type="paired")

## We will need about 50 participants

## Now lets estimate our effect size in our fully powered study:
results4 <- simPairedBreadExperiment(50)

t.test(results4$test, results4$control, paired = TRUE, alternative="greater")
