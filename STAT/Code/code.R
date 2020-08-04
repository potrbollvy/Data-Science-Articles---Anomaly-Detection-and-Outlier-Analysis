teaching <- read.csv("teaching.csv")

head(teaching)

############### Section 2

counts.by.group = aggregate(x = teaching$Grade, 
                         by = list(teaching$Group),
                         FUN = count)

counts.by.group = aggregate(x = teaching$Grade, 
                            by = list(teaching$Group),
                            FUN = length)

means.by.group = aggregate(x = teaching$Grade, 
  by = list(teaching$Group),
  FUN = mean)

variances.by.group = aggregate(x = teaching$Grade, 
                           by = list(teaching$Group),
                           FUN = var)

teaching.summary = merge(counts.by.group,
                         means.by.group,
                         variances.by.group,
                         by="Group.1")

teaching.summary = merge(merge(counts.by.group,
                         means.by.group,
                         by="Group.1"),
                         variances.by.group,
                         by="Group.1")

teaching.summary

colnames(teaching.summary) <- c("Group", 
                                "Sample Size", 
                                "Sample Mean", 
                                "Sample Variance")

teaching.summary

library(dplyr)
teaching.summary <- counts.by.group %>% 
                      full_join(means.by.group,
                                by="Group.1" )

teaching.summary

teaching.summary <- counts.by.group %>% 
                      full_join(means.by.group,
                                by="Group.1" ) %>%
                                  full_join(variances.by.group,
                                            by="Group.1" )
  
colnames(teaching.summary) <- c("Group", 
                                "Sample Size", 
                                "Sample Mean", 
                                "Sample Variance")

teaching.summary

N.A = teaching.summary[1,2]
N.B = teaching.summary[2,2]
N=N.A+N.B

y.bar.A = teaching.summary[1,3]
y.bar.B = teaching.summary[2,3]

S2.A = teaching.summary[1,4]
S2.B = teaching.summary[2,4]
S2.P = ((N.A-1)*S2.A+(N.A-1)*S2.B)/(N.A+N.B-2)

t0

alpha=0.05
t.star = qt(alpha,N-2)
t.star

t.star = qt(alpha,N-2, lower.tail=FALSE)
t.star

############### Section 3

(mu = mean(teaching$Grade))



plot(teaching$ID,teaching$Grade)
abline(h = mu)

plot(teaching$ID,teaching$Grade, col=teaching$Group, pch=teaching$Group)
plot(teaching$ID,teaching$Grade, 
     col=teaching$Group, 
     pch=as.numeric(teaching$Group))
plot(teaching$ID,teaching$Grade, 
     col=c("red","blue")[as.numeric(teaching$Group)], 
     pch=c(15,17)[as.numeric(teaching$Group)])
abline(h = mu)
abline(h = y.bar.B, col="blue", lwd=2, lty=2)
abline(h = y.bar.A, col="red", lwd=2, lty=2)
legend("topleft", legend=c("Group A", "Group B"),
       col=c("red", "blue"), pch=c(15,17), cex=0.8)

model.lm <- lm(Grade ~ Group, data = teaching)
SS.Table <- anova(model.lm)
SS.Table

summary(model.lm)
attributes(summary(model))
(R2 = summary(model)$r.squared)

plot(model.lm)

(B.T <- bartlett.test(Grade~Group, teaching))

install.packages("lawstat")
library(lawstat)
(L.T <- levene.test(teaching$Grade, teaching$Group, location="median", correction.method="zero.correction"))

# Section 4 -- will need a multi-variate dataset (perhaps a subset of the algae blooms data)
# GLM
# ANOVA for GLM
# Something about having model with ordinal variables and dummy variables, to see the effect it has on the ANOVA tables
# t-test
# Bonferroni
# Assumptions on residuals
# outliers and influential points? 
# multicolinearity and variance inflation factors
# look in applied regression textbook for examples, too? 


# Section 5 -- best subset  selection (see MAT3373 HW2)
# 

# Section 6 -- Hotelling stuff
# Bonferroni
# generating samples from a multivariate distribution using the Cholesky decomposition in R

# Section 7 -- MANOVA + Wilk's Lambda
# https://sites.ualberta.ca/~lkgray/uploads/7/3/6/2/7362679/22_-_manova.pdf

# Section 8 -- goodness of fit stuff, can probably use the actual example in the notes? 

# Section 9 -- ANCOVA in R (some other example?)
# Paired comparison (MAT 2377)
# ANCOVA itself? 

#Section 10 -- something about  Nonlinear Regression