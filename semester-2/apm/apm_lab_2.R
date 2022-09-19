#APM_lab2
email <- read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/email.csv"), header=TRUE)
head(email)

e <- email
e$cc <- ifelse(email$cc > 0, 1, 0)
e$attach <- ifelse(email$attach > 0, 1, 0)
e$dollar <- ifelse(email$dollar > 0, 1, 0)
e$inherit <- ifelse(email$inherit > 0, 1, 0)
e$password <- ifelse(email$password > 0, 1, 0)

g <- glm(spam ~ to_multiple + winner + format + re_subj + exclaim_subj +
           attach + dollar + inherit + password,
         data = e, family = binomial)
summary(g)

p <- predict(g, type = "response")
p. <- p

set.seed(1)
noise <- rnorm(nrow(e), sd = 0.08)
plot(p, e$spam + noise,
     xlim = 0:1,
     ylim = c(-0.5, 1.5),
     axes = FALSE,
     xlab = "Predicted probability",
     ylab = "",
     col="grey",
     pch = 20)
axis(1)
axis(2,
     at = c(0,1),
     labels = c("0 (not spam)", "1 (spam)"))

#Our interest here is to predict whether an email is spam or not as accurately as possible. To be able to assess
#accuracy, we can split the data into a training and test set, fit a model to the training set and then see how
#well it predicts the response for the data in the test set.
#Let’s start by splitting the data into training (60%) and test set (40%). Setting the seed at the beginning
#ensures reproducibility

#Let’s start by splitting the data into training (60%) and test set (40%). Setting the seed at the beginning
#ensures reproducibility.
set.seed(007)
smp_size <- floor(0.6 * nrow(e))
index <- sample(seq_len(nrow(e)),size=smp_size)
?sample
train <- e[index, ]
test <- e[-index, ]

m <- glm(spam ~ to_multiple + winner + format + re_subj + exclaim_subj +
           attach + dollar + inherit + password,
         data = train, family = binomial)
summary(m)

results_prob <- predict(m,newdata=test,type = "response")

results <- ifelse(results_prob > 0.75,1,0)

answers <- test$spam

misClassificError <- mean(answers != results)
misClassificError

acc <- 1-misClassificError
acc

#Let us look at how many false positive and false negative answers we get.
all <- data.frame(answers=answers, results=results)
trueneg <- all[answers==0,]
dim(trueneg)

truepos <- all[answers==1,]
dim(truepos)

fpr <- mean(trueneg$answers!=trueneg$results)
fpr

fnr <- mean(truepos$answers!=truepos$results)
fnr

# 0 out of 1426 non-spam emails were classified as spam
length(truepos$answers)

sum(truepos$results)
# 0 out of 143 spam emails were classified as spam

library(ROCR)
# ROC and AUC
p <- predict(m, test, type="response")
pr <- prediction(p, test$spam)
# TPR = sensitivity = true positive rate, FPR=1-specificity = false positive rate
prf<- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

