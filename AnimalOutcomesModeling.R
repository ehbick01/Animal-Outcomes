# -------------------------------------------
# Louisville Animal Services Outcome Modeling
#
# Date : May 20, 2015
# -------------------------------------------

# Load Packages
library(openxlsx)
library(ggplot2)
library(AUC)
library(rpart)
library(rattle)
library(rpart.plot)
library(randomForest)
library(reshape2)
library(tidyr)
library(scales)
library(ggthemes)


# ------------
# Read in data
# ------------

df <- read.table(url('http://api.louisvilleky.gov/api/File/DownloadFile?fileName=AnimalServicesOutcomes.txt'), 
           sep='\t', quote=NULL, comment='', header=TRUE)

df <- as.data.frame(apply(df,2,function(x) gsub('UNKNOWN', '',x)))

# ----------------------------
# Build unique vars for inputs
# ----------------------------

# Build variables for animal type
dfType <- as.data.frame(df$Animal.Type)

for (i in 1:length(unique(dfType[,1]))) {
  
  dfType[,(1+i)] <- ifelse(dfType[,1] == unique(dfType[,1])[i], 1, 0)
  
  colnames(dfType)[(i+1)] <- as.character(unique(dfType[,1])[i])
  
}

df2 <- cbind(df, dfType[,-1])

# Build variables for age
dfAge <- as.data.frame(df$Estimated.Age)

for (i in 1:length(unique(dfAge[,1]))) {
  
  dfAge[,(1+i)] <- ifelse(dfAge[,1] == unique(dfAge[,1])[i], 1, 0)
  
  colnames(dfAge)[(i+1)] <- as.character(unique(dfAge[,1])[i])
  
}

colnames(dfAge) <- sapply(colnames(dfAge), function (x) ifelse(x[1] == '' | x[1] == 'UNKNOWN', 'Age Not Specified', x[1]))

df2 <- cbind(df2, dfAge[,-1])

# Build variables for gender
dfGender <- as.data.frame(df$Sex)

for (i in 1:length(unique(dfGender[,1]))) {
  
  dfGender[,(1+i)] <- ifelse(dfGender[,1] == unique(dfGender[,1])[i], 1, 0)
  
  colnames(dfGender)[(i+1)] <- as.character(unique(dfGender[,1])[i])
  
}

colnames(dfGender) <- sapply(colnames(dfGender), function (x) ifelse(x[1] == '' | x[1] == 'UNKNOWN', 'Gender Not Specified', x[1]))

df2 <- cbind(df2, dfGender[,-1])

# Build variables for size
dfSize <- as.data.frame(df$Size)

for (i in 1:length(unique(dfSize[,1]))) {
  
  dfSize[,(1+i)] <- ifelse(dfSize[,1] == unique(dfSize[,1])[i], 1, 0)
  
  colnames(dfSize)[(i+1)] <- as.character(unique(dfSize[,1])[i])
  
}

colnames(dfSize) <- sapply(colnames(dfSize), function (x) ifelse(x[1] == '' | x[1] == 'UNKNOWN', 'Size Not Specified', x[1]))

df2 <- cbind(df2, dfSize[,-1])

# Build variables for color
dfColor <- as.data.frame(df$Color)

for (i in 1:length(unique(dfColor[,1]))) {
  
  dfColor[,(1+i)] <- ifelse(dfColor[,1] == unique(dfColor[,1])[i], 1, 0)
  
  colnames(dfColor)[(i+1)] <- as.character(unique(dfColor[,1])[i])
  
}

colnames(dfColor) <- sapply(colnames(dfColor), function (x) ifelse(x[1] == '' | x[1] == 'UNKNOWN', 'Color Not Specified', x[1]))

df2 <- cbind(df2, dfColor[,-1])

# Build variables for intake type
dfIntake <- as.data.frame(df$IntakeType)

for (i in 1:length(unique(dfIntake[,1]))) {
  
  dfIntake[,(1+i)] <- ifelse(dfIntake[,1] == unique(dfIntake[,1])[i], 1, 0)
  
  colnames(dfIntake)[(i+1)] <- as.character(unique(dfIntake[,1])[i])
  
}

colnames(dfIntake) <- sapply(colnames(dfIntake), function (x) ifelse(x[1] == '' | x[1] == 'UNKNOWN', 'Intake Not Specified', x[1]))

df2 <- cbind(df2, dfIntake[,-1])

# Build variables for outcome type
dfOutcome <- as.data.frame(df$Outcome.Type)

for (i in 1:length(unique(dfOutcome[,1]))) {
  
  dfOutcome[,(1+i)] <- ifelse(dfOutcome[,1] == unique(dfOutcome[,1])[i], 1, 0)
  
  colnames(dfOutcome)[(i+1)] <- as.character(unique(dfOutcome[,1])[i])
  
}

colnames(dfOutcome) <- sapply(colnames(dfOutcome), function (x) ifelse(x[1] == '' | x[1] == 'UNKNOWN', 'Outcome Not Specified', x[1]))

df2 <- cbind(df2, dfOutcome[,-1])

# Build variables for outcome subtype
dfOutcomesub <- as.data.frame(df$Outcome.Subtype)

for (i in 1:length(unique(dfOutcomesub[,1]))) {
  
  dfOutcomesub[,(1+i)] <- ifelse(dfOutcomesub[,1] == unique(dfOutcomesub[,1])[i], 1, 0)
  
  colnames(dfOutcomesub)[(i+1)] <- as.character(unique(dfOutcomesub[,1])[i])
  
}

colnames(dfOutcomesub) <- sapply(colnames(dfOutcomesub), function (x) ifelse(x[1] == '' | x[1] == 'UNKNOWN', 'Subtype Not Specified', x[1]))

df2 <- cbind(df2, dfOutcomesub[,-1])

# Build variables for zip found
dfZip <- as.data.frame(df$Zip.Where.Found)

for (i in 1:length(unique(dfZip[,1]))) {
  
  dfZip[,(1+i)] <- ifelse(dfZip[,1] == unique(dfZip[,1])[i], 1, 0)
  
  colnames(dfZip)[(i+1)] <- as.character(unique(dfZip[,1])[i])
  
}

colnames(dfZip) <- sapply(colnames(dfZip), function (x) ifelse(x[1] == '' | x[1] == 'UNKNOWN', 'Zip Not Specified', x[1]))

df2 <- cbind(df2, dfZip[,-1])

# Build variables for length of stay
dfDays <- as.data.frame(as.numeric(as.Date(df$OutcomeDate) - as.Date(df$Intake.Date)))
colnames(dfDays) <- 'DaysIn'

df2 <- cbind(df2, dfDays)

# Build variable for pit/not pit
df2$Pit <- ifelse(grepl('PIT BULL', df2$Breed) == TRUE, 1, 0)

df3 <- df2[, -c(1:15)]

# ------------------------------
# Create basic descriptive plots
# ------------------------------

# Euthanasia by animal type
dfEuth <- subset(df3, Euthanized == 1 & Requested == 0) # Dataset for only euthanized animals (not requested)
names(dfEuth) <- gsub(' ', '', names(dfEuth))

type <- data.frame(Breed = gsub(' ', '', unique(df2$Animal.Type)))
type$Breed <- as.character(type$Breed)

for (i in 1:nrow(type)) { # Calculate number of animals euthanized by type
  
  type$Count[i] <- eval(
    parse(
      text = paste('sum(dfEuth$', type[i,1], ')', sep = '')
      )
    )
}

type <- type[order(-type[,2], type[,1]), ]

# Plot euthanasia rates by type
ggplot(type, aes(x = reorder(Breed, -Count), y = Count)) +
  geom_bar(stat = 'identity', fill = 'Dark Grey') +
  ggtitle('Euthanasia by Animal Type') +
  labs(x="Animal Type", y="Amount Euthanized") +
  theme_tufte(base_size = 10, base_family = 'MonoSans')
ggsave('Euthanasia-by-Type.PNG')

# Euthanasia as a share of total taken in
names(df2) <- gsub(' ', '', names(df2))

for (i in 1:nrow(type)) { # Calculate number of animals taken in by type
  
  type$Total[i] <- eval(
    parse(
      text = paste('sum(df2$', type[i,1], ')', sep = '')
    )
  )
}

type$Share <- 100*(type$Count/type$Total)

# Plot euthanasia shares by type
ggplot(type, aes(x = reorder(Breed, -Share), y = Share)) +
  geom_bar(stat = 'identity', fill = 'Dark Grey') +
  ggtitle('Euthanasia Share by Animal Type') +
  labs(x="Animal Type", y="Percent Euthanized") +
  theme_tufte(base_size = 10, base_family = 'MonoSans')
ggsave('Euthanasia-Share-by-Type.PNG')

# Visualizing cats euthanized
euthCat <- subset(dfEuth, CAT == 1)
names(euthCat) <- gsub('-', '', names(euthCat))

catAge <- data.frame(Age = names(euthCat[,8:12]))
catAge$Age <- gsub('-', '', catAge$Age)
catAge$Age <- as.character(catAge$Age)

for (i in 1:nrow(catAge)) { # Calculate number of animals euthanized by type
  
  catAge$Count[i] <- ifelse(catAge[i,1] != '', eval(
    parse(
      text = paste('sum(euthCat$', catAge[i,1], ')', sep = '')
    )
  ), '')
}

catAge <- catAge[order(-catAge[,2], catAge[,1]), ]

# Plot euthanasia rates by age for cats
ggplot(catAge, aes(x = reorder(Age, -Count), y = Count)) +
  geom_bar(stat = 'identity', fill = 'Dark Grey') +
  ggtitle('Euthanasia by Cat Age') +
  labs(x="Age", y="Amount Euthanized") +
  theme_tufte(base_size = 10, base_family = 'MonoSans')
ggsave('Euthanasia-by-Cat-Age.PNG')

# Euthanasia as a share of total taken in
df2$Estimated.Age <- gsub(' ', '', df2$Estimated.Age)
df2$Estimated.Age <- gsub('-', '', df2$Estimated.Age)
df2Cat <- subset(df2, Animal.Type == 'CAT')
names(df2Cat) <- gsub('-', '', names(df2Cat))

for (i in 1:nrow(catAge)) { # Calculate number of animals taken in by type
  
  catAge$Total[i] <- eval(
    parse(
      text = paste('sum(df2Cat$', catAge[i,1], ')', sep = '')
    )
  )
}

catAge$Share <- 100*(catAge$Count/catAge$Total)

# Plot euthanasia shares by cat age
ggplot(catAge, aes(x = reorder(Age, -Share), y = Share)) +
  geom_bar(stat = 'identity', fill = 'Dark Grey') +
  ggtitle('Euthanasia Share by Cat Age') +
  labs(x="Age", y="Percent Euthanized") +
  theme_tufte(base_size = 10, base_family = 'MonoSans')
ggsave('Euthanasia-Share-by-Cat-Age.PNG')

# Plot by outcome subtype
catSub <- data.frame(Sub = names(euthCat[,124:174]))
catSub$Sub <- gsub('-', '', catSub$Sub)
catSub$Sub  <- as.character(catSub$Sub)
catSub[9,1] <- 'ThirdParty' # Rename to ThirdParty
names(euthCat)[132] <- 'ThirdParty' # Rename to ThirdParty

for (i in 1:nrow(catSub)) { # Calculate number of animals euthanized by type
  
  catSub$Count[i] <- ifelse(catSub[i,1] != '', eval(
    parse(
      text = paste('sum(euthCat$', catSub[i,1], ')', sep = '')
    )
  ), '')
}

catSub <- catSub[order(-catSub[,2], catSub[,1]), ]

# Plot euthanasia rates subtype age for cats
ggplot(subset(catSub, Count > 0), aes(x = reorder(Sub, -Count), y = Count)) +
  geom_bar(stat = 'identity', fill = 'Dark Grey') +
  ggtitle('Euthanasia by Subtype in Cats') +
  labs(x="Outcome Subtype", y="Amount Euthanized") +
  theme_tufte(base_size = 10, base_family = 'MonoSans')
ggsave('Euthanasia-by-Cat-Subtype.PNG')

# Euthanasia as a share of total taken in
df2$Outcome.Subtype <- gsub(' ', '', df2$Outcome.Subtype)
df2$Outcome.Subtype <- gsub('-', '', df2$Outcome.Subtype)
df2Cat <- subset(df2, Animal.Type == 'CAT')
names(df2Cat) <- gsub('-', '', names(df2Cat))
names(df2Cat)[147] <- 'ThirdParty'
catSub[13,1] <- 'Breed.1' # DF2 has two breed columns

for (i in 1:nrow(catSub)) { # Calculate number of animals taken in by type
  
  catSub$Total[i] <- eval(
    parse(
      text = paste('sum(df2Cat$', catSub[i,1], ')', sep = '')
    )
  )
}

catSub$Share <- 100*(catSub$Count/catSub$Total)

# Plot euthanasia shares by dog age
ggplot(subset(catSub, Count >0), aes(x = reorder(Sub, -Share), y = Share)) +
  geom_bar(stat = 'identity', fill = 'Dark Grey') +
  ggtitle('Euthanasia Share by Subtype in Cats') +
  labs(x="Outcome Subtype", y="Percent Euthanized") +
  theme_tufte(base_size = 10, base_family = 'MonoSans')
ggsave('Euthanasia-Share-by-Cat-Subtype.PNG')

## Same plots for dogs

# Visualizing dogs euthanized
euthDog <- subset(dfEuth, DOG == 1)
names(euthDog) <- gsub('-', '', names(euthDog))

dogAge <- data.frame(Age = names(euthDog[,8:12]))
dogAge$Age <- gsub('-', '', dogAge$Age)
dogAge$Age <- as.character(dogAge$Age)

for (i in 1:nrow(dogAge)) { # Calculate number of animals euthanized by type
  
  dogAge$Count[i] <- ifelse(dogAge[i,1] != '', eval(
    parse(
      text = paste('sum(euthDog$', dogAge[i,1], ')', sep = '')
    )
  ), '')
}

dogAge <- dogAge[order(-dogAge[,2], dogAge[,1]), ]

# Plot euthanasia rates by age for dogs
ggplot(dogAge, aes(x = reorder(Age, -Count), y = Count)) +
  geom_bar(stat = 'identity', fill = 'Dark Grey') +
  ggtitle('Euthanasia by Dog Age') +
  labs(x="Age", y="Amount Euthanized") +
  theme_tufte(base_size = 10, base_family = 'MonoSans')
ggsave('Euthanasia-by-Dog-Age.PNG')

# Euthanasia as a share of total taken in
df2$Estimated.Age <- gsub(' ', '', df2$Estimated.Age)
df2$Estimated.Age <- gsub('-', '', df2$Estimated.Age)
df2Dog <- subset(df2, Animal.Type == 'DOG')
names(df2Dog) <- gsub('-', '', names(df2Dog))

for (i in 1:nrow(dogAge)) { # Calculate number of animals taken in by type
  
  dogAge$Total[i] <- eval(
    parse(
      text = paste('sum(df2Dog$', dogAge[i,1], ')', sep = '')
    )
  )
}

dogAge$Share <- 100*(dogAge$Count/dogAge$Total)

# Plot euthanasia shares by dog age
ggplot(dogAge, aes(x = reorder(Age, -Share), y = Share)) +
  geom_bar(stat = 'identity', fill = 'Dark Grey') +
  ggtitle('Euthanasia Share by Dog Age') +
  labs(x="Age", y="Percent Euthanized") +
  theme_tufte(base_size = 10, base_family = 'MonoSans')
ggsave('Euthanasia-Share-by-Dog-Age.PNG')

# Plot by outcome subtype
dogSub <- data.frame(Sub = names(euthDog[,124:174]))
dogSub$Sub <- gsub('-', '', dogSub$Sub)
dogSub$Sub  <- as.character(dogSub$Sub)
dogSub[9,1] <- 'ThirdParty'

for (i in 1:nrow(dogSub)) { # Calculate number of animals euthanized by type
  
  dogSub$Count[i] <- ifelse(dogSub[i,1] != '', eval(
    parse(
      text = paste('sum(euthDog$', dogSub[i,1], ')', sep = '')
    )
  ), '')
}

dogSub <- dogSub[order(-dogSub[,2], dogSub[,1]), ]

# Plot euthanasia rates subtype age for dogs
ggplot(subset(dogSub, Count > 0), aes(x = reorder(Sub, -Count), y = Count)) +
  geom_bar(stat = 'identity', fill = 'Dark Grey') +
  ggtitle('Euthanasia by Subtype in Dogs') +
  labs(x="Outcome Subtype", y="Amount Euthanized") +
  theme_tufte(base_size = 10, base_family = 'MonoSans')
ggsave('Euthanasia-by-Dog-Subtype.PNG')

# Euthanasia as a share of total taken in
df2$Outcome.Subtype <- gsub(' ', '', df2$Outcome.Subtype)
df2$Outcome.Subtype <- gsub('-', '', df2$Outcome.Subtype)
df2Dog <- subset(df2, Animal.Type == 'DOG')
names(df2Dog) <- gsub('-', '', names(df2Dog))
colnames(df2Dog)[147] <- 'ThirdParty'
dogSub[4,1] <- 'Breed.1' # DF2 has two breed columns

for (i in 1:nrow(dogSub)) { # Calculate number of animals taken in by type
  
  dogSub$Total[i] <- eval(
    parse(
      text = paste('sum(df2Dog$', dogSub[i,1], ')', sep = '')
    )
  )
}

dogSub$Share <- 100*(dogSub$Count/dogSub$Total)

# Plot euthanasia shares by dog age
ggplot(subset(dogSub, Count >0), aes(x = reorder(Sub, -Share), y = Share)) +
  geom_bar(stat = 'identity', fill = 'Dark Grey') +
  ggtitle('Euthanasia Share by Subtype in Dogs') +
  labs(x="Outcome Subtype", y="Percent Euthanized") +
  theme_tufte(base_size = 10, base_family = 'MonoSans')
ggsave('Euthanasia-Share-by-Dog-Subtype.PNG')


# -----------------------------------------------------------------------------------
# Building models using machine learning techniques, given the ~300 variables created
# -----------------------------------------------------------------------------------

# ----------------------------
# Create model for euthanasia
# ----------------------------

# Move Euthanized to front of dataset
moveMe <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}

df3 <- moveMe(df3, 'Euthanized', where = 'first')

# Set up train and test sets for dogs NOT requested by owner to be euthanized
df3 <- subset(df3, DOG == 1 & Requested == 0)
df3 <- df3[,-c(2:8, 106:827)] # Get rid of other animal types and other outcome types/zip codes

set.seed(111)
index <- sample(1:nrow(df3), size = 0.7*nrow(df3)) 
train <- df3[index,] 
test <- df3[-index,]

# Define base models using mean for euthanasia (average expectation)
baseEuth <- mean(df3$Euthanized)

# Base model evaluations
evalBase <- sqrt(mean((baseEuth - test$Euthanized)^2))
maeBase <- mean(abs((baseEuth - test$Euthanized)))

# Predict and evaluate against test set
finalBase = cbind(test$Euthanized, baseEuth)

# Calculate AUC and plot ROC curve
aucBase = auc(roc(as.numeric(finalBase[,2]), as.factor(finalBase[,1])))
plot(roc(as.numeric(finalBase[,2]), as.factor(finalBase[,1])))

# ------------------------
# Evaluating linear models
# ------------------------

# Starting with basic stepwise
null <- glm(Euthanized ~ 1, family = binomial(link = logit), data = train)
biggest <- formula(glm(Euthanized ~., family = binomial(link = logit), data = train))
glmModel <- step(null, direction = 'forward', scope = biggest)

# Grab coef table
coefs <- as.data.frame(glmModel$coefficients)
write.csv(coefs, 'linearCoefs.csv')

# Calculate odds for each factor (exponentiating the log odds)
expOdds <- as.data.frame((exp(glmModel$coefficients)/(1+exp(glmModel$coefficients))))
expOdds <- as.data.frame(format(exp(glmModel$coefficients), scientific = FALSE, digits = 2))
rows <- rownames(expOdds)
expOdds <- cbind(rows, expOdds)
colnames(expOdds) <- c('Variable', 'Odds')
expOdds[,2] <- sapply(expOdds[,2], function(x) as.numeric(as.character(x)))
expOdds <- expOdds[order(-expOdds[,2], expOdds[,1]),]
write.csv(expOdds, 'linearOdds.csv')

# Predict and evaluate against test set
predLinear = predict(glmModel, test, type = 'response')
finalLinear = cbind(test$Euthanized, predLinear)

# Calculate AUC and plot ROC curve
aucLinear = auc(roc(as.numeric(finalLinear[,2]), as.factor(finalLinear[,1])))
plot(roc(as.numeric(finalLinear[,2]), as.factor(finalLinear[,1])))

# -------------------------
# Evaluating decision trees
# -------------------------

# Sstarting with full tree
rt <- rpart(Euthanized ~., 
            data = train)

prp(rt,
    type = 4,
    main = "Euthanasia classification tree\n based on animal characteristics",
    clip.right.labs = FALSE, branch = 1, varlen = 0,
    cex = 0.5, cex.main = 0.5,
    box.col = "skyblue")

# Predict and evaluate against test set
predTree = predict(rt, test)
finalTree = cbind(test$Euthanized, predTree)

# Calculate AUC and plot ROC curve
aucTree = auc(roc(as.numeric(finalTree[,2]), as.factor(finalTree[,1])))
plot(roc(as.numeric(finalTree[,2]), as.factor(finalTree[,1])))

# Prune tree
printcp(rt)

# Find optimum Complexity Parameter (CP)
minXerror <- rt$cptable[which.min(rt$cptable[,'xerror']), 'CP']

# User CP to prune tree
rtPruned <- prune(rt, cp = minXerror)

prp(rtPruned,
    type = 4,
    main = "Euthanasia classification tree based on animal characteristics\nin Louisville Metro Animal Services",
    clip.right.labs = FALSE, branch = 1, varlen = 0,
    cex = 0.5, cex.main = 0.5,
    box.col = "skyblue")

# Predict and evaluate against test set
predPruned = predict(rtPruned, test)
finalPruned = cbind(test$Euthanized, predPruned)

# Calculate AUC and plot ROC curve
aucPruned = auc(roc(as.numeric(finalPruned[,2]), as.factor(finalPruned[,1])))
plot(roc(as.numeric(finalPruned[,2]), as.factor(finalPruned[,1])))

# ------------------------
# Evaluating random forest
# ------------------------

varNames <- paste('V', seq(1, ncol(train)-1, by = 1), sep ="")
matchTable <- cbind(colnames(train), c('Euthanized',varNames))
colnames(matchTable) <- c('Original', 'New')

colnames(train)[c(2:ncol(train))] <- varNames
colnames(test)[c(2:ncol(test))] <- varNames

# Create random forest with 1000 trees
rf <- randomForest(Euthanized ~., data = train, importance = TRUE, ntree = 100)

# Number of trees to to minimize mse
which.min(rf$mse)

# Plot to see estimated error as a function of the number of trees
plot(rf)

# Calculate the importance of each variable
imp <- as.data.frame(sort(importance(rf)[,1], decreasing = TRUE), optional = T)
imp <- cbind(rownames(imp), imp)
colnames(imp) <- c('New', '% Inc MSE')
imp <- merge(imp, matchTable, by = 'New')
imp <- imp[order(-imp[,2], imp[,1]),]

# Predict and evaluate against test set
predForest = predict(rf, test)
finalForest = cbind(test$Euthanized, predForest)

# Calculate AUC and plot ROC curve
aucForest = auc(roc(as.numeric(finalForest[,2]), as.factor(finalForest[,1])))
plot(roc(as.numeric(finalForest[,2]), as.factor(finalForest[,1])))

# Linear model off of factors that decrease MSE according to variable importance
train <- df3[index,] 
test <- df3[-index,] # Rebuild test and train sets for proper variable names

# Build variable set
xnam <- as.character(subset(imp, `% Inc MSE` > 0)$Original)
xnam <- sapply(xnam, function(x) paste('`', x, '`', sep =''))

# Build function and model
fmla <- as.formula(paste("Euthanized ~ ", paste(xnam, collapse= "+")))
linForest <- glm(fmla, family = binomial(link = logit), data = train)


# Grab coef table
coefs <- as.data.frame(linForest$coefficients)
write.csv(coefs, 'forestCoefs.csv')

# Calculate odds for each factor (exponentiating the log odds)
expOdds <- as.data.frame((exp(linForest$coefficients)/(1+exp(linForest$coefficients))))
expOdds <- as.data.frame(format(exp(linForest$coefficients), scientific = FALSE, digits = 2))
rows <- rownames(expOdds)
expOdds <- cbind(rows, expOdds)
colnames(expOdds) <- c('Variable', 'Odds')
expOdds[,2] <- sapply(expOdds[,2], function(x) as.numeric(as.character(x)))
expOdds <- expOdds[order(-expOdds[,2], expOdds[,1]),]
write.csv(expOdds, 'forestOdds.csv')

# Predict and evaluate against test set
predForestLinear = predict(linForest, test)
finalForestLinear = cbind(test$Euthanized, predForestLinear)

# Calculate AUC and plot ROC curve
aucForestLinear = auc(roc(as.numeric(finalForestLinear[,2]), as.factor(finalForestLinear[,1])))
plot(roc(as.numeric(finalForestLinear[,2]), as.factor(finalForestLinear[,1])))

# ----------------------------------
# Create table of evaluation metrics
# ----------------------------------

# Populate table with metrics
accuracy <- data.frame(Method = c('Base', 'Linear', 'Full Tree', 'Pruned Tree', 'Random Forest', 'Random Forest Linear'),
                            AUC = c(aucBase, aucLinear, aucTree, aucPruned, aucForest, aucForestLinear))

# Round metrics
accuracy$AUC <- round(accuracy$AUC, 2)

# ------------------------------------------
# Gather and plot all predictions vs actuals
# ------------------------------------------

# Create dataframe of all predictions
allPreds <- data.frame(actual = test$Euthanized, 
                       base = baseEuth, 
                       linear = predLinear, 
                       full.tree = predTree, 
                       pruned.tree = predPruned, 
                       random.forest = predForest)

# Build linear model using top 5 or 10 or something important factors from the randomForest model
# Build predictions into Shiny tool