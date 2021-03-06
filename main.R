# install packages if it is not already
if(!require("data.table"))
{
	install.packages("data.table")
}
if(!require("ggplot2"))
{
  install.packages("ggplot2")
}
if(!require("caTools"))
{
  install.packages("caTools")
}
if(!require("tensorflow"))
{
  install.packages("tensorflow")
}

# attach all functions provided by these packages
library(data.table)
library(ggplot2)
library(caTools)
library(keras)

#download spam data set to local directory, if it is not present
if(!file.exists("spam.data"))
{
	download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data", "spam.data")
}

# Read spam data set and convert to input matrix
spam.dt <- data.table::fread("spam.data")
N.obs <- nrow(spam.dt)
X.raw <- as.matrix(spam.dt[, -ncol(spam.dt), with=FALSE])
y.vec <- spam.dt[[ncol(spam.dt)]]
X.mat <- scale(X.raw)

max.epochs <- 100
test.epochs <- 1
step.size <- 0.05
n.hidden.units <- c(ncol(X.mat), 20, 1)

# Create vector with size = num observations in the whole data set
# elements are TRUE if observation is in train set, FALSE otherwise. Should be 80T-20F
# is.train contains rows which are in the train set. All other rows not in is.train are in the is.test set
is.train <- sample(1 : nrow(X.mat), .8 * nrow(X.mat), replace = F)
is.test <- setdiff(1:nrow(X.mat), is.train)


# create vector with size = num observations in the whole data set
# elements are TRUE if observation is in train set, FALSE otherwise. Should be 60T-40F 
# is.subtrain contains rows which are in the train set. All other rows not in is.subtrain are in validation set
is.subtrain <- sample(1 : (length(is.train)), (.6 * length(is.train)), replace = F)
validation <- setdiff(1:length(is.train), is.subtrain)

# TODO: Define 3 neural networks in keras (sequential model with 1 dense layer)
model <- keras_model_sequential()

# NN1: 10 hidden units, 1 hidden layer
NN1 <- function(test.epochs)
{
model %>%
	layer_flatten(input_shape = ncol(X.mat)) %>% #input layer
	layer_dense(units = 1, activation = 'sigmoid') %>% #hidden layer
	layer_dense(10, activation = 'softmax') #output layer
	
model %>% compile(
	loss = "binary_crossentropy",
	optimizer = "none",
	metrics = c('accuracy'))
	
model %>% fit(
	x = x.train, y = y.train,
	epochs = 5, validation_split = 0,
	verbose = 2
	)
}
	
# NN2: 100 hidden units, 1 hidden layer
NN2 <- function(test.epochs)
{
model %>%
	layer_flatten(input_shape = ncol(X.mat)) %>% #input layer
	layer_dense(units = 10, activation = 'sigmoid') %>%#hidden layer
	layer_dense(10, activation = 'softmax') #output layer
	
model %>% compile(
	loss = "binary_crossentropy",
	optimizer = 'none',
	metrics = c('accuracy'))
	
model %>% fit(
	x = x.train, y = y.train,
	epochs = 5, validation_split = 0,
	verbose = 2
	)
}
	
# NN3: 1000 hidden units, 1 hidden layer
NN3 <- function(test.epochs)
{
model %>%
	layer_flatten(input_shape = ncol(X.mat)) %>% #input layer
	layer_dense(units = 100, activation = 'sigmoid') %>% #hidden layer
	layer_dense(10, activation = 'softmax') #output layer
	
model %>% compile(
	loss = "binary_crossentropy",
	optimizer = 'none',
	metrics = c('accuracy'))
	
model %>% fit(
	x = x.train, y = y.train,
	epochs = 5, validation_split = 0,
	verbose = 2
	)	
}

# Plot log loss as fxn of num epochs (different colors for each # hidden units), use different linetype for sets (subtrain = solid, validation = dashed). Draw a point to emphasize min of each validation loss
# These different lines must be on the same plot
# 		Note: log loss is BinaryCrossentropy in keras
#		Note: step size is the same as learning rate in keras
# Plot subtrain/validation loss as fxn of num epochs, draw point to emphasize min validation loss
weight.dt <- data.frame(do.call(rbind, stuff[2]))
layer.i <- stuff[[4]]
ggplot()+
  facet_grid(. ~ layer.i, labeller=label_both)+
  geom_tile(aes(
    x=input, y=output, fill=weight),
    data=weight.dt)

# define best_epochs for each NN
# best_epochs = epochs to minimize validation loss
best.epochs <- 10

# Retrain each NN on entire train set. Use corresponding best_epochs
NN1(best.epochs)
NN2(best.epochs)
NN3(best.epochs)

# Use these new learned models to compute predictions. What is the prediction accuracy? What is the accuracy of the baseline model which predicts the most frequent class in the trian labels?
y.tab <- table(is.train)
y.pred <- as.integer(names(y.tab[which.max(y.tab)]))
accuracy <- mean(is.subtrain == y.pred)
