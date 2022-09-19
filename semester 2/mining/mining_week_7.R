install.packages("tensorflow")
library(tensorflow)
install_tensorflow()
install.packages("keras")
install_keras()

We first access the data set using the dataset_mnist function, load it and create variables
for the test and train data sets:
  library(keras); library(tensorflow)
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y
dim(x_train)
## [1] 60000 28 28
dim(x_test)
## [1] 10000 28 28
head(y_train)
## [1] 5 0 4 1 9 2

head(y_test)
## [1] 7 2 1 0 4 1
The x data is a three dimensional array (image, width, height) of grayscale values. To
prepare the data for training we convert the 3-d arrays into matrices by reshaping width and
height into a single dimension (28x28 images are flattened into length 784 vectors). Then,
we convert the grayscale values from integers ranging between 0 to 255 into floating point
values ranging between 0 and 1:
  # reshape
  x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255
The y data is an integer vector with values ranging from 0 to 9 (which refers to the handwritten digit). To prepare this data for training we one-hot encode the vectors into binary
class matrices using the to_categorical() function:
  y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)
head(y_train)
## [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
## [1,] 0 0 0 0 0 1 0 0 0 0
## [2,] 1 0 0 0 0 0 0 0 0 0
## [3,] 0 0 0 0 1 0 0 0 0 0
## [4,] 0 1 0 0 0 0 0 0 0 0
## [5,] 0 0 0 0 0 0 0 0 0 1
## [6,] 0 0 1 0 0 0 0 0 0 0
Defining the model
The core data structure of Keras is a model, a way to organise layers. The simplest type of
model is the Sequential model, a linear stack of layers.
We begin by creating a sequential model and then adding layers using the pipe (%>%) operator:

  model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10, activation = 'softmax')
The layer_dense is the regular fully connected layer, which implements the operation:
  output = activation(dot(input, weight) + bias). The number of nodes in the this layer is
specified by units. The activation function is specified by activation; some options include sigmoid, relu, softplus (a smooth approximation to ReLU). input_shape argument
to the first layer specifies the shape of the input data (a length 28 × 28 = 784 numeric vector representing a grayscale image). The final layer outputs a length 10 numeric vector
(probabilities for each digit) using a softmax activation function.
The layer_dropout consists in randomly setting a fraction rate of input units to 0 at each
update during training time, which helps prevent overfitting.
We can now use summary() to have a look at the parameters of the model:
  summary(model)
## Model: "sequential"
## ________________________________________________________________________________
## Layer (type) Output Shape Param #
## ================================================================================
## dense_2 (Dense) (None, 256) 200960
## ________________________________________________________________________________
## dropout_1 (Dropout) (None, 256) 0
## ________________________________________________________________________________
## dense_1 (Dense) (None, 128) 32896
## ________________________________________________________________________________
## dropout (Dropout) (None, 128) 0
## ________________________________________________________________________________
## dense (Dense) (None, 10) 1290
## ================================================================================
## Total params: 235,146
## Trainable params: 235,146
## Non-trainable params: 0
## ________________________________________________________________________________
So, the model has 235, 146 unknown parameters that need to be estimated.
We can now compile the model choosing an appropriate loss function (loss), optimisation
algorithm (optimizer), and performance evaluation metrics (metrics):

model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
#Training and evaluation
#Use the fit() function to train the model:
set.seed(1)
history <- model %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)  
  
#We can now evaluate the model’s performance on the test data:
model %>% evaluate(x_test, y_test)
## loss accuracy
## 0.11815 0.97860
and generate predictions:
  model %>% predict_classes(x_test) %>% head
## [1] 7 2 1 0 4 1

