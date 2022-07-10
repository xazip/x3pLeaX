#' Create CNN architecture
#'
#' @param Learning_rate learning rate speed
#' @param decay weighted decay rate
#' @return CNN model as returned from keras
#' @import keras
#' @export
create_residual_3dCNN <- function(Learning_rate = 0.0001
                                  #, decay = FALSE, momentum = 0.9
                                  ){

  k_clear_session()
  tensorflow::tf$random$set_seed(777777)

  input <- keras::layer_input(shape = c(128, 128, 1, 1))

  skip_1 <- input %>% #-----------------------------------------------------------------------------> First skip connection being usec
    keras::layer_conv_3d(filters = 32, kernel_size = c(7, 7, 1), strides = c(2, 2, 1), padding = "same") %>% #Layer 2
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_max_pooling_3d(pool_size = c(2, 2, 1))

  output_1 <- skip_1 %>%
    keras::layer_conv_3d(filters = 32, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% #Layer 2
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_conv_3d(filters = 32, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% #Layer 3
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu")

  output_2 <- keras::layer_add(list(output_1, skip_1)) %>%
    keras::layer_conv_3d(filters = 32, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same")%>% #Layer 4
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_conv_3d(filters = 32, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 5
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu")

  output_3 <- keras::layer_add(list(output_2, output_1)) %>%
    keras::layer_conv_3d(filters = 32, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same")%>% # Layer 6
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_conv_3d(filters = 32, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 7
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu")

  output_4 <- keras::layer_add(list(output_3, output_2)) %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(3, 3, 1), strides = c(2, 2, 1), padding = "same") %>% # Layer 8
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # layer 9
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu")

  skip_2 <- output_3 %>% #-----------------------------------------------------------------------------> feature_maps differ,so we perform a projection using 1x1 convolution
    keras::layer_conv_3d(filters = 64, kernel_size = 1, strides = c(2,2,1), padding = "same") %>% # Layer 10
    keras::layer_batch_normalization(center = TRUE, scale = TRUE)


  output_5 <- keras::layer_add(list(output_4, skip_2)) %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 11
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 12
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu")

  output_6 <- keras::layer_add(list(output_5, output_4)) %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 13
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 14
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu")

  output_7 <- keras::layer_add(list(output_6, output_5)) %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 15
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 16
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu")

  output_8 <- keras::layer_add(list(output_7, output_6)) %>%
    keras::layer_conv_3d(filters = 128, kernel_size = c(3, 3, 1), strides = c(2, 2, 1), padding = "same") %>% # Layer 17
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_conv_3d(filters = 128, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 18
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu")

  skip_3 <- output_7 %>% #-----------------------------------------------------------------------------> feature_maps differ,so we perform a projection using 1x1 convolution
    keras::layer_conv_3d(filters = 128, kernel_size = 1, strides = c(2, 2, 1), padding = "same") %>% # Layer 19
    keras::layer_batch_normalization(center = TRUE, scale = TRUE)

  output_9 <- keras::layer_add(list(output_8, skip_3)) %>%
    keras::layer_conv_3d(filters = 128, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 20
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_conv_3d(filters = 128, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 21
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu")

  output_10 <- keras::layer_add(list(output_9, output_8)) %>%
    keras::layer_conv_3d(filters = 128, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 22
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_conv_3d(filters = 128, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 23
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu")

  output_11 <- keras::layer_add(list(output_10, output_9)) %>%
    keras::layer_conv_3d(filters = 256, kernel_size = c(3, 3, 1), strides = c(2, 2, 1), padding = "same") %>% # Layer 24
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_conv_3d(filters = 256, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 25
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu")

  skip_4 <- output_10 %>%
    keras::layer_conv_3d(filters = 256, kernel_size = 1, strides = c(2, 2, 1), padding = "same") %>% # Layer 26
    keras::layer_batch_normalization(center = TRUE, scale = TRUE)

  output_12 <- keras::layer_add(list(output_11, skip_4)) %>%
    keras::layer_conv_3d(filters = 256, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 27
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_conv_3d(filters = 256, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same") %>% # Layer 28
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu")

  output <- output_12 %>%
    keras::layer_global_average_pooling_3d() %>%
    keras::layer_flatten() %>%
    keras::layer_dense(units = 512) %>% #Layer 29
    keras::layer_batch_normalization(center = TRUE, scale = TRUE) %>%
    keras::layer_activation("relu") %>%
    keras::layer_dense(units = 5, activation = "softmax") # Layer 30



  cnn_resNet_30 <- keras::keras_model(input, output)

  cnn_resNet_30 %>% keras::compile(loss = "categorical_crossentropy",
                            optimizer = optimizer_sgd(lr = Learning_rate#, 
                                                      #decay = decay, momentum = momentum
                                                      ),
                            metrics = "accuracy")

  return(cnn_resNet_30)

}


##############################


#Dynamic Residual Network
#This function allows a user to create a 3d convolutional residual network
#The network can be as deep as the user wants
#The user is also able to create the Residual_X architecture by changing the inputScalarSize and the cardinality

#inputImageSize, the size of your input tensor
#inputScalerSize, scale parameter of your input shape, we do not use this for our project
#numberOfClassificationLabels, the number of labels we are trying to predict
#layers, the amount of residual blocks
#residualBlockSchedule, the number of sub residual blocks
#lowestResolution, the smallest filter size
#mode, classification or regression, regression has not been implemented, classification assumes non-binary problem

createResNetModel3D <- function(inputImageSize,
                                inputScalarsSize = 0,
                                numberOfClassificationLabels = 6,
                                layers = 1:4,
                                residualBlockSchedule = c(3, 4, 6, 3),
                                lowestResolution = 64,
                                cardinality = 1,
                                mode = c('classification')){
  
  addCommonLayers <- function(model){
    model <- model %>% layer_batch_normalization()
    model <- model %>% layer_activation_leaky_relu()
    return(model)
  }
  
  groupedConvolutionLayer3D <- function(model, numberOfFilters, strides){
    
    # Per standard ResNet, this is just a 3-D convolution
    if(cardinality == 1){
      groupedModel <- model %>% layer_conv_3d(filters = numberOfFilters,
                                              kernel_size = c(3, 3, 1),
                                              strides = strides,
                                              padding = 'same')
      return(groupedModel)
    }
    
    if(numberOfFilters %% cardinality != 0)
    {
      stop("Possible Filter Issue")
    }
    
    numberOfGroupFilters <- as.integer(numberOfFilters / cardinality)
    convolutionLayers <- list()
    
    
    Slice(keras$layers$Layer) %py_class%{
      initialize <- function(begin, end){
        super$initialize()
        self$begin <- begin
        self$end <- end
        
      }
      
      call <- function(inputs){
        
        tf$strided_slice(inputs, self$begin, self$end)
      }
      
      get_config <- function(){
        list(begin = self$begin$numpy(),
             end = self$end$numpy())
      }
    }
    
    layer_slice <- create_layer_wrapper(Slice)
    
    
    for(j in 1:cardinality){
      
      dim_1 <- as.integer(model$get_shape()[1])
      dim_2 <- as.integer(model$get_shape()[2])
      dim_3 <- as.integer(model$get_shape()[3])
      dim_4 <- as.integer(model$get_shape()[4])
      
      dim_5_1 <- as.integer(( j - 1 ) * numberOfGroupFilters)
      dim_5_2 <- as.integer( j * numberOfGroupFilters )
      k_set_image_data_format('channels_last')
      
      
      #layer_slice <- create_layer_wrapper(layer_slice)
      convolutionLayers[[j]] <- model %>% layer_slice(begin = tf$constant(list(as.integer(0), as.integer(0), as.integer(0), as.integer(0), dim_5_1)),
                                                      end = tf$constant(list(as.integer(1), dim_2, dim_3, dim_4, dim_5_2)))
      
      convolutionLayers[[j]] <- convolutionLayers[[j]] %>%
        layer_conv_3d(filters = numberOfGroupFilters,
                      kernel_size = c(3, 3, 1),
                      strides = strides,
                      padding = 'same')
    }
    groupedModel <- layer_concatenate(convolutionLayers)
    
    return(groupedModel)
  }
  
  residualBlock3D <- function(model, 
                              numberOfFiltersIn,
                              numberOfFiltersOut,
                              strides = c(1, 1, 1),
                              project = FALSE){
    shortcut <- model
    
    model <- model %>% layer_conv_3d(filters = numberOfFiltersIn,
                                     kernel_size = c(1, 1, 1),
                                     strides = c(1, 1, 1),
                                     padding = 'same')
    
    model <- addCommonLayers(model)
    
    # ResNeXt (identical to ResNet when `cardinality` == 1)
    model <- groupedConvolutionLayer3D(model,
                                       numberOfFilters = numberOfFiltersIn,
                                       strides = strides)
    model <- addCommonLayers(model)
    
    model <- model %>% layer_conv_3d(filters = numberOfFiltersOut,
                                     kernel_size = c(1, 1, 1),
                                     strides = c(1, 1, 1),
                                     padding = 'same')
    
    model <- model %>% layer_batch_normalization()
    
    if(project == TRUE || prod(strides == c(1, 1, 1)) == 0)
    {
      shortcut <- shortcut %>% layer_conv_3d(filters = numberOfFiltersOut,
                                             kernel_size = c(1, 1, 1),
                                             strides = strides,
                                             padding = 'same')
      
      shortcut <- shortcut %>% layer_batch_normalization()
    }
    
    model <- layer_add(list( shortcut, model ))
    
    model <- model %>% layer_activation_leaky_relu()
    
    return(model)
  }
  
  mode <- match.arg(mode)
  
  inputImage <- layer_input(shape = inputImageSize)
  
  nFilters <- lowestResolution
  
  outputs <- inputImage %>% layer_conv_3d(filters = nFilters,
                                          kernel_size = c(7, 7, 1),
                                          strides = c(2, 2, 1),
                                          padding = 'same')
  
  outputs <- addCommonLayers(outputs)
  
  outputs <- outputs %>% layer_max_pooling_3d(pool_size = c(3, 3, 1),
                                              strides = c(2, 2, 1),
                                              padding = 'same')
  
  for(i in seq_len(length(layers))){
    
    nFiltersIn <- lowestResolution * 2 ^ (layers[i])
    
    nFiltersOut <- 2 * nFiltersIn
    
    for(j in seq_len(residualBlockSchedule[i])){
      
      project <- FALSE
      
      if(i == 1 && j == 1){
        
        project <- TRUE
        
      }
      if(i > 1 && j == 1){
        strides <- c(2, 2, 1)
      } else {
        strides <- c(1, 1, 1)
      }
      outputs <- residualBlock3D(outputs,
                                 numberOfFiltersIn = nFiltersIn,
                                 numberOfFiltersOut = nFiltersOut,
                                 strides = strides,
                                 project = project)
    }
  }
  outputs <- outputs %>% layer_global_average_pooling_3d()
  
  layerActivation <- ''
  if(mode == 'classification'){
    
    layerActivation <- 'softmax'
    
  } else {
    
    stop('Regression is not implemented in this function, please use Classification or leave... lol')
  }
  
  resNetModel <- NULL
  
  if(inputScalarsSize > 0){
    
    inputScalars <- layer_input(shape = c(inputScalarsSize))
    
    concatenatedLayer <- layer_concatenate(list(outputs, inputScalars))
    
    outputs <- concatenatedLayer %>%
      layer_dense(units = numberOfClassificationLabels, activation = layerActivation)
    
    resNetModel <- keras_model(inputs = list(inputImage, inputScalars), outputs = outputs)
    
  } else {
    outputs <- outputs %>%
      layer_flatten() %>%
      layer_dense(units = numberOfClassificationLabels, activation = layerActivation)
    resNetModel <- keras_model(inputs = inputImage, outputs = outputs)
  }
  
  return(resNetModel)
}


model <- createResNetModel3D(inputImageSize = c(128, 128, 1, 1),
                             inputScalarsSize = 0,
                             numberOfClassificationLabels = 6,
                             layers = 1:4,
                             residualBlockSchedule = c(3, 4, 6, 3),
                             lowestResolution = 64,
                             cardinality = 32,
                             mode = 'classification')  







#######################################################


strategy <- tensorflow::tf$distribute$MirroredStrategy()

with(strategy$scope(),{
  
  input <- keras::layer_input(shape = c(128, 128, 1, 1))
  
  step_1 <- input %>% 
    keras::layer_conv_3d(filters = 64, kernel_size = c(7, 7, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_2 <- step_1 %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu") 
  
  step_3 <- step_2 %>%
    keras::layer_max_pooling_3d(pool_size = c(2, 2, 1))
  
  step_4 <- step_3 %>%
    keras::layer_conv_3d(filters = 128, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_5 <- step_4 %>%
    keras::layer_conv_3d(filters = 128, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_6 <- step_5 %>%
    keras::layer_max_pooling_3d(pool_size = c(2, 2, 1))
  
  step_7 <- step_6 %>%
    keras::layer_conv_3d(filters = 256, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_8 <- step_7 %>%
    keras::layer_conv_3d(filters = 256, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_9 <- step_8 %>%
    keras::layer_max_pooling_3d(pool_size = c(2, 2, 1))
  
  step_10 <- step_9 %>%
    keras::layer_conv_3d(filters = 512, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_11 <- step_10 %>%
    keras::layer_conv_3d(filters = 512, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_12 <- step_11 %>%
    keras::layer_max_pooling_3d(pool_size = c(2, 2, 1))
  
  step_13 <- step_12 %>%
    keras::layer_conv_3d(filters = 1024, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_14 <- step_13 %>%
    keras::layer_conv_3d(filters = 1024, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_15 <- step_14 %>%
    keras::layer_max_pooling_3d(pool_size = c(2, 2, 1))
  
  step_16 <- step_15 %>%
    keras::layer_spatial_dropout_3d(rate = 0.5)
  
  step_17 <- step_16 %>%
    keras::layer_conv_3d(filters = 2048, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_18 <- step_17 %>%
    keras::layer_conv_3d(filters = 2048, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  
  step_19 <- step_18 %>%
    keras::layer_conv_3d_transpose(filters = 1024, kernel_size = c(1, 1, 1), strides = c(2, 2, 1))
  
  step_20 <- keras::layer_concatenate(list(step_14, step_19))                                                                             # End of "Encoder"
  #---------------------------------------------------------------------------------------------------------------------------------------# Start of Decoder...... Skip Connection 1
  step_21 <- step_20 %>%
    keras::layer_conv_3d(filters = 1024, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_22 <- step_21 %>%
    keras::layer_conv_3d(filters = 1024, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  
  step_23 <- step_22 %>%
    keras::layer_conv_3d_transpose(filters = 512, kernel_size = c(1, 1, 1), strides = c(2, 2, 1))
  
  step_24 <- keras::layer_concatenate(list(step_11, step_23))
  
  #-----------------------------------------------------------------------------------------------------------------------------------------#  Skip Connection 2
  step_25 <- step_24 %>%
    keras::layer_conv_3d(filters = 512, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_26 <- step_25 %>%
    keras::layer_conv_3d(filters = 512, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  
  step_27 <- step_26 %>%
    keras::layer_conv_3d_transpose(filters = 256, kernel_size = c(1, 1, 1), strides = c(2, 2, 1))
  
  step_28 <- keras::layer_concatenate(list(step_8, step_27))
  
  #-----------------------------------------------------------------------------------------------------------------------------------------# Skip Connection 3
  
  
  step_29 <- step_28 %>%
    keras::layer_conv_3d(filters = 256, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_30 <- step_29 %>%
    keras::layer_conv_3d(filters = 256, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  
  step_31 <- step_30 %>%
    keras::layer_conv_3d_transpose(filters = 128, kernel_size = c(1, 1, 1), strides = c(2, 2, 1))
  
  step_32 <- keras::layer_concatenate(list(step_5, step_31))
  
  
  #-----------------------------------------------------------------------------------------------------------------------------------------# Skip Connection 4
  
  
  step_33 <- step_32 %>%
    keras::layer_conv_3d(filters = 128, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  step_34 <- step_33 %>%
    keras::layer_conv_3d(filters = 128, kernel_size = c(3, 3, 1), strides = c(1, 1, 1), padding = "same", activation = "relu")
  
  
  step_35 <- step_34 %>%
    keras::layer_conv_3d_transpose(filters = 64, kernel_size = c(1, 1, 1), strides = c(2, 2, 1))
  
  step_36 <- keras::layer_concatenate(list(step_2, step_35))                                                                              # Skip Connection 5
  
  step_37 <- step_36 %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(1, 1, 1), strides = c(1, 1, 1),activation = "relu")
  
  step_38 <- step_37 %>%
    keras::layer_conv_3d(filters = 64, kernel_size = c(1, 1, 1), strides = c(1, 1, 1),activation = "relu")
  
  step_39 <- step_38 %>%
    keras::layer_conv_3d(filters = 6, kernel_size = c(1, 1, 1), strides = c(1, 1, 1), activation = "softmax")
  
  
  output <- step_39
  model <- keras::keras_model(input = input, output = output)
  

  
})

######################################################

create_residual_network_cnn <- function(input_dim, #input_shape, 1D, 2D, and 3D CNN's, 
                                        #Channel = "last" will be used
                                        #Sample size excluded is Implied
                                        
                                        number_of_classifications = 6, #Number of labels to be classified.
                                        
                                        blocks = 1:4, #indexes for custom block designs
                                        
                                        blockSchedule_corresponding_to_layers = c(3, 4, 6, 3), #blockSchedule based on skip connections, specifically a identity skip
                                        #returns the same value that was used as its argument
                                        #Specifically, Concatenation, Addition, Subtraction, Multiplication
                                        block_version = c("version_1", "version_2"),
                                        
                                        minimum_filter_size = 64, #minimum filter sizes used for downward architectures
                                        
                                        cardinality = 1, #cardinality, specifying the number of independent paths
                                        
                                        problem_type = c("classification") #Type of supervised prediction problem
                                        #Type will determine the cost function to use
                                        
)
{
  
  operational_layers <- function(model, #model object
                                 
                                 normalization = TRUE, #Whether to use batch normalization or not
                                 non_linear_activation = c("elu", 
                                                           
                                                           #Exponential Linear Unit or its widely known name ELU is a function that tend to converge cost to zero faster and produce more accurate results. 
                                                           #Different to other activation functions, ELU has a extra alpha constant which should be positive number.
                                                           #ELU is very similiar to RELU except negative inputs. They are both in identity function form for non-negative inputs.
                                                           #On the other hand, ELU becomes smooth slowly until its output equal to -Î± whereas RELU sharply smoothes.
                                                           
                                                           "relu",
                                                           
                                                           #Rectified Linear Units. The formula is deceptively simple: max(0,z). 
                                                           #Despite its name and appearance, it's not linear and provides the same benefits as Sigmoid
                                                           #(i.e. the ability to learn nonlinear functions), but with better performance.
                                                           
                                                           "leaky_relu",
                                                           
                                                           #LeakyRelu is a variant of ReLU. 
                                                           #Instead of being 0 when z<0, a leaky ReLU allows a small, non-zero, constant gradient Î± (Normally, Î±=0.01). 
                                                           #However, the consistency of the benefit across tasks is presently unclear. 
                                                           
                                                           "para_relu",
                                                           
                                                           #Similar to relu but the alpha parameter is learned during training instead of being a static argument
                                                           
                                                           "sigmoid",
                                                           
                                                           #Sigmoid takes a real value as input and outputs another value between 0 and 1.
                                                           #It's easy to work with and has all the nice properties of activation functions:
                                                           #it's non-linear, continuously differentiable, monotonic, and has a fixed output range.
                                                           
                                                           "tanh"
                                                           
                                                           #Tanh squashes a real-valued number to the range [-1, 1].
                                                           #It's non-linear. But unlike Sigmoid, its output is zero-centered.
                                                           #Therefore, in practice the tanh non-linearity is always preferred to the sigmoid nonlinearity. 
                                                           
                                                           
                                                           #none => no activation function
                                                           
                                                           #Consideration for which activation to use should be based on the values in the input data
                                                           #and how closely those values follow a specific distribution 
                                 )
  ){
    if(normalization == TRUE){
      
      model <- model %>% keras::layer_batch_normalization() #> ----------------
      #|
      #|  #each activation function has specific arguments for fine tuning 
      #|  #further consideration for user specifics
      #|
      #> ----------------
      
    }
    
    if(non_linear_activation == "elu"){
      model <- model %>% keras::layer_activation_elu()
    } else if(non_linear_activation == "relu"){
      model <- model %>% keras::layer_activation_relu()
    } else if(non_linear_activation == "leaky_relu"){
      model <- model %>% keras::layer_activation_leaky_relu()
    } else if(non_linear_activation == "para_relu"){
      model <- model %>% keras::layer_activation_parametric_relu()
    } else if(non_linear_activation == "sigmoid"){
      model <- model %>% keras::layer_activation(activation = "sigmoid")
    } else if(non_linear_activation == "tanh"){
      model <- model %>% keras::layer_activation(activation = "tanh")
    }
    
    return(model)   #> ----------------
    #|
    #|  #each activation function has specific arguments for fine tuning 
    #|  #further consideration for user specifics
    #|
    #> ----------------
    
    
  } #End of Function [1]
  
  
  convolution_adjustment <- function(model,
                                     Filters,
                                     Strides){
    
    if(cardinality == 1){
      adjusted_model <- model %>% layer_conv_3d(filters = Filters,
                                                kernel_size = c(3, 3, 1),
                                                strides = Strides,
                                                padding = 'same')
      return(adjusted_model)
    } # cardinality equal to 1 => single downward path
    
    if(Filters %% cardinality != 0)
    {
      stop("possible filter size issue or number of independent paths related to models cardinality")  #when creating  network architecture with a single path or many independent paths
      #learnable layers filter size modulo cardinality should always be zero
    }
    
    Slice(keras$layers$Layer) %py_class%{ #Define Custom Layer using py_class constructor 
      initialize <- function(begin, end){
        super$initialize()
        self$begin <- begin
        self$end <- end
        
      }
      
      call <- function(inputs){  #<-------------------------------------------------------------------
        #                                                                                             |                                                                     
        tf$strided_slice(inputs, self$begin, self$end) # function to tensor slice shape from last     | 
      }                                                # constructed layer of current model           |
      # This is the core operator for cardinality    |
      get_config <- function(){ # Define config  so model + graph are both portable                   |
        list(begin = self$begin$numpy(), #Convert to numpy for JSON serializability                   |
             end = self$end$numpy())     #                                                            |
      }                                  #                                                            |  
    }                                    # begin and end are params for tf.strided_slice              |
    #                                                                                                 |  
    layer_slice <- create_layer_wrapper(Slice) #Create wrapper allowing for use of %>% operator with "model" as input
    
    
    group_filters_related_to_paths <- as.integer(Filters / cardinality) 
    convolution_layers <- list()
    
    for(j in 1:cardinality){
      
      dim_1 <- as.integer(model$get_shape()[1])
      dim_2 <- as.integer(model$get_shape()[2])
      dim_3 <- as.integer(model$get_shape()[3])
      dim_4 <- as.integer(model$get_shape()[4])
      
      dim_5_1 <- as.integer(( j - 1 ) * group_filters_related_to_paths)
      dim_5_2 <- as.integer( j * group_filters_related_to_paths)
      
      k_set_image_data_format('channels_last')
      
      
      convolution_layers[[j]] <- model %>% layer_slice(begin = tf$constant(list(as.integer(0), as.integer(0), as.integer(0), as.integer(0), dim_5_1)),
                                                       end = tf$constant(list(as.integer(1), dim_2, dim_3, dim_4, dim_5_2)))
      
      convolution_layers[[j]] <- convolution_layers[[j]] %>%
        layer_conv_3d(filters = group_filters_related_to_paths,
                      kernel_size = c(3, 3, 1),
                      strides = Strides,
                      padding = 'same')
    } #Create list of independent residual blocks
    
    adjusted_model <- layer_concatenate(convolution_layers) # concatenate each path together
    
    #             Layer(n-1) ---------------------------  Skip Connection
    #                 |                                 | 
    #      --------------------------                   |
    #      |          |             |                   |
    #      |          |             |                   |
    #      |          |             |                   |
    #     Path 1    Path 2        Path 3                | Independent Paths
    # |  Conv   |  Conv     |  Conv     |               |
    # |    .    |           |           |               |
    # |    .    |           |           |               |
    # |    .    |           |           |               |
    # |    .    |           |           |               |
    # |         |           |           |               |
    #  --------- ----------- -----------                |
    #      |         |            |                     |
    #      |         |            |                     |
    #      |         |            |                     |
    #      |         |            |                     |
    #      ---->     +     <----                        | concatenation of layers
    #                 <--------------------------------                                  
    #                 
    
    #Increasing cardinality will create a ensemble of many smaller networks
    #This idea is similar to wide residual networks, rather then going deeper we can go wider
    #Resnext combines both ideas of deeper and wider without actually doing either
    #It follows from a split-transform merge paradigm where the outputs 
    # are merged by adding them together
    
    return(adjusted_model)
    
  } #End of Function [2]
  
  
  residualBlock_original <- function(model, 
                                     FiltersIn,
                                     FiltersOut,
                                     strides = c(1, 1, 1),
                                     project = FALSE){
    shortcut <- model
    
    
    model <- model %>% layer_conv_3d(filters = FiltersIn,
                                     kernel_size = c(1, 1, 1),
                                     strides = c(1, 1, 1),
                                     padding = 'same')
    
    model <- operational_layers(model, 
                                normalization = TRUE,
                                non_linear_activation = "leaky_relu")
    
    model <- convolution_adjustment(model,
                                    Filters = FiltersIn,
                                    Strides = strides)
    
    model <- operational_layers(model, 
                                normalization = TRUE,
                                non_linear_activation = "leaky_relu")
    
    model <- model %>% layer_conv_3d(filters = FiltersOut,
                                     kernel_size = c(1, 1, 1),
                                     strides = c(1, 1, 1),
                                     padding = 'same')
    
    
    
    if(project == TRUE || prod(strides == c(1, 1, 1)) == 0)
    {
      shortcut <- shortcut %>% layer_conv_3d(filters = FiltersOut,
                                             kernel_size = c(1, 1, 1),
                                             strides = strides,
                                             padding = 'same')
      
    }
    
    model <- layer_add(list( shortcut, model ))
    
    model <- operational_layers(model, 
                                normalization = FALSE,
                                non_linear_activation = "leaky_relu")
    
    #Original Residual block design 
    
    #   Layer(n-1) 
    #      |                                  
    #      --------------------------  Skip Connection  
    #      |                        |
    # |  Weight              |      |
    # |  Batch Normalization |      |
    # |  Activation Operation|      | 
    # |  Weight              |      |
    # |  Batch Normalization |      |
    # |----------------------|      |
    #           |                   |  
    #           |                   |
    #           + <-----------------  Addition by identity mapping
    #
    #           |
    #  Activation Operation         Nonlinear & differentiable operation after addition... continue 
    
    return(model)
  }#End of Function [3]
  
  
  residualBlock_pre_activation_variant <- function(model, 
                                                   FiltersIn,
                                                   FiltersOut,
                                                   strides = c(1, 1, 1),
                                                   order = c("first", "second"),
                                                   project = FALSE){
    shortcut <- model
    
    if(order == "second"){
      
      model <-  operational_layers(model, 
                                   normalization = TRUE,
                                   non_linear_activation = "leaky_relu")
    }
    
    
    model <- model %>% layer_conv_3d(filters = FiltersIn,
                                     kernel_size = c(1, 1, 1),
                                     strides = c(1, 1, 1),
                                     padding = 'same')
    
    model <- operational_layers(model, 
                                normalization = TRUE,
                                non_linear_activation = "leaky_relu")
    
    model <- convolution_adjustment(model,
                                    Filters = FiltersIn,
                                    Strides = strides)
    
    model <- operational_layers(model, 
                                normalization = TRUE,
                                non_linear_activation = "leaky_relu")
    
    model <- model %>% layer_conv_3d(filters = FiltersOut,
                                     kernel_size = c(1, 1, 1),
                                     strides = c(1, 1, 1),
                                     padding = 'same')
    
    if(project == TRUE || prod(strides == c(1, 1, 1)) == 0){
      shortcut <- shortcut %>% layer_conv_3d(filters = FiltersOut,
                                             kernel_size = c(1, 1, 1),
                                             strides = strides,
                                             padding = 'same')
      
    }
    
    
    model <- layer_add(list( shortcut, model ))
    
    #Full Pre-Activation Residual block design
    #refined residual block, a pre-activation variant of residual block,
    #in which the gradients can flow through the shortcut connections to any other earlier layer unimpededly.
    #This is the Resnet Version 2 architecture and shows considerable improvements compared to version 1 from above... supposedly... sorta sudo science lol
    
    #   Layer(n-1) 
    #      |                                  
    #      --------------------------  Skip Connection  
    #      |                        |
    # |  Batch Normalization |      |
    # |  Activation Operation|      | 
    # |  Weight              |      |
    # |  Batch Normalization |      |
    # |  Activation Operation|      | 
    # |  Weight              |      |
    # |----------------------|      |
    #           |                   |  
    #           |                   |
    #           + <-----------------  Addition by identity mapping
    #
    #           |
    #  after addition... continue 
    
    return(model)
  } #End of Function [4]
  
  #Following top down architecture
  
  
  
  model_input <- keras::layer_input(shape = input_dim) #Create input layer
  
  first_filter <- minimum_filter_size 
  
  output <- model_input %>% layer_conv_3d(filters = first_filter,
                                          kernel_size = c(7, 7, 1),
                                          strides = c(2, 2, 1),
                                          padding = 'same')  #Begin graph configuration
  
  #input -> 1st CNN ->
  
  output <- operational_layers(output, 
                               normalization = TRUE,
                               non_linear_activation = "leaky_relu")
  
  #input -> 1st CNN -> BN -> activation
  
  output <- output %>% layer_max_pooling_3d(pool_size = c(3, 3, 1),
                                            strides = c(2, 2, 1),
                                            padding = 'same')
  
  #input -> 1st CNN -> BN -> activation -> pool
  
  for(i in seq_len(length(blocks))){            #filter size for the start of each new residual plot
    
    in_ <- minimum_filter_size * 2 ^ (blocks[i]) #Factor of 2 expansion defined in Best Practices for CNN Architectures
    
    out_ <- 2 * in_    #Factor of 2 expansion defined in Best Practices for CNN Architectures
    
    for(j in seq_len(blockSchedule_corresponding_to_layers[i])){ #Residual Block Layout Based on Postion and Depth of the model
      
      Project <- FALSE
      
      if(i == 1 && j == 1){
        
        Project <- TRUE
        
      }
      
      if(i > 1 && j == 1){
        
        Strides <- c(2, 2, 1)
        
      } else{
        
        Strides <- c(1, 1, 1)
        
      }
      
      if(block_version == "version_1"){
        
        output <- residualBlock_original(model = output,
                                         FiltersIn = in_,
                                         FiltersOut = out_,
                                         strides = Strides,
                                         project = Project)
      }
      
      if(block_version == "version_2"){
        
        if(i == 1 && j == 1){
          
          output <- residualBlock_original(model = output,
                                           FiltersIn = in_,
                                           FiltersOut = out_,
                                           strides = Strides,
                                           project = Project)
          
        } else{
          
          output <- residualBlock_pre_activation_variant(model = output,
                                                         FiltersIn = in_,
                                                         FiltersOut = out_,
                                                         strides = Strides,
                                                         project = Project,
                                                         order = "second")
          
        }                                                                            #User defined Selection of either Resnet_v2 or Resnet_V1 Architecture
      }
    }
  }
  
  #input -> 1st CNN -> BN -> activation -> pool ->
  #block_1 * [#] -> block_2 * [#] -> block_3 * [#] -> block_4 * [#]... block_n * [#]
  
  output <- output %>% layer_global_average_pooling_3d()                            # Global Average Pooling 
  
  #input -> 1st CNN -> BN -> activation -> pool ->
  #block_1 * [#] -> block_2 * [#] -> block_3 * [#] -> block_4 * [#]... block_n * [#]
  # -> pool
  
  mode <- problem_type
  
  final_layer_activation <- ''
  if(mode == 'classification'){
    
    final_layer_activation <- 'softmax'
    
  } else {
    
    stop('Regression is not implemented in this function, please use Classification or leave... lol')
  }
  
  Model <- NULL
  
  output <- output %>%
    keras::layer_flatten() %>%
    keras::layer_dense(units = number_of_classifications,
                       activation = final_layer_activation)
  
  Model <- keras::keras_model(inputs = model_input, outputs = output)
  
  return(Model)
  
}

# model_1 <- create_residual_network_cnn(input_dim = c(128, 128, 1, 1),
#                                        number_of_classifications = 6,
#                                        blocks = 1:4,
#                                        blockSchedule_corresponding_to_layers = c(3, 4, 6, 3),
#                                        minimum_filter_size = 64,
#                                        cardinality = 1,
#                                        block_version = "version_2",
#                                        problem_type = "classification")












