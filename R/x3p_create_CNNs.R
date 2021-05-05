#' Create CNN architecture
#'
#' @param Learning_rate learning rate speed
#' @param decay weighted decay rate
#' @export
create_residual_3dCNN <- function(Learning_rate = 0.1, decay = FALSE){

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
    keras::layer_dense(units = 6, activation = "softmax") # Layer 30



  cnn_resNet_30 <- keras::keras_model(input, output)

  cnn_resNet_30 %>% keras::compile(loss = "categorical_crossentropy",
                            optimizer = optimizer_rmsprop(lr = Learning_rate, decay = decay),
                            metrics = "accuracy")

  return(cnn_resNet_30)


}
