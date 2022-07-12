M = dim(mat)[1]
N = dim(mat)[2]

#image(mat, col = c(0, 1))

mat <- apply(mat, 1, as.list)

B = list()

cell_check <- function(mat, M, N, x, y){
  if(x < 1 || x > M || y < 1 || y > N || mat[[x]][[y]] == 1) return(FALSE)
  else return(TRUE)
}


P = list()

while(length(P) < 1){
  
  for(i in 1:6){
    for(j in 1:6){
      if(cell_check(mat, M, N, i, j)){
        P = append(P, list(c(i,j)))
        return(P)
        
      }
    }
  }
}


starting_pixel = P[[1]]

x_1 <- starting_pixel[[1]]
y_1 <- starting_pixel[[2]]

B = append(B, list(starting_pixel))

x_2 <- starting_pixel[[1]]
y_2 <- starting_pixel[[2]] - 1

diff_x <- x_2 - x_1
diff_y <- y_2 - y_1

x <- 0
y <- 0
                                                           #  (-1, -1)  |  (-1, 0)    | (-1, +1)    #
moores_neighborhood <- list(c(x, y),                       #----------------------------------------#
                            c(x, y-1),                     #  (0, -1)   |  (0, 0)     | (0, +1)     #
                            c(x-1, y-1),                   #----------------------------------------#
                            c(x-1, y),                     #  (+1, -1)  | (+1, 0)     | (+1, +1)    #
                            c(x-1, y+1),                   #----------------------------------------#
                            c(x, y+1),
                            c(x+1, y+1),
                            c(x+1, y),
                            c(x+1, y-1)) #Counter Clockwise traversal along moores neighborhood



jacob_stop_criterion_termination <- 1


while(jacob_stop_criterion_termination < 2){
  index = 1
  
  for(i in 1:length(moores_neighborhood)){
    if(diff_x == moores_neighborhood[[i]][[1]] && diff_y == moores_neighborhood[[i]][[2]]){
      index = i
      break
    }
  }
  
  while(TRUE){
    if(index == 9){
      index = 1
    }
    
    current_x = x_1 + moores_neighborhood[[index + 1]][[1]]
    current_y = y_1 + moores_neighborhood[[index + 1]][[2]]
    
    if(mat[[current_x]][[current_y]] != 1){
      x_1 = current_x
      y_1 = current_y
      
      temp = list(c(x_1, y_1))
      
      if(all(unlist(temp) == starting_pixel)){
        jacob_stop_criterion_termination = jacob_stop_criterion_termination + 1
        return(jacob_stop_criterion_termination)
      }
      
      B = append(B, temp)
      diff_x = prev_x - x_1
      diff_y = prev_y - y_1
      
      break
    }
    
    prev_x = current_x
    prev_y = current_y
    
    index = index + 1
    
  }
}


B

