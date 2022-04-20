# File:         demo.r 
# Description:  Naive demo-solution given at classroom session
#               for Project 1, Artificial Intelligence 2019, UU
# Author:       Fredrik Nilsson

# Install the package
# install.packages("DeliveryMan_1.1.0.tar.gz", repos = NULL, type="source")
rm(list = ls())
# Load the library
library("DeliveryMan")

# Read documentation
# ?runDeliveryMan
# ?testDM

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # What is our goal?
  if(carInfo$load == 0) {
      carInfo$mem$goal <- nextPickup(trafficMatrix, 
                                     carInfo, 
                                     packageMatrix)
      print(carInfo$mem$goal)
    } else {
      carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
      print(carInfo$mem$goal)
  }
  
  # How do we get there?
  carInfo$nextMove <- nextMove(trafficMatrix,
                               carInfo,
                               packageMatrix)
  return(carInfo)
}

# Find the nearest pickup location for an undelivered package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  distanceVector = abs((packageMatrix[,1] - carInfo$x)) +
                    ((packageMatrix[,2] - carInfo$y))
  distanceVector[packageMatrix[,5] != 0] = Inf
  return(packageMatrix[which.min(distanceVector), c(1,2)])
}

#Function to calculate optimistic heuristic
heuristic <- function(carInfo,x,y){
  carInfo$mem$goal
  xDist = abs(carInfo$mem$goal[[1]] - x)
  yDist = abs(carInfo$mem$goal[[2]] - y)
  # Different h depending on if you go left right up or down
  hUp = abs(carInfo$mem$goal[[2]] - (y+1))+1+ xDist
  hDown = abs(carInfo$mem$goal[[2]] -(y-1))+1+ xDist
  hRight = abs(carInfo$mem$goal[[1]] - (x+1))+1 + yDist
  hLeft = abs(carInfo$mem$goal[[1]] - (x-1))+1 + yDist
  
  hList<-c(hDown,hLeft,hRight,hUp)
  return(hList)
  
}

#function to calculate the cost of making a move
cost <- function(trafficMatrix, x, y){
  if (y<9){
  costUp = trafficMatrix$vroads[x,y]
  }else{
    costUp = 2000
  }
  if (y>1){
  costDown = trafficMatrix$vroads[x,y-1]
  }else{
    costDown= 2000
  }
  if (x<9){
    costRight = trafficMatrix$hroads[x,y]
  }else{
    costRight = 2000
  }
  if (x>1){
    costLeft = trafficMatrix$hroads[x-1,y]
  }else{
    costLeft= 2000
  }
  cost <- c(costDown,costLeft,costRight,costUp)
  return(cost)
}

#nextMove fuction that implements the A* algorithm
nextMove <- function(trafficMatrix, carInfo,packageMatrix){
  x= carInfo$x
  y= carInfo$y
  g = cost(trafficMatrix, x,y)
  h = heuristic(carInfo,x,y)
  f=g+h
  
  #open origin and save nodes in frontier
  frontier <- list(list(x=x, y=y-1,f=f[[1]],g=g[[1]],h=h[[1]],path=2),
                   list(x=x-1, y=y,f=f[[2]],g=g[[2]],h=h[[2]],path=4),
                   list(x=x+1, y=y,f=f[[3]],g=g[[3]],h=h[[3]],path=6),
                   list(x=x, y=y+1,f=f[[4]],g=g[[4]],h=h[[4]],path=8))
 
 
  i=5
  while (carInfo$mem$goal[1]!=x | carInfo$mem$goal[2]!=y) {
    #pick frontier item with lowest f
    f_vec <- c()
    for (j in frontier) {
      f_vec <- c(f_vec, j$f)
    }
    #open
    index = which.min(f_vec)
    #set new x and y
    x<-frontier[[index]]$x
    y<-frontier[[index]]$y
    #set p to path of this node
    p<-frontier[[index]]$path
    readline(prompt = "press [enter]")
    print(x)
    print(y)
    #new cost
    g = cost(trafficMatrix, x,y)
    h = heuristic(carInfo,x,y)
    f = g+h
    dirVec <-matrix(c(c(x,y-1),c(x-1,y),c(x+1,y),c(x,y+1)), ncol = 2, nrow = 4, byrow = TRUE)
    
    
    frontier = frontier[-index]
    #pop open node from frontier
    #
    
    for (k in 1:4) {
      if (g[[k]]<1000){
        frontier[[i]] <- list(x=dirVec[k,1],y=dirVec[k,2],f=f[k],g=g[k],h=h[k],path=p)
        i=i+1
        }
      }
      
  readline(prompt = "press [enter]")
  
  print(i)
  print(x)
  print(y)
  print(f)
  }

  
  return(p)
  
}
  
  
#}

# Find the move to get to carInfo$mem$goal
#nextMove <- function(trafficMatrix, carInfo, packageMatrix) {
#return(Astar(trafficMatrix, carInfo))
#}


runDeliveryMan(carReady = myFunction)

































               