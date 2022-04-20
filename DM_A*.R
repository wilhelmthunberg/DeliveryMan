#Group 37
#script that implements the A* algorithm on the delivery man problem
rm(list = ls())
# Load the library
library("DeliveryMan")

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # What is our goal?
  if(carInfo$load == 0) {
    carInfo$mem$goal <- nextPickup(trafficMatrix, 
                                   carInfo, 
                                   packageMatrix)
  } else {
    carInfo$mem$goal <- packageMatrix[carInfo$load, c(3,4)]
  }
  # How do we get there?
  carInfo$nextMove <- nextMove(trafficMatrix,
                               carInfo,
                               packageMatrix)
  return(carInfo)
}

nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  distanceVector = abs((packageMatrix[,1] - carInfo$x)) +abs((packageMatrix[,2] - carInfo$y))
  distanceVector[packageMatrix[,5] != 0] = Inf
  return(packageMatrix[which.min(distanceVector), c(1,2)])
}

#fitness function that returns g,h, and f of 4 negihbouring nodes
fitness <-function(carInfo,trafficMatrix, x,y, gPrev){
  #first calculate the heuristic(manhattan distance) for all 4 neighbouring nodes
  goal =carInfo$mem$goal
  xDist = abs(goal[[1]] - x)
  yDist = abs(goal[[2]] - y)
  hUp = abs(goal[[2]] - (y+1))+ xDist
  hDown = abs(goal[[2]] -(y-1))+ xDist
  hRight = abs(goal[[1]] - (x+1)) + yDist
  hLeft = abs(goal[[1]] - (x-1)) + yDist
  h<-c(hDown,hLeft,hRight,hUp)
  
  #then calculate the total cost after moving to these nodes
  #moving outside field costs Inf
  if (y<10){
    costUp = trafficMatrix$vroads[x,y]
  }else{
    costUp = Inf
  }
  if (y>1){
    costDown = trafficMatrix$vroads[x,y-1]
  }else{
    costDown= Inf
  }
  if (x<10){
    costRight = trafficMatrix$hroads[x,y]
  }else{
    costRight = Inf
  }
  if (x>1){
    costLeft = trafficMatrix$hroads[x-1,y]
  }else{
    costLeft= Inf
  }
  g <- c(costDown,costLeft,costRight,costUp)+gPrev
  
  f= g+h
  
  return(list(f,g,h))
  
}

#function to find index of specified coordinates in list
isInList = function(coord, lis) {
  if (length(lis) == 0) {
    return (0)
  }
  for (i in 1:length(lis)) {
    if (all(coord == c(lis[[i]]$x, lis[[i]]$y))) {
      return (i)
    }
  }
  return (0)
}

nextMove <- function(trafficMatrix, carInfo,packageMatrix){
  #current pos of car
  x= carInfo$x
  y= carInfo$y
  
  #if car is standing on goal, stay
  if(carInfo$mem$goal[1]==x & carInfo$mem$goal[2]==y) {
    return(5)
  }
  #cost at origin node is 0
    gPrev=0
    #fitness function for moving to all surrounding nodes
    fit = fitness(carInfo,trafficMatrix, x,y, gPrev)
    f=fit[[1]]
    g=fit[[2]]
    h=fit[[3]]
    #initiate frontier by adding origin node
    i=1
    frontier <-list(list(x=x, y=y, g=0, h=h[1]+1, f=h[1]+1, path=5))
    #coordinates of neighbour nodes
    xCoord = c(x,x-1,x+1,x)
    yCoord = c(y-1,y,y,y+1)
    #add neighbours of origin if not outside field
    for (k in 1:4) {
      if(g[[k]]!=Inf){
        i=i+1
        frontier[[i]] <-list(x=xCoord[k], y=yCoord[k], g=g[[k]], h=h[[k]], f=f[[k]], path=k*2)
      }
    }
    v=1
    visited<-list(list(x=x, y=y, g=0, h=h[1]+1, f=h[1]+1, path=5))
    frontier=frontier[-1]
    i=i-1
    
    #while not on goal node
    while (carInfo$mem$goal[1]!=x | carInfo$mem$goal[2]!=y) {
      #f of all nodes in one vector
      f_vec =lapply(frontier, function(item) c(item$f))
      #find index of lowest f and open this node
      index = which.min(f_vec)
      
      #add opened node to list of visted nodes
      v=v+1
      visited[[v]]<-frontier[[index]]
      #add first move from orgin node to new node
      p<-frontier[[index]]$path
      #x and y of opened node
      x<-frontier[[index]]$x
      y<-frontier[[index]]$y
      #previous cost
      gPrev =  frontier[[index]]$g
      #look at fitness function of new surrounding nodes
     fit = fitness(carInfo,trafficMatrix, x,y, gPrev)
      f=fit[[1]]
      g=fit[[2]]
      h=fit[[3]]
      #remove from frontier
      frontier = frontier[-index]
      i=i-1
      
      #coordinates of neigbour nodes to opened node
      xCoord = c(x,x-1,x+1,x)
      yCoord = c(y-1,y,y,y+1)
      #for all neigbour nodes
      for (k in 1:4) {
        #not visited and not outside field
        if (!isInList(c(xCoord[[k]],yCoord[k]), visited) & g[[k]]!=Inf){
          #index of duplicate in frontier
          dupIndex=isInList(c(xCoord[[k]],yCoord[k]), frontier)
          if(dupIndex!=0){
            #replace duplicate if lower f
            if(f[[k]]<frontier[[dupIndex]]$f ){
              frontier = frontier[-dupIndex]
              frontier[[i]] <- list(x=xCoord[[k]],y=yCoord[k],f=f[[k]],g=g[[k]],h=h[[k]],path=p)
            }
          }else{
            i=i+1
            frontier[[i]] <- list(x=xCoord[[k]],y=yCoord[k],f=f[[k]],g=g[[k]],h=h[[k]],path=p)
          }
        } 
        
      }
    }
    return(p)
  
}


runDeliveryMan(myFunction,doPlot=T )


#test = testDM(myFunction)
print('Average number of turns:')
print(test)