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


fitness <-function(carInfo,trafficMatrix, x,y, gPrev){
  
  #first calculate the heuristic for all 4 neighbouring nodes
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

nextMove <- function(trafficMatrix, carInfo,packageMatrix){
  #current pos of car
  
  x= carInfo$x
  y= carInfo$y
  
  if(carInfo$mem$goal[1]==x & carInfo$mem$goal[2]==y) {
    print('stay')
    return(5)
  }else{
    gPrev=0
    #fitness function for moving to all surrounding nodes
    fit = fitness(carInfo,trafficMatrix, x,y, gPrev)
    f=fit[[1]]
    g=fit[[2]]
    h=fit[[3]]
    
    i=1
    frontier <-list(list(x=x, y=y, g=0, h=h[1]+1, f=h[1]+1, path=5))
    xCoord = c(x,x-1,x+1,x)
    yCoord = c(y-1,y,y,y+1)
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
      #set new p from old p, since we are only interested in first step
      p<-frontier[[index]]$path
      #x and y of opened node
      x<-frontier[[index]]$x
      y<-frontier[[index]]$y
      #previous cost
      gPrev =  frontier[[index]]$g
      
      #look at new surrounding nodes
      fit =   fit = fitness(carInfo,trafficMatrix, x,y, gPrev)
      f=fit[[1]]
      g=fit[[2]]
      h=fit[[3]]
      
      #add node to visited and remove from frontier
      v=v+1
      visited[[v]]<-frontier[[index]]
      frontier = frontier[-index]
      i=i-1
      
      
      #coordinates of surrounding nodes
      xCoord = c(x,x-1,x+1,x)
      yCoord = c(y-1,y,y,y+1)
      #coordinates of already visited nodes
      
      for (k in 1:4) {
        ptm<-proc.time() 
        visitDup = sapply(visited, function(item) 
          isTRUE(all.equal(c(item$x,item$y),
                           c(xCoord[[k]],yCoord[[k]]))))
        #########Use which min to find index where visited is
        
        
        # coordInFront = coordInSet(xyInFrontier,c(xCoord[[k]],yCoord[[k]]) )
        frontDup = sapply(frontier, function(item) 
          isTRUE(all.equal(c(item$x,item$y),
                           c(xCoord[[k]],yCoord[[k]]))))
        #########Use which min to find index where duplicates are
        #l <- proc.time()-ptm
        #print(el)
        if (!(TRUE %in% visitDup) & g[[k]]!=Inf){
          
          if(TRUE %in% frontDup){
            #  readline(prompt = " true1")
            ind = which(frontDup==TRUE)
            if(f[[k]]<frontier[[ind]]$f ){
              #   readline(prompt = " true2")
              frontier = frontier[-ind]
              frontier[[i]] <- list(x=xCoord[[k]],y=yCoord[k],f=f[[k]],g=g[[k]],h=h[[k]],path=p)
            }
          }else{
            i=i+1
            frontier[[i]] <- list(x=xCoord[[k]],y=yCoord[k],f=f[[k]],g=g[[k]],h=h[[k]],path=p)
            #readline(prompt = " true3")
          }
        } 
        
      }
    }
    
    return(p)
  }
}


#runDeliveryMan(myFunction,doPlot=T )
print(testDM(myFunction,n=1))