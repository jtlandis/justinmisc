#Useful functions to help Run final_figures.Rmd

paste2 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  gsub(paste0("(^",sep,"|",sep,"$)"),"",
       gsub(paste0(sep,sep),sep,
            do.call(paste,c(L,list(sep=sep)))))
}


char.position <- function(x, pattern, position = 0, instance = 1){
  if(!is.character(x)){
    stop("x must be a character")
  }
  if(!is.character(pattern)) {
    stop("pattern must be a character")
  }
  if(!is.numeric(position) && ((position %% 1)!=0)) {
    stop("position must be an integer")
  }
  
  r <- nchar(x)
  
  inst <- instance
  if(!is.numeric(inst)){
    if(is.character(inst) && inst != "last") {
      stop("instance must be a positive integer or be the string \"last\"")
    } else {
      inst <- max(r)
    }
    
  } else{
    if((inst %% 1) !=0 || inst <= 0) {
      stop("instance must be a positive integer or be the string \"last\"")
    }
    
  }
  
  
  pos <- matrix(nrow = length(x), ncol = max(r))
  for(j in 1:length(x)){
    b <- 0
    c <- 0
    a <- r[j]
    for(i in 1:a) {
      if(substr(x[j],i,i) == pattern) {
        b <- b + 1
        if(((position + i) <= 0)|((position + i) >= a)) {
          if((position + i) <= 0) {
            
            pos[j,c(b:max(r))] <- 1
            
          }
          if((position + i) >= a) {
            pos[j,c(b:max(r))] <- a
            
          }
          c <- 1
        } else{
          pos[j, c(b:max(r))] <- i + position
          c <- 1
        }
      } else {
        if(c == 0){
          pos[j] <- NA
        }
      }
      
    }
  }
  
  p <- pos[,inst]
  return(p)
  
}

break.dist <- function(breaks) {
  n <- length(breaks)
  vec <- rep(0, n-1)
  for(i in 2:n) {
    vec[i-1] <- abs(breaks[i]-breaks[i-1]) 
  }
  return(vec)
}

color.split <- function(start, end, into) {
  #Creating color vector reference for base 16 codes
  a <- rep(c("0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F"), 16)
  b <- rep(c("0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F"), each = 16)
  color.vec <- cbind(b,a)
  color.vec <- as.data.frame(color.vec) %>%
    mutate(code = paste(b,a,sep="")) %>%
    mutate(number = strtoi(code, base = 16))
  start <- c(substr(start, 1,2), substr(start, 3,4), substr(start, 5,6))
  end <- c(substr(end, 1,2), substr(end, 3,4), substr(end, 5,6))
  start <- strtoi(start,16)
  end <- strtoi(end,16)
  diff <- (end - start)/into
  col.split.into <- rep("0", into)
  for(k in 1:into) {
    work <- start + round(k*diff, 0)
    col.split.into[k] <- paste("#",
                               color.vec[(work +1),]$code[1],
                               color.vec[(work +1),]$code[2], 
                               color.vec[(work +1),]$code[3], 
                               sep = "")
  }
  return(col.split.into)
}

#Function called by user
#value = data frame numerical vector that is used in ggplot for colors
#colors = hexidecimal colors desired. Colors will be assigned from least to greatest of the value vector
#cbreaks = vector equal length of colors, indicates at what value you desire the color of that index to be
#tol = level of tolerance of how close the colors will be assigned to cbreaks. Smaller tolerance will be a larger parsing

gradiant.color.range <- function(value, colors, cbreaks, tol = 0.05, report = FALSE) {

  colors.input <- colors # keeping the input for later
  colors <- substr(colors.input, 2,7) # removes '#'
  
  if(!is.logical(report)) {
    stop("report must be a logical vector")
  }
  if(!is.numeric(cbreaks)) {
    stop("cbreaks must be a vector of numeric values")
  }
  if(length(colors)!=length(cbreaks)){
    stop("cbreaks and colors must be the same size")
  }
  break.test <- cbreaks
  break.test <- cbreaks[order(break.test)]
  if(!all(cbreaks == break.test)) {
    stop("cbreaks must be assigned break points in ascending order")
  }

  v.max <- max(value) 
  v.min <- min(value)
  
  r <- v.max-v.min
  n <- length(colors)
  break.test <- cbreaks - (v.min) #shift cbreaks to be positive, necessary for accurate index counting when parsing the range
  
  
  for(i in n:700) { #find the index at which the range is sufficently parsed to put the color vector at desired cbreaks
    
    int <- (r)/i
    new <- rep(0, length(cbreaks))
    for(j in 1:length(break.test)) {
      if((abs(break.test[j])>=int)) {
        new[j] <- break.test[j] %% int
      } else {
        new[j] <- int %% break.test[j]
        if(break.test[j] == 0) {
          new[j] <- 0
        }
      }
    }
    
    if(all(new <= tol)) {
      div <- i
      break()
    }
    
    
    
    
    
  }
  
  index <- round(((break.test+new)/int), 0)+1  #can't explain why the plus 1 is necessary yet
  dis <- break.dist(index)
  if((div %% 2) !=0) {
    vec.l <- c(ceiling(div/2), floor(div/2))
  } else {
    vec.l <- c(div/2,div/2)
  }
  col.vec <- rep(colors.input[c(1,length(colors.input))], vec.l)
  
  
  for(l in 1:length(dis)) {
    grad.col <- color.split(start = colors[l], end = colors[l+1], into = dis[l])
    for(m in 1:length(grad.col)) {
      col.vec[index[l]+m] <- grad.col[m]
    }
  }
  if(report == TRUE) {
    parse.value <- rep(0, length(col.vec))
    for(i in 1:length(col.vec)) {
      parse.value[i] <- int*(i-1) + v.min
    }
    col.vec <- as.data.frame(cbind(col.vec, parse.value))
    colnames(col.vec) <- c("colors", "value")
    col.vec$value <- as.numeric(as.character(col.vec$value))
    col.vec$colors <- as.character((col.vec$colors))
  }
  return(col.vec)
}


commoncol <- function(x, y, join=FALSE) {
  if(!is.data.frame(x)) {
    stop("x must be a data frame")
  }
  if(!is.data.frame(y)){
    stop("y must be a data frame")
  }
  if(!is.logical(join)){
    stop("join must be logical")
  }
  
  xcolnames <- colnames(x)
  ycolnames <- colnames(y)
  x.n <- length(xcolnames)
  y.n <- length(ycolnames)
  
  if(x.n<=y.n){
    n <- x.n
    small <- xcolnames
    large <- ycolnames
  } else {
    n <- y.n
    small <- ycolnames
    large <- xcolnames
  }
  
  vec <- rep(NA, n)
  crctn <- 0
  
  for(i in 1:n) {
    if(any(small[i]==large)){
      vec[i-crctn] <- small[i]
    } else{
      crctn <- crctn+1
    }
  }
  m <- n-crctn
  vec <- vec[1:m]
  
  if(join==TRUE){
    new.vec <- rep(NA,length(vec))
    for(i in 1:length(vec)){
      new.vec[i] <- paste(vec[i],"=",vec[i], sep ="")
    }
    return(new.vec)
  }else {
    return(vec)
  }
  
  
}