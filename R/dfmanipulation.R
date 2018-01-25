#Data frame manipulation functions

#' @title Paste that removes NAs
#'
#' @description Use this Function to ignore NAs in paste
#'
#' @name paste2
#'
#' @param sep character to separate paste2 arguments
#'
#' @return Character string
#'
#' @source https://stackoverflow.com/questions/13673894/suppress-nas-in-paste
#'
#' @export
paste2 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  gsub(paste0("(^",sep,"|",sep,"$)"),"",
       gsub(paste0(sep,sep),sep,
            do.call(paste,c(L,list(sep=sep)))))
}

#' @title  Find positions of a particular pattern.
#'
#' @description designed to find the position of a particular character
#' in a string. Useful with substr function
#'
#' @name char.position
#'
#' @param x Character vector to search through
#' @param pattern character string that the function will attempt to
#' find in x
#' @param position positive or negative integer used to modulate the position
#' returned if pattern is found in x, char.position returns the exact position by
#' default. -1 returns position before pattern and 1 returns position after pattern.
#' @param instance positive integer used to modulate which instance of the pattern
#' is returned. Default is first instance.
#'
#' @return Number vector of position of pattern found, returns NA if not found
#'
#' @export
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

#' @title Find Common Column Names
#'
#' @description Used to find common column names of two data frames.
#' Useful for large data sets
#'
#' @name commoncol
#'
#' @param x data frame
#' @param y data frame
#' @param join Logical, augments the return. Default set to FALSE. Still
#' a work in progress
#'
#' @return If join = FALSE this will return character string vector of the
#' common names. If join = TRUE this will try to return character string in
#' format used for innerjoin.
#'
#' @export
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
