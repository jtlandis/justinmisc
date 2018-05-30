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

#' @title  Index or Column name
#'
#' @description Generally internal funciton mostly used to see if vector
#'  passed specifies column by character name or index
#'
#' @name index.o.coln
#'
#' @param vec vector we are testing
#' @param v.size expected size of vec
#' @param v.name argument name of vec in its function call
#' @param name.col name of all column vectors
#'
#' @return integer vector of size v.size
#'
#' @export
index.o.coln <- function(vec, v.size, v.name, name.col) {
  if(length(vec)!=v.size) {
    stop(paste(v.name,"must be of length 1", sep = " "))
  }
  if(is.character(vec)|is.numeric(vec)) {
    if(is.character(vec)){
      index <- vector("integer", length = v.size)
      for(i in 1:v.size) {
        if(any(vec[i]==name.col)) {
          index[i] <- which(vec[i]==name.col)

        } else {
          stop(paste("ERROR: at least 1 element of",v.name,"does not match the column names of df",sep = " "))
        }
      }

    }
    if(is.numeric(vec)){
      if(all((vec%%1)==0)){
        index <- vec
      } else {
        stop(paste("ERROR:",v.name,"was not given whole integers", sep = " "))
      }
    }

  } else {
    stop(paste(v.name,"de.atomic must be character vector of column name or column index",sep = " "))
  }

  return(index)
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
#' is returned. Default is first instance. Assign "last" to return last instance in
#' string
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
  pat.length <- nchar(pattern)
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
      if(substr(x[j],i,(i+pat.length-1)) == pattern) {
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


#' @title Make data frame atomic
#'
#' @description Used when one column has nested values that
#' make the data frame not atomic
#'
#' @name make.atomic
#'
#' @param df data frame
#' @param de.atomic Index of the column or character column
#' name to make atomic
#' @param sep character pattern delimiter for nested factors
#' @param new.name Name of the atomic column
#'
#' @return denested data frame
#'
#' @export
make.atomic <- function(df, de.atomic, sep, new.name = "atomic") {

  if(!is.character(new.name)) {
    stop("new.name must be a character vector")
  }
  name.col <- colnames(df)
  index <- index.o.coln(vec = de.atomic,v.size = 1,v.name = "de.atomic",name.col = name.col)

  df <- df %>%
    mutate(atomic=(strsplit(as.character(df[,name.col[index]]),split = sep))) %>%
    unnest(atomic)

  colnames(df) <- c(name.col, new.name)

  return(df)

}



#' @title Mode Type
#'
#' @description Find the mode of the variables of a df as
#' dictated by str
#'
#' @name mode.type
#'
#' @param df data frame
#'
#' @return Return character vector of mode types
#'
#' @export
mode.type <- function(df) {
  output <- capture.output(str(df))
  output <- output[-1]
  output <- substr(x = output,
                   start = justinmisc::char.position(x = output,
                                                     pattern = ":",
                                                     position = 2,
                                                     instance = 1),
                   stop = str_length(output))

  output <- substr(x = output,
                   start = 1,
                   stop = justinmisc::char.position(x = output,
                                                    pattern = " ",
                                                    position = -1,
                                                    instance = 1))
  return(output)
}


#' @title correct mode types
#'
#' @description Function to correct modes after df
#' manipulation changes columns to characters
#'
#' @name correct.mode
#'
#' @param df data frame
#' @param mode.vec designed to take output of function mode.type(),
#' mode.vec must be one of the following: "num","int","Factor",
#' "cplx","chr","logi","raw", "ignore". Mode type is changed by index.
#'
#' @return df with mode types
#'
#' @export
correct.mode <- function(df, mode.vec, space_replace = "_") {

  if(ncol(df)!=length(mode.vec)) {
    stop("ERROR: number of data frame columns does not equal length of mode.vec ")
  }
  modes <- c("num","int","Factor","cplx","chr","logi","raw","ignore")
  for(j in 1:(length(mode.vec))) {
    if(!any(mode.vec[j]==modes)) {
      stop("mode.vec must be one of the following: \"num\",\"int\",\"Factor\",\"cplx\",\"chr\",\"logi\",\"raw\", \"ignore\"")
    }
  }
  cnames <- colnames(df)
  if(any(str_detect(cnames, " "))) {
    warning("Colnames contain spaces")
    cnames <- gsub(" ", space_replace, cnames)
  }
  for(i in 1:(ncol(df))){
    if(mode.vec[i]=="num") {
      eval(parse(text = paste("df$",colnames(df)[i],
                              " <- as.numeric(as.character(",
                              "df$",colnames(df)[i],
                              "))",
                              sep = "")))
      next
    }
    if(mode.vec[i]=="int") {
      eval(parse(text = paste("df$",colnames(df)[i],
                              " <- as.integer(as.character(",
                              "df$",colnames(df)[i],
                              "))",
                              sep = "")))
      next
    }
    if(mode.vec[i]=="Factor") {
      eval(parse(text = paste("df$",colnames(df)[i],
                              " <- as.factor(as.character(",
                              "df$",colnames(df)[i],
                              "))",
                              sep = "")))
      next
    }
    if(mode.vec[i]=="chr") {
      eval(parse(text = paste("df$",colnames(df)[i],
                              " <- as.character(",
                              "df$",colnames(df)[i],
                              ")",
                              sep = "")))
      next
    }
    if(mode.vec[i]=="cplx") {
      eval(parse(text = paste("df$",colnames(df)[i],
                              " <- as.cplx(as.character(",
                              "df$",colnames(df)[i],
                              "))",
                              sep = "")))
      next
    }
    if(mode.vec[i]=="logi") {
      eval(parse(text = paste("df$",colnames(df)[i],
                              " <- as.logical(as.character(",
                              "df$",colnames(df)[i],
                              "))",
                              sep = "")))
      next
    }
    if(mode.vec[i]=="raw") {
      eval(parse(text = paste("df$",colnames(df)[i],
                              " <- as.raw(as.character(",
                              "df$",colnames(df)[i],
                              "))",
                              sep = "")))
      next
    }
  }

  return(df)
}

#' @title reshape count data into frequency data
#'
#' @description Change count data into frequency data by repeating
#' lines of data frame via the count column.
#'
#' @name count.to.frequency
#'
#' @param df data frame
#' @param count Column vector name (character) or index (integer)
#' that specifies count data
#' @param drop.na Logical: Default set to FALSE for speed, NA may
#' affect output thus set to TRUE if unsure.
#'
#' @return df with mode types
#'
#' @export
count.to.frequency <- function(df,count,drop.na = FALSE){


  count <- index.o.coln(vec = count,
                        v.size = 1,
                        v.name = "count",
                        name.col = colnames(df))

  if(drop.na==TRUE){
    df <- df[!is.na(df[,count]),]
  }

  df.work <- df[,-count]
  df.mode <- mode.type(df = df.work)
  count <- df[,count]
  if(any(is.na(count))){
    stop("count vector cannot contain NA")
  }
  if(sum(count%%1)>=(1e-15*length(count))){
    stop("count vector must have only integers")
  }

  coln <- colnames(df.work)

  df.new <- rep(df.work[,coln[1]], count )
  for(i in 2:length(coln)){
    vec <- rep(as.character(df.work[,coln[i]]), count)
    df.new <- cbind(df.new, vec)
  }

  colnames(df.new) <- coln
  df.new <- as.data.frame(df.new)
  df.new <- correct.mode(df = df.new, mode.vec = df.mode)
  return(df.new)

}

#' @title Reorder factors according to simple funciton
#'
#' @description Will reorder factors based off of a group subset
#'
#' @name reorderFactor.by
#'
#' @param data data frame
#' @param factor.to.order column name or index of factors that will be reordered
#' @param factor.col column name or index that contains 'by.factor'
#' @param by.factor character name of factor in factor.col that data will
#' be subsetted for calculations and group desired for order
#' @param FUN Simple function that takes a vector of numbers and returns a single
#' output. Default set to NULL, must assign a function if group.by is not NULL.
#' @param applied.on Numeric vector used as inputs to FUN. Will ultamitly
#' determine order of factor.to.order
#' @param group.by column names or indexes that further groups by.factor subset to
#' which FUN is applied. Default set to NULL.
#' @param decreasing logical, default set to FALSE. Determines if order is increasing
#' or decreasing.
#'
#' @return returns data with factor.to.order reordered by FUN of by.factor subsetted through group.by.
#'
#' @export
reorderFactor.by <- function(data, factor.to.order,factor.col, by.factor, FUN=NULL, applied.on,group.by=NULL, decreasing = FALSE) {

  df <- data
  names <- colnames(df)
  index.t.o <- index.o.coln(vec = factor.to.order, v.size = 1, v.name = "factor.to.order", name.col = names)
  index.f <- index.o.coln(vec = factor.col, v.size = 1, v.name = "factor.col", name.col = names)
  index.on <- index.o.coln(vec = applied.on, v.size = 1, v.name = "applied.on", name.col = names)
  if(!is.character(by.factor)){
    stop("by.factor must be a character")
  }
  if(!is.factor(df[,index.t.o])) {
    stop("factor.to.order must be a factor vector")
  }
  if(!is.numeric(df[,index.on])) {
    stop("applied.on must be a numeric vector")
  }

  a <- df[df[,index.f]==by.factor,]
  if(nrow(a)==0){
    stop(paste("by.factor was not found in",names[index.f], sep = " "))
  }
  a$temp.vector.ignore <- a[,index.on]


  if(!is.null(group.by)){
    if(is.null(FUN)) {
      stop("Please assign a function when using group.by")
    }
    index.gb <- index.o.coln(vec = group.by, v.size = length(group.by), v.name = "group.by", name.col = names)
    a$id <- interaction(a[,index.gb])

    factor.list <- levels(a$id)
    a.build <- subset(a, id %in% factor.list[1])
    a.build <- a.build %>%
      mutate(new.order=FUN(a.build$temp.vector.ignore))
    for(i in 2:length(factor.list)){
      a.work <- subset(a, id %in% factor.list[i])
      a.work <- a.work %>%
        mutate(new.order=FUN(as.numeric(a.work$temp.vector.ignore)))
      a.build <- rbind(a.build, a.work)
    }
  } else {
    a.build <- a
    a.build$new.order <- a[,index.on]
  }

  df[,index.t.o] <- factor(df[,index.t.o], levels = unique(a.build[order(a.build$new.order, decreasing = decreasing),index.t.o]))
  return(df)
}
