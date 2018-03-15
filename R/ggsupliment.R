#Useful functions to help Run final_figures.Rmd

#' @import ggplot2
#' @import dplyr
#' @import magrittr
#' @importFrom  tidyr unnest
#' @import stringr
#' @import ggrepel



#' @title returns the distance between numbers
#'
#' @name break.dist
#'
#' @param breaks used to find distance between numbers
#'
#' @return returns the distance betweens numbers
break.dist <- function(breaks) {
  n <- length(breaks)
  vec <- rep(0, n-1)
  for(i in 2:n) {
    vec[i-1] <- abs(breaks[i]-breaks[i-1])
  }
  return(vec)
}

#' @title Splitting color evenly
#'
#' @description Splits the two color hexidecimal codes into evenly spaced colors
#'
#' @name color.split
#'
#' @param start color code passed of where to begin
#' @param end color code passed of where to end
#' @param into number of colors that will be found between start and end
#'
#' @return color vector the size of into with colors evenly spaced apart
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

#' @title generate desired color gradient
#'
#' @description Used to generate desired color gradiants in ggplot. This is
#' intended to be used with scale_fill_gradientn function. Find colors codes
#' at: http://htmlcolorcodes.com/
#'
#' @name gradiant.color.range
#'
#' @param value the numeric vector passed in ggplot that the user wishes to
#' map to a gradient scale
#' @param colors character string vector of hexidecimal color codes wished to
#' use. Colors will be assigned from least to greatest of the value vector
#' @param cbreaks vector equal length of colors, indicates at what value the
#' user desires the color of that index to be.
#' @param tol numeric of the level of tolerance of how close the colors will
#' be to assigned cbreaks. Smaller tolerance will return a larger parsing.
#' default set to 0.05 Adjustments may be necessary depending on the scale of
#' value vector.
#' @param report logical vector used to control if value estimates should be
#' reported. Default set to FALSE. This function should not be used with
#' scale_fill_gradientn if this param is set to TRUE.
#' @return character string vector of hexidecimal codes of arbitrary length.
#' This will be understood by scale_fill_gradientn functions 'colours' parameter.
#' @examples
#'  df <- as.data.frame(cbind(rnorm(n = 100,mean = 0,sd = 20),
#'                            rep(c("A","B"), each=50),
#'                            rep(1:50, 2)))
#'  colnames(df) <- c("value","f1","f2")
#'  df$value <-as.numeric(as.character(df$value))
#'  df$f1 <- as.factor(df$f1)
#'  df$f2 <- as.factor(df$f2)
#'  ggplot(df, aes(x=f1,y=f2)) +
#'    geom_tile(aes(fill=value)) +
#'    scale_fill_gradientn(colours = gradiant.color.range(value = df$value,
#'                                                        colors = c("#A700FB",
#'                                                                   "#1700FB",
#'                                                                   "#14F3ED",
#'                                                                   "#FFFFFF",
#'                                                                   "#E0DD23",
#'                                                                   "#E09623",
#'                                                                   "#E03123"),
#'                                                        cbreaks = c(-40, -20,-10,0,10,20,40)),
#'                         breaks= c(-40, -20,-10,0,10,20,40))
#' @export
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


  for(i in n:14000) { #find the index at which the range is sufficently parsed to put the color vector at desired cbreaks

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
