

#' @title mean.shift
#'
#' @name mean.shift
#'
#' @param vec numeric vector passed
#'
#' @return returns data mean centered
mean.shift <- function(vec) {
  x.bar <- mean(vec)
  vec.return <- vec - x.bar
  return(vec.return)
}
#' @title mean.shift.inv
#'
#' @name mean.shift.inv
#'
#' @param vec numeric vector passed
#'
#' @return returns data invereted mean centered
mean.shift.inv <- function(vec) {
  x.bar <- mean(vec)
  vec.return <- x.bar - vec
  return(vec.return)
}

#' @title  just.pca
#'
#' @description Takes Tidy data and Generates pca.object that retains
#' information of original data.
#'
#' @name just.pca
#'
#' @param tidydata Tidy data input
#' @param y column name(s) or index(es) of columns whose interaction is used
#' as y component of formula in dcast's formula = y~x. y or the interaction
#' of columns passed to y becomes the 'individuals' of the pca. This interaction
#' forms an 'id.col' that is added to the original data frame and used to join
#' pca components and the original data frame.
#' @param x column name(s) or index(es) of columns whose interaction is used
#' as y component of formula in dcast's formula = y~x. x or the interaction
#' of columns passed to x becomes the 'variables' of the pca.
#' @param value.var Character string of name of column which stores values.
#' @param scale Logical, scales the variables to have the same unit variance.
#' Default set to FALSE.
#' @param num.degrees Integer that determines the number of principal components
#' to join to the orginal data frame. Default set to 5.
#' @param invert Logical, determines if mean centering should be inverted or not.
#' Default set to FALSE.
#'
#' @return returns pca object that is a list of "data","loadings","values",
#' "covariance","contribution"
#'
#' @export
just.pca <- function(tidydata, y,x,value.var,scale = FALSE, num.degrees = 5, invert = FALSE) {
  #Most of this code is recreating popular PCA functions to understand what is happening under the hood

  tidycoln <- colnames(tidydata)
  y.index <- index.o.coln(vec = y, v.size = length(y), v.name = "y", name.col = tidycoln)
  x.index <- index.o.coln(vec = x, v.size = length(x), v.name = "x", name.col = tidycoln)
  val.index <- index.o.coln(vec = value.var, v.size = 1, v.name = "value.var", name.col = tidycoln)

  if(length(y.index)==1) {
    colnames(tidydata)[y.index] <- "id.col"
  } else {
    tidydata$id.col <- interaction(tidydata[,y.index])
    y.index <- ncol(tidydata)
  }
  if(length(x.index)==1) {
    colnames(tidydata)[x.index] <- "variable.col"
  } else {
    tidydata$variable.col <- interaction(tidydata[,x.index])
    x.index <- ncol(tidydata)
  }
  tidydata <- as.data.frame(tidydata)
  tidycoln <- colnames(tidydata)
  tidy.input <- unique(tidydata[,c(y.index,x.index, val.index)])
  matrix <- dcast(data = tidy.input,formula = id.col~variable.col, value.var = value.var )
  tidydata <- tidydata[,-c(x.index, val.index)]
  tidydata <- as.data.frame(unique(tidydata))
  colnames(tidydata) <- tidycoln[-c(x.index,val.index)]
  #other.index <- index.o.coln(vec = Column.ex, v.size = length(Column.ex), v.name = "Column.ex",name.col = colnames(matrix))
  #other.columns <- tidydata[,]
  #matrix.df <- tidydata[,-(index)]

  if(length(unique(rownames(matrix)))!=nrow(matrix)){
    stop("row names are not unique!")
  }

  rownames(matrix) <- matrix$id.col
  matrix <- matrix[,-1]
  if(invert==T) {
    matrix.df.work <- apply(X = matrix, MARGIN = 2,FUN = mean.shift.inv)
  } else{
    matrix.df.work <- apply(X = matrix, MARGIN = 2,FUN = mean.shift)
  }


  #testing scale
  if(scale==TRUE){
    rnames <- rownames(matrix.df.work)
    matrix.df.work <- apply(X = matrix.df.work, MARGIN = 2, FUN = scale)
    rownames(matrix.df.work) <- rnames
  }

  #covariance
  my.cov = cov(matrix.df.work)
  points.names <- rownames(my.cov)
  #eigen values
  my.eig = eigen(my.cov)
  n <- ncol(my.eig[["vectors"]])
  PC <- vector(mode = "character", n)
  for(i in 1:n) {
    PC[i] <- paste("PC",i,sep = "")
  }
  colnames(my.eig$vectors) <- PC
  rownames(my.eig$vectors) <- points.names
  loadings <- my.eig$vectors
  values <- my.eig$values
  new <- matrix.df.work%*%loadings
  new <- new[,-c((num.degrees+1):n)]
  new <- as.data.frame(new) %>%
    mutate(id.col=rownames(new))

  df <- inner_join(tidydata, new, by = c("id.col"))
  contrib <- loadings*loadings
  object <- list(df, loadings, values, my.cov, contrib)
  names(object) <- c("data","loadings","values","covariance","contribution")

  return(object)

}

#' @title  plotContrib
#'
#' @description plots the contribution of variables in pca in ggplot
#'
#' @name plotContrib
#'
#' @param pca.obj Takes pca object made by just.pca()
#' @param PC Integer input of which PC dimension should be plotted
#' @param first Integer input. Returns the first 10 most contributing
#' variables by default.
#'
#' @return ggplot bar plot of the the most contributing variables to
#' a PC dimension.
#'
#' @export
plotContrib <- function(pca.obj, PC, first = 10) {
  contrib <- pca.obj$contribution
  contrib <- as.data.frame(contrib[,PC])
  colnames(contrib) <- "Contribution"
  contrib$names <- rownames(contrib)
  contrib <- contrib[order(contrib$Contribution),]
  order.names <- contrib[order(contrib$Contribution, decreasing = T),]$names
  contrib$names <- factor(contrib$names, levels = order.names)
  order.names <- order.names[c(1:first)]
  contrib <- contrib[contrib$names %in% order.names,]
  contrib[,1] <- contrib[,1]*100
  # contrib <- count.to.frequency(df = contrib, count = 1)
  #contrib <- drop.levels(contrib)
  p <- ggplot(contrib, aes(x = names, y = Contribution)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(list(y = "Contribution (%)", x = "Variables")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = -90, hjust =0, vjust = .5, size = 8))


  return(p)
}

#' @title neg.square
#'
#' @name neg.square
#'
#' @param x numeric vector passed
#'
#' @return returns the squares of x, but retains negative sign
neg.square <- function(x) {
  neg <- ifelse(x<0, -1, 1)
  x <- x*x
  x <- x*neg

  return(x)
}

#' @title scale.abs.max
#'
#' @name scale.abs.max
#'
#' @param x numeric vector passed
#'
#' @return returns x scaled by the absolute max
scale.abs.max <- function(x) {
  m <- max(abs(x))
  x <- x/(m)
  return(x)
}

#' @title circleFun
#'
#' @name circleFun
#'
#' @param center coordinate points for the center of circle
#' @param diameter diameter length of circle, 2r
#' @param npoints number of points to estimate
#'
#' @source https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2/6863490#6863490 from Joran
#'
#' @return returns data frame to draw circle points.
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

#' @title to.unit
#'
#' @description Takes cartesian coordiants and returns the unit circle
#' points as x, y coordinates.
#'
#' @name to.unit
#'
#' @param x numeric vector passed of length 2
#'
#' @return returns x y cordinates in polar form
#'
#' @export
to.unit <- function(x) {
  neg <- FALSE
  if(length(x)!=2) {
    stop()
  }
  if(x[1]<0 && x[1]!=0) {
    neg <- TRUE
  }
  if(sum(abs(x))==0) {
    x <- c(0,0)
    return(x)
  } else {
    x.hat <- cos((atan(x[2]/x[1])))
    y.hat <- sin((atan(x[2]/x[1])))
    x[1] <- x.hat
    x[2] <- y.hat
    if(neg==TRUE) {
      x <- x*(-1)
    }
    return(x)
  }

}

#' @title  pca.biplot
#'
#' @description Returns biplot of two pc dimensions
#'
#' @name pca.biplot
#'
#' @param pca.obj Takes pca object made by just.pca()
#' @param pcs Integer input vector of length 2 of which PC dimensions
#' should be plotted
#' @param flip Logical, Default set to FALSE. will flip coordinates
#' over x=y line. Only included because PCA function's biplot seems to
#' always be flipped.
#' @param scale Logical, Default set to TRUE. Will scale vectors to unit
#' circle. If set to FALSE, will use eigenvalues as relative length. Scale
#' set to FALSE will also remove regular coordinates and plotted circle.
#'
#' @return ggplot plot of the variables in a coordinate graph of two
#' pc dimensions.
#'
#' @export
pca.biplot <- function(pca.obj, pcs, flip = FALSE, scale = T) {
  index <- index.o.coln(vec = pcs, v.size = 2, v.name = "pcs",name.col = colnames(pca.objec$loadings))
  weights <- pca.obj$loadings[,index]


  unit.dir <- t(apply(weights, MARGIN = 1, to.unit))

  weights[,1] <- weights[,1]*sqrt(pca.obj$values[index[1]])
  weights[,2] <- weights[,2]*sqrt(pca.obj$values[index[2]])
  sq <- sqrt((weights[,1]^2)+(weights[,2]^2))

  percent.var <- round((pca.obj$values/(sum(pca.obj$values)))*100,digits = 1)
  #squares <- neg.square(weights)

  if(scale==T) {
    scale.sq <- sq/(max(sq))
    #weights <- apply(weights, MARGIN = 2, scale.abs.max)
    weights <- unit.dir*scale.sq
  } else {
    # unit.dir[,1] <- unit.dir[,1]*sqrt(pca.obj$values[index[1]])
    # unit.dir[,2] <- unit.dir[,2]*sqrt(pca.obj$values[index[2]])
    weights <- unit.dir*sq
  }

  col.temp <- colnames(weights)
  colnames(weights) <- c("V1","V2")
  weights <- as.data.frame(weights)
  if(flip==TRUE){
    weights <- weights*-1
  }
  weights$names <- rownames(weights)
  #library(ggrepel)
  p <- ggplot(weights, aes(x = V1, y = V2,label=names)) +
    geom_point(position = position_jitter()) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_segment(mapping=aes(x=0,
                             y=0,
                             xend=V1,
                             yend=V2),
                 arrow=arrow(angle = 10,
                             length = unit(.05,
                                           units = "npc")),
                 size=.1, color="blue")

  if(scale==T) {
    p <- p + coord_cartesian(xlim = c(-1,1),
                             ylim = c(-1,1)) +
      geom_polygon(aes(x, y),
                   data = circleFun(center = c(0,0),
                                    diameter = 2,
                                    npoints = 100),
                   colour = "black",
                   fill = NA,
                   inherit.aes = F)
  }
  p <- p +
    geom_label_repel()+
    theme_classic() +
    labs(list(x = paste(col.temp[1],
                        " (",
                        percent.var[index[1]],
                        " %)",
                        sep = ""),
              y = paste(col.temp[2],
                        " (",
                        percent.var[index[2]],
                        " %)",
                        sep = "")))
  return(p)
}


#' @title  plot3d_pca
#'
#' @description Returns pca scatter plot in 3d that can be moved
#'
#' @name plot3d_pca
#'
#' @param pca.obj Takes pca object made by just.pca()
#' @param coord character string or numb of PC dimensions to use. Must
#' be length 3.
#' @param color Factor to color.
#' @param size Size of the dots made
#' @param brewer.name Selets brewer pallete by brewer.pal name
#'
#' @return 3D movable scatter plot of selected PCs
#'
#' @export
plot3d_pca <-function(pca.object, coord, color, size = 5, brewer.name = "Spectral") {

  #import rgl
  options(rgl.printRglwidget = TRUE)

  percent.var <- round((pca.object$values/(sum(pca.object$values)))*100,digits = 1)
  data <- pca.object$data

  pca.indx <- index.o.coln(vec = coord, v.size = 3, v.name = "coord", name.col = colnames(data))
  col.temp <-colnames(pca.object$loadings)
  index <- index.o.coln(vec = coord, v.size = 3, v.name = "coord", name.col = col.temp)
  color.indx <- index.o.coln(vec = color, v.size = 1, v.name = "color", name.col = colnames(data))

  df.work <- data[,c(pca.indx,color.indx)]
  #colors <- color.map(color.code = "#FF8900", n = nlevels(data[,color.indx]))
  data[,color.indx] <- as.factor(data[,color.indx])
  n <- nlevels(data[,color.indx])
  colors <- brewer.pal(n = n, name = brewer.name)

  ptest <- plot3d(x = data[,pca.indx[1]],
                  y = data[,pca.indx[2]],
                  z = data[,pca.indx[3]],
                  col = colors[data[,color.indx]],
                  size = size,
                  xlab = paste(col.temp[index[1]],
                               " (",
                               percent.var[index[1]],
                               " %)",
                               sep = ""),
                  ylab = paste(col.temp[index[2]],
                               " (",
                               percent.var[index[2]],
                               " %)",
                               sep = ""),
                  zlab = paste(col.temp[index[3]],
                               " (",
                               percent.var[index[3]],
                               " %)",
                               sep = ""))
  return(ptest)
}
