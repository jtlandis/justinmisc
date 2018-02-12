#functions used for some analysis

#' @title perform 2^k analysis on factors
#'
#' @description will report the effects of various factors of two levels
#'
#' @name twokeffect
#'
#' @param df data frame passed containing value vector and factors
#' @param v.col.n positive integer that describes the index of the response vector
#' @param fact.col.n vector of positive integer(s) that describes the index of the
#' factors to be used
#' @param interaction Logical, augments if interaction between factor levels will
#' be reported. Default is set to FALSE
#'
#' @return data frame of effects
#'
#' @export
twokeffect <- function(df,v.col.n,fact.col.n, interaction=F) {

  if((v.col.n%%1) != 0){
    stop("v.vol.n must be a integer")
  }
  n <- length(fact.col.n)
  if(any(fact.col.n%%1 != 0)) {
    stop("fact.col.n must be an integer vector")
  }
  if(n <2) {
    stop("fact.col.n must be of at least length 2.")
  }
  if(!is.logical(interaction)) {
    stop("interaction must be a logical vector. Assign 'TRUE' to also get interactions between factors")
  }
  colsize <- n + 1
  sub<- vector("integer", colsize)
  sub[1] <- v.col.n
  for(i in 2:colsize) {
    sub[i] <- fact.col.n[i-1]
  }
  df <- as.data.frame(df[,sub])
  cnames <- as.character(colnames(df))
  if(!is.numeric(df[,1])) {
    stop("value must be a numeric vector")
  }
  for(j in 2:colsize){
    if(!is.factor(df[,j])) {
     stop(paste(cnames[j], " must be a factor.", sep = ""))
    }
    if(nlevels(df[,j])!=2) {
      stop(paste(cnames[j], " must be a factor of only 2 levels.", sep = ""))
    }
    if((length(unique(df[,j]))!=2)) {
      stop(paste(cnames[j], " must be a factor of only 2 levels.", sep = ""))
    }

  }
  index1 <- 1
  index2 <- 0

  if(interaction==TRUE) {
    effect <- vector("numeric", (choose(n,2)+(n)))
    result <- vector("character", (choose(n,2)+(n)))
    rnames <- vector("character", (choose(n,2)+(n)))
    for(k in 2:(colsize-1)){
      for(l in (3+index2):(colsize)){
        x.fctrs <- as.character(unique(df[,k]))
        y.fctrs <- as.character(unique(df[,l]))
        cmb.mean1 <- mean(df[((df[,k]==x.fctrs[1])&(df[,l]==y.fctrs[1])|(df[,k]==x.fctrs[2])&(df[,l]==y.fctrs[2])),1])
        cmb.mean2 <- mean(df[((df[,k]==x.fctrs[2])&(df[,l]==y.fctrs[1])|(df[,k]==x.fctrs[1])&(df[,l]==y.fctrs[2])),1])
        if(is.nan(cmb.mean1)|is.nan(cmb.mean2)){
          effect[n+index1] <-NA
        } else {
          if(cmb.mean1>cmb.mean2) {
            effect[n+index1] <- cmb.mean1-cmb.mean2
          } else {
            effect[n+index1] <- cmb.mean2-cmb.mean1
          }
        }
        result[n+index1] <- paste("The interaction difference between these four factors is ", round(effect[n+index1]),".", sep = "")
        rnames[n+index1] <- paste(cnames[k], " vs ", cnames[l], sep = "")
        index1 <- index1 + 1
      }
      index2 <- index2 +1
    }
  } else{
    effect <- vector("numeric", n)
    result <- vector("character", n)
    rnames <- vector("character", n)
  }


  for(m in 2:colsize){
    fctrs <- as.character(unique(df[,m]))
    mean1 <- mean(df[df[,m]==fctrs[1],1])
    mean2 <- mean(df[df[,m]==fctrs[2],1])

    if(mean1>mean2) {
      effect[m-1] <- mean1-mean2
      result[m-1] <- paste("On average, switching from ", fctrs[2], " to ", fctrs[1], " increases ", cnames[1], " by ", round(effect[m-1]), ".", sep = "")
      rnames[m-1] <- cnames[m]
    } else {
      effect[m-1] <- mean2-mean1
      result[m-1] <- paste("On average, switching from ", fctrs[1], " to ", fctrs[2], " increases ", cnames[1], " by ", round(effect[m-1]), ".", sep = "")
      rnames[m-1] <- cnames[m]
    }
  }

  vec <- cbind(effect, result)
  vec <- as.data.frame(vec)
  colnames(vec) <- c("Effects","Results")
  vec$Effects <- as.numeric(as.character(vec$Effects))
  rownames(vec) <- rnames
  return(vec)
}

#' @title Reports a balanced sample of a data frame's factors
#'
#' @description will return a balancd data frame subset of the original
#'
#' @name balance.sample
#'
#' @param df data frame passed containing value vector and factors
#' @param v.col.n positive integer that describes the index of the response vector
#' @param fact.col.n vector of positive integer(s) that describes the index of the
#' factors to be used
#' @param replace Not yet working
#' @param min.samp.numb Can take a positive integer, only subsets of factors
#' greater than or equal to this number will be reported
#'
#' @return Balanced data frame that is a subset of the first
#'
#' @export
balance.sample <- function(df,v.col.n,fact.col.n, replace=FALSE, min.samp.numb = "min") {
  if((v.col.n%%1) != 0){
    stop("v.vol.n must be a integer")
  }

  n <- length(fact.col.n)
  if(any(fact.col.n%%1 != 0)) {
    stop("fact.col.n must be an integer vector")
  }
  # if(n <2) {
  #   stop("fact.col.n must be of at least length 2.")
  # }
  if(!is.logical(replace)) {
    stop("replace must be a logical vector.")
  }
  colsize <- n + 1
  sub<- vector("integer", colsize)
  sub[1] <- v.col.n
  for(i in 2:colsize) {
    sub[i] <- fact.col.n[i-1]
  }
  df <- df[,sub]
  #df$id <- interaction(df[,-1])
  df <- as.data.frame(df)
  cnames <- as.character(colnames(df))
  if(!is.numeric(df[,1])) {
    stop("value must be a numeric vector")
  }
  for(j in 2:colsize){
    if(!is.factor(df[,j])) {
      stop(paste(cnames[j], " must be a factor.", sep = ""))
    }
    if(nlevels(df[,j])<2) {
      stop(paste(cnames[j], " must be a factor of at least 2 levels.", sep = ""))
    }
    if((length(unique(df[,j]))<2)) {
      stop(paste(cnames[j], " must be a factor of at least 2 levels.", sep = ""))
    }

  }

  tab <- table(df[,-1])
  if(!any(min.samp.numb %in% c("max","min"))) {
    if((min.samp.numb%%1)!=0){
      stop("samp.numb must be either max or min. or an integer")
    } else{
      tab.temp <-  as.data.frame(tab)
      a <- ncol(tab.temp)
      tab.temp <- tab.temp[tab.temp[,a]>=min.samp.numb,]
      tab.temp$id <- interaction(tab.temp[,-a])
      temp.df <- df
      temp.df$id <- interaction(temp.df[,-1])

        temp.df <-temp.df[temp.df[,ncol(temp.df)] %in% tab.temp[,ncol(tab.temp)],]

      temp.df <- drop.levels(temp.df)
      df <- temp.df[,-ncol(temp.df)]
      sample.n <- min(tab.temp[,a])
    }
  } else{
    if(min.samp.numb=="min") {
      sample.n <- min(tab)
    }
  }

  if(sample.n==0){
    stop("Some combination of factors passed has no samples to pull from. \n Consider passing integer to min.samp.numb.")
  }

  level.comp <-1
  for(a in 2:colsize){
    level.n.temp <- nlevels(df[,a])*level.comp
    level.comp <- level.n.temp
  }
  #alpha <- level.comp*sample.n

  factor.id.df <- as.data.frame(unique(df[,-1]))
  #factor.id.df$id <- interaction(factor.id.df)
  #browser()
  vec <- c()
  for(i in 1:nrow(factor.id.df)){
    temp.df <- df
    for(j in 1:ncol(factor.id.df)){
      temp.df <- temp.df[temp.df[,j+1]==factor.id.df[i,j],]
    }
    if(replace == T){
      v <- sample(x = temp.df[,1],size = sample.n, replace = replace)
    } else {
      v <- sample(x = temp.df[,1],size = sample.n)
    }

    v <- temp.df[temp.df[,1]%in% v,] #max and replace not working as intended because this will not repeat repeated values
    vec <- rbind(vec, v)
  }
  vec <- as.data.frame(vec)
  return(vec)
}

#' @title generate combinations
#'
#' @description Generate a dataframe of the combinations
#'
#' @name comb.comp
#'
#' @param vector factor vector of at least 2
#' @param rtrn.all logical, controlled internally
#' @return dataframe
comb.comp <- function(vector, rtrn.all = FALSE){
  if(!is.factor(vector)){
    stop("ERROR: at least one column is not a vector of factors")
  }
  n <- nlevels(vector)
  if(n<2) {
    stop("Number of factor levels should at least be 2")
  }
  lvl.names <- levels(vector)
  numb.comb <- choose(n,2)
  rep.seq <- seq(from = n-1, to = 0, by = -1)
  factor1 <- rep(lvl.names, rep.seq)
  factor2 <- rep("factor", length(factor1))

  j <-1
  index <- 0
  for(i in 1:length(factor1)){
    if(j==n){
      index <- index +1
      j <-1 +index
    }
    j <- j + 1
    factor2[i]<- lvl.names[j]
  }

  comp.f <- cbind(factor1,factor2)
  comp.f <- as.data.frame(comp.f)
  colnames(comp.f) <- c("f1","f2")

  if(rtrn.all==TRUE){
  comp.f <- comp.f %>%
    mutate(comb = paste(f1,".v.",f2, sep = ""))
  }

  return(comp.f)
}


#' @title Wilcoxon tests through many factors
#'
#' @description Calculate the wilcoxon test between several factors
#'
#' @name combo.wilcox
#'
#' @param df data frame passed containing value vector and factors
#' @param response positive integer that describes the index of the response vector
#' @param factor vector of positive integer(s) that describes the index of the
#' factors to be used. Number of factors passed changes the comparison. One factor
#' vector passed will return wilcox test values between levels. Multiple factor
#' vectors passed will return wilcon test values between all combination of interactions
#' of factors.
#' @param p.v.correct Logical assignment. Default set to FALSE. Will return logical vector
#' along with test results. TRUE indicates P.value is less than adjusted P.value, FALSE
#' indicates P.value is greated than adjusted P.value. Adjusted P.value is calculated using
#' the bonferroni correction.
#' @param single.factor.comp logical assignment. Default set to FALSE. Set to TRUE when you
#' user wants compare between two levels of the same factor while all other factors are
#' held constant. When set to TRUE, factor vector must be at least length 2.
#'
#' @return dataframe of wilcoxn test P.value results
#'
#' @export
combo.wilcox <- function(df, response, factor, p.v.correct= FALSE, single.factor.comp = FALSE) {

  #browser()
  if((response%%1) != 0){
    stop("response must be a integer")
  }
  n <- length(factor)
  if(any(factor%%1 != 0)) {
    stop("factor must be an integer vector")
  }
  if(any(factor<0)) {
    stop("factor must contain positive intiger(s)")
  }
  if(!is.logical(p.v.correct)) {
    stop("interaction must be a logical vector. Assign 'TRUE' to also get interactions between factors")
  }
  if(n >= 2) { #if n is less than 2, function should assume all factor comparisons
               #that want to be made are in this single vector and skip this edit.
               #if greater than 2, colapse into 1 factor via interaction
               # will also allow for single factor comparisons
    colsize <- n + 1
    sub<- vector("integer", colsize)
    sub[1] <- response
    for(i in 2:colsize) {
      sub[i] <- factor[i-1]
    }
    df.work <- as.data.frame(df[,sub])
    cnames <- as.character(colnames(df.work))
    for(j in 2:colsize){
      if(!is.factor(df.work[,j])) {
        stop(paste(cnames[j], " must be a factor.", sep = ""))
      }
      if(nlevels(df.work[,j])<2) {
        stop(paste(cnames[j], " must be a factor of at least 2 levels.", sep = ""))
      }
      if((length(unique(df.work[,j]))<2)) {
        stop(paste(cnames[j], " must be a factor of at least 2 levels.", sep = ""))
      }

    }
    #add single.factor.comp variable here...
    #build a for loop to loop over factor vectors passed, and then through factors
    #build either the df that is needed, or just a vector of desired interactions and subset from df later.

    df.work <- df.work %>%
      mutate(int=interaction(df.work[,-1]))
    df <- df.work

    if(single.factor.comp==TRUE){

      combo.vec <- comb.comp(vector = df[,2])
      combo.vec <- cbind(rep(2,nrow(combo.vec)), combo.vec)
      colnames(combo.vec) <- c("column.index","f1","f2")
      for(m in 3:colsize){
        combo.vec.work <- comb.comp(vector = df[,m])
        combo.vec.work <- cbind(rep(m,nrow(combo.vec.work)), combo.vec.work)
        colnames(combo.vec.work) <- c("column.index","f1","f2")
        combo.vec <- rbind(combo.vec, combo.vec.work)
      }

      combo.vec <- as.data.frame(combo.vec)

      vec <- c()
      for(o in 1:nrow(combo.vec)){
      df.temp1 <- df.work[df.work[,combo.vec[o,1]] == as.character(combo.vec[o,2]) | df.work[,combo.vec[o,1]] == as.character(combo.vec[o,3]),]
      df.temp1$temp <- interaction(df.temp1[,c(-1,-(combo.vec[o,1]),-(colsize+1))])
      temp.names <- levels(df.temp1$temp)

        df.z <- c()
        for(z in 1:nlevels(df.temp1$temp)){
          df.z.work <- df.temp1[df.temp1$temp == temp.names[z],]
          #df.z <- rbind(df.z,df.z.work)
          df.z.work <- drop.levels(df.z.work)
          wil.work <- wilcox.test(df.z.work[,1]~df.z.work[,combo.vec[o,1]])
          new.df.z <- cbind(as.character(paste(cnames[combo.vec[o,1]])),
                            as.character(paste(levels(df.z.work[,combo.vec[o,1]])[1],
                                               ".v.",
                                               levels(df.z.work[,combo.vec[o,1]])[2],sep = "")),
                                        as.character(df.z.work[1,ncol(df.z.work)]),
                                        wil.work[[3]])
          colnames(new.df.z) <- c("Tested Factor","Tested Levels", "Constant Levels", "P.value")
          df.z <- rbind(df.z,new.df.z)
        }
        vec <-  rbind(vec,df.z)
      }
      vec <- as.data.frame(vec)
      vec$P.value <- as.numeric(as.character(vec$P.value))
      if(p.v.correct==TRUE){
        adjusted <- (0.05)/(nrow(vec))
        vec <- vec %>%
          mutate(Sig.=ifelse(P.value<adjusted, TRUE, FALSE))
      }
      return(vec)
    }else{
      df <- as.data.frame(df.work[,c(1,ncol(df.work))])
    }
  } else{
    df <- as.data.frame(df[,c(response,factor)])
  }
  if(!is.numeric(df[,1])) {
    stop("response must be a numeric vector")
  }
  df <- drop.levels(df)
  combo.vec <- comb.comp(vector = df[,2], rtrn.all = TRUE)
  ncomp.made <- nrow(combo.vec)
  vec <- vector("numeric", ncomp.made)
  names.c <-  vector("character", ncomp.made)
  for(k in 1:ncomp.made){
    df.temp <- df[df[,2]==as.character(combo.vec[k,1])|df[,2]==as.character(combo.vec[k,2]),]
    df.temp <- drop.levels(df.temp)
    wil.work <- wilcox.test(df.temp[,1]~df.temp[,2],data=df.temp)
    vec[k] <- wil.work[[3]]
    names.c[k] <- combo.vec[k,3]
  }

  vec <- cbind(names.c,vec)
  vec <- as.data.frame(vec)
  colnames(vec) <- c("Tested Levels","P.value")
  vec$P.value <- as.numeric(as.character(vec$P.value))
  if(p.v.correct==TRUE){
    adjusted <- (0.05)/(ncomp.made)
    vec <- vec %>%
      mutate(Sig.=ifelse(P.value<adjusted, TRUE, FALSE))
  }
  return(vec)
}

#' @title Three factor contrast anova
#'
#' @description Generate the anova test statistics for three factors
#'
#' @name contrast.aov3
#'
#' @param df data frame passed containing value vector and factors
#' @param response positive integer that describes the index of the response vector
#' @param factor3 positive integer vector of length three that describes the index
#' of three factor vectors within the passed df.
#'
#' @return return dataframe of anova test statistic
#'
#' @export
contrast.aov3 <- function(df, response, factor3){

  if((response%%1) != 0){
    stop("response must be a integer")
  }
  n <- length(factor3)
  if(any(factor3%%1 != 0)) {
    stop("factor3 must be an integer vector")
  }
  if(n!=3) {
    stop("factor3 must be of length 3.")
  }
  colsize <- n + 1
  sub<- vector("integer", colsize)
  sub[1] <- response
  for(i in 2:colsize) {
    sub[i] <- factor3[i-1]
  }
  df <- as.data.frame(df[,sub])
  cnames <- as.character(colnames(df))
  if(!is.numeric(df[,1])) {
    stop("value must be a numeric vector")
  }
  for(j in 2:colsize){
    if(!is.factor(df[,j])) {
      stop(paste(cnames[j], " must be a factor.", sep = ""))
    }
    if(nlevels(df[,j])<2) {
      stop(paste(cnames[j], " must be a factor of at least 2 levels.", sep = ""))
    }
    if((length(unique(df[,j]))<2)) {
      stop(paste(cnames[j], " must be a factor of at least 2 levels.", sep = ""))
    }

  }

  test <- df
  cnames <- colnames(test)[-1]
  .t1 <- summary(aov(test[,1]~test[,2]+test[,3]+test[,4]))[[1]]
  .t1 <- .t1 %>%
    mutate(names=rownames(.t1)) %>%
    mutate(id="2,3,4")
  .t2 <- summary(aov(test[,1]~test[,2]+test[,4]+test[,3]))[[1]]
  .t2 <- .t2 %>%
    mutate(names=rownames(.t2))%>%
    mutate(id="2,4,3")
  .t3 <- summary(aov(test[,1]~test[,3]+test[,2]+test[,4]))[[1]]
  .t3 <- .t3 %>%
    mutate(names=rownames(.t3))%>%
    mutate(id="3,2,4")
  .t4 <- summary(aov(test[,1]~test[,3]+test[,4]+test[,2]))[[1]]
  .t4 <- .t4 %>%
    mutate(names=rownames(.t4))%>%
    mutate(id="3,4,2")
  .t5 <- summary(aov(test[,1]~test[,4]+test[,2]+test[,3]))[[1]]
  .t5 <- .t5 %>%
    mutate(names=rownames(.t5))%>%
    mutate(id="4,2,3")
  .t6 <- summary(aov(test[,1]~test[,4]+test[,3]+test[,2]))[[1]]
  .t6 <- .t6 %>%
    mutate(names=rownames(.t6))%>%
    mutate(id="4,3,2")
  test <- rbind(.t1,.t2,.t3,.t4,.t5,.t6)
  test <- dcast(data=test, names~id,value.var = "Pr(>F)")
  rownames(test) <- c("Residuals", cnames)
  return(test)
}

#' @title standard hclust function
#'
#' @description internal clustering function to automatically
#' reorder factors
#'
#' @name clust
#'
#' @param df data frame passed
#' @param col.index index columns of df that will be clustered
#' @param value.var value by which clustering occures
#'
#' @return df but with reordered factors
clust <- function(df, col.index, value.var) {

  df.wide <- dcast(df, formula = df[,col.index[1]]~df[,col.index[2]], value.var = value.var)
  row.names(df.wide) <- df.wide[,1]
  df.wide <- df.wide[,-1]
  a <- dist((df.wide))
  hc <- hclust(a)

  .test <- as.data.frame((df.wide))
  .test <- .test %>%
    mutate(names = rownames(.test))
  c <- .test[hc$order,]$names

  df[,col.index[1]] <- factor(df[,col.index[1]], levels = c)
  return(df)
}

#' @title euclidian clustering function
#'
#' @description reorder factors passed to y.by.x via their respective
#' clusters
#'
#' @name distance.cluster
#'
#' @param df data frame passed containing both factor columns to reorder
#' and numeric column to cluster by
#' @param y.by.x Index of the columns or character column
#' names to reorder
#' @param value.var Character column name by which clustering will be
#' calculated
#' @param which.clut Defines which column is reordered. Default set to both.
#' Set to "y" to only reorder y.by.x[1], set to "x" to only reorder y.by.x[2]
#'
#' @return returns df but with reordered factors
#'
#' @export
distance.cluster <- function(df, y.by.x, value.var,which.clust = "both") {


  if(length(y.by.x)!=2){
    stop("y.by.x must be length 2")
  }
  df <- drop.levels(df)
  name.col <- colnames(df)
  if(is.character(y.by.x)|is.numeric(y.by.x)) {
    if(is.character(y.by.x)) {
      if(any(name.col==y.by.x)) {
        index<-which(name.col==y.by.x)
      } else {
        stop("character does not any column name")
      }

    }
    if(is.numeric(y.by.x)) {
      if(all((y.by.x%%1)==0)) {
        index <- y.by.x
      } else {
        stop("Numbers must be integers of columns of interest")
      }
    }

  } else {
    stop("y.by.x must be character vector of column name or column index")
  }

  if(!any(which.clust==(c("both","y","x")))) {
    stop("specify which factor you wish to cluster by. Default \"both\", or \"y\" or \"x\". ")
  }

  if(which.clust=="both"|which.clust=="y") {
    df <- clust(df = df,col.index = index,value.var = value.var)
  }
  if(which.clust=="both"|which.clust=="x") {
    df <- clust(df = df,col.index = index[c(2,1)],value.var = value.var)
  }

  return(df)

}

