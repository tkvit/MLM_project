#Functions supports for MML project

doPlots <- function(data.in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data.in=data.in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotDensity <- function(data.in, i) {
  data <- data.frame(x=data.in[,i], Response=data.in$diagnosisgroup)
  p <- ggplot(data) + geom_density(aes(x=x, colour=factor(Response))) +
    #geom_line(aes(x=x), stat="density", size=1, alpha=1.0) +
    xlab(colnames(data.in)[i]) + theme_light()#+theme(axis.title.x = element_blank())
  return (p)
}

plotBox <- function(data.in, i) {
  data <- data.frame(y=data.in[,i], Response=data.in$diagnosisgroup)
  p <- ggplot(data, aes(x=factor(Response), y=y)) + geom_boxplot() +
     ylab(colnames(data.in)[i]) + theme_light() + theme(axis.title.x = element_blank())
  return (p)
}

cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}