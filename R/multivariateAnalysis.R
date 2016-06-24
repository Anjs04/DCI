

# http://stackoverflow.com/questions/31337922/adding-italicised-r-with-correlation-coefficient-to-a-scatter-plot-chart-in-ggpl
corrEqn <- function(x,y, digits = 2) {
  corr_coef <- round(cor(x, y, method = "spearman"), digits = digits)
  paste("italic(r) == ", corr_coef)
}

generateCorrelation <- function(var1, var2, label) {
  g <- ggplot(data.scaled, aes_string(x=var1, y=var2)) +
    geom_point(shape = 19, size = 2) +    # Use hollow circles
    geom_smooth(colour = "red", fill = "lightgreen", method = lm) +
    geom_text(x = 3, y = 3,
            label = corrEqn(data.scaled[,var1],
                             data.scaled[,var2]), parse = TRUE)

  if (PRINTING) {
 		ggsave(file = paste("./figs/correlation/", label, ".png", sep = ""),
 		  width = png.width,
 		  height = png.height
 		)
 	}
  correlation.test <- cor.test(data.scaled[,var1], data.scaled[,var2])
  return (correlation.test)

}

generateCorrelationsExploratory <- function() {

  cors.index <- cor(data.scaled[,vars.index.no.quals], method = "spearman")
  # cors.index <- cor(data.scaled[,vars.index], method = "spearman")
  var.breaks <- colnames(cors.index)[seq(1, length(colnames(cors.index)), 6)]
  g <- qplot(x=Var1, y=Var2, data=melt(cors.index), fill=value, geom="tile") +
      scale_fill_gradient2(limits=c(-1, 1), low = "#EA3568", mid = "white", high = "#139DEA") +
      labs(x = "All 133 scalar variables", y = "", fill = "Correlations") +
      scale_x_discrete(breaks = var.breaks) +
      scale_y_discrete(breaks = var.breaks) +
      theme(
            # GRID
            # TITLE
            # plot.title = element_text(colour = title.color, lineheight=1.0, face="bold", size=graph.title.size),
            # axis.title = element_text(lineheight=1.0, size = axis.title.size),
            axis.title.x = element_text(size = axis.title.size, vjust = x.axis.vjust),
            axis.title.y = element_text(size = axis.title.size, vjust = y.axis.vjust),

            # LINE
            axis.line = element_line(colour = "black"),
#            axis.ticks = element_blank(),

            # TEXT
            axis.text.x = element_text(color=text.color, angle=45, vjust=1.0, hjust=1.0, size = 6),
            axis.text.y = element_text(color=text.color, angle=45, size = 6),
            axis.title = element_text(color=title.color, lineheight=1.0, size = axis.title.size)
      )


  if (PRINTING) {
		ggsave(file = "figs/correlation/exploratory-matrix.png",
		  width = png.width,
		  height = png.height
		)
	}

  return (g)

}

generateTheme <- function(g) {
  return (theme(
                  # GRID
                  panel.grid.minor.y = element_blank(),
                  # panel.grid.major.y = element_blank(),
                  panel.grid.major.y = element_line(colour = foreground.color),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank(),

                  # BACKGROUND
                  panel.background = element_rect(fill = background.color, colour = foreground.color),

                  # TITLE
                  # plot.title = element_text(colour = title.color, lineheight=1.0, face="bold", size=graph.title.size),
                  axis.title = element_text(color=title.color, lineheight=1.0, size = axis.title.size),
                  # axis.title = element_text(lineheight=1.0, size = axis.title.size),
                  axis.title.x = element_text(size = axis.title.size, vjust = x.axis.vjust),
                  axis.title.y = element_text(size = axis.title.size, vjust = y.axis.vjust),

                  # LINE
                  axis.line = element_line(colour = "black"),

                  # TEXT
                  axis.text.x = element_text(color=text.color, angle=45, vjust=1.0, hjust=1.0, size = axis.text.size),
                  axis.text.y = element_text(color=text.color, angle=45, size = axis.text.size)
                  # axis.text.x = element_text(angle=45, vjust=1.0, hjust=1.0, size = axis.text.size),
                  # axis.text.y = element_text(angle=45, size = axis.text.size)
                  , legend.direction = 'horizontal',
                  legend.position = 'bottom'
              ))
}

generateGraphForPCA <- function(fit) {

  library(ggbiplot)

  g <- ggbiplot(fit, obs.scale = 1, var.scale = 1,
                groups = data$gender,
	              ellipse = TRUE,
	              circle = TRUE, 
                alpha = 0.5,
                var.axes = F,
                varname.size = 6)
  g <- g + generateTheme()
  # g <- g + scale_color_discrete(name = '')
  g <- g + scale_color_manual(name = '', values = yawcrcPalette9)

	return (g)

}

generateScreeForPCA <- function(fit) {

  # Variances of principal components
  sum.variances <- length(fit$sdev)
  variances <- data.frame(variances=fit$relative.variance, pcomp=1:sum.variances)
  # **2 means ^2

  #Plot of variances
  g <- ggplot(variances, aes(pcomp, variances)) + geom_bar(stat="identity", fill="gray") + geom_line()
  g <- g + generateTheme()

  return (g)

}


generateCorrelations_test <- function() {

  cors.totals <- cor(data.scaled[,vars.totals], method = "spearman")
  rownames(cors.totals) <- paste(sapply(rownames(cors.totals), questionCategory), ", [", rownames(cors.totals), "]")
  colnames(cors.totals) <- paste(sapply(colnames(cors.totals), questionCategory), ", [", colnames(cors.totals), "]")
  write.table(cors.totals, "output/cors-totals.csv", col.names=NA, sep = ",")

  cors.index <- cor(data.scaled[,vars.index], method = "spearman")
  write.table(cors.index, "output/cors-index.csv", col.names=NA, sep = ",")

  cors.above.08 <- cor(data.scaled[,vars.index], method = "spearman")
  cors.above.08[cors.above.08 == 1.0] <- 0
  cors.above.08[cors.above.08 <= 0.8] <- 0
  write.table(cors.above.08, "output/cors-index-08.csv", col.names=NA, sep = ",")

  library(Hmisc)
  rcorrs <- rcorr(cors.index, type="spearman")
  p.values <- rcorrs$P
  write.table(p.values, "output/cors-p-values.csv", col.names=NA, sep = ",")

	library(corrplot)
	## Can't write this file to disk
	# library(Cairo)
	# library(cairoDevice)
	# Cairo(surface = c("png"), filename = "figs/correlation-matrix.png")
	corrplot(rcorrs$r, method="circle", order="hclust", addrect=2)
	# dev.off()

	## ggplot version
	qplot(x=Var1, y=Var2, data=melt(cors.index), fill=value, geom="tile") +
   		scale_fill_gradient2(limits=c(-1, 1))
	ggsave(file = "figs/correlation-matrix-1.png", width = 8, height = 6)

	## ggplot version
	qplot(x=Var1, y=Var2, data=melt(rcorrs$r), fill=value, geom="tile") +
   		scale_fill_gradient2(limits=c(-1, 1))
	ggsave(file = "figs/correlation-matrix-2.png", width = 8, height = 6)


	cors.scaled.index <- cor(data.scaled[,vars.index])
	qplot(x=Var1, y=Var2, data=melt(cors.scaled.index), fill=value, geom="tile") +
	  scale_fill_gradient2(limits=c(-1, 1))
	ggsave(file = "figs/correlation-matrix-3.png", width = 8, height = 6)


  # Test regression and multicollinearity
	library(psych)
	library(car)
	library(plyr)
  fit=lm(Q74_1~.,data=data.scaled[,vars.index])
  vif(fit) # variance inflation factors
  sqrt(vif(fit)) > 2 # problem?

	## PCA
  ## http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/

	dt.pca <- prcomp(data.scaled[,vars.totals], center = TRUE, scale. = TRUE)
	# d.pca <- prcomp(data.scaled[,vars.index])
	print(dt.pca)

	# plot method
	plot(dt.pca, type = "l")

	# summary method
	summary(dt.pca)


	comp1 <- dt.pca$rotation[,1]
	n <- length(comp1)
	stack(comp1[order(stack(comp1)$values)])

	printMeanAndSdByGroup(data.scaled,data.scaled[224])

	library("MASS")
	d.lda <- lda(data$age.breaks ~
	               data$total.74
	             + data$total.287
	             + data$total.341
	             + data$total.343
	             + data$total.352
	             + data$total.353
	             + data$total.428
	             + data$total.429
	             + data$total.430
	             + data$total.431
	             + data$total.434
	             + data$total.435
	             + data$total.437)
	d.lda.values <- predict(d.lda, data.scaled)
	ldahist(data = d.lda.values$x[,1], g=data$age.breaks)
	ldahist(data = d.lda.values$x[,2], g=data$age.breaks)
	plot(d.lda.values$x[,1],d.lda.values$x[,2]) # make a scatterplot
	text(d.lda.values$x[,1],d.lda.values$x[,2],data$age.breaks,cex=0.4,pos=4,col="red") # add labels

	d.pca <- prcomp(data.scaled[,vars.index.no.quals], center = TRUE, scale. = TRUE)
	# d.pca <- prcomp(data.scaled[,vars.index])
	print(d.pca)



	# plot method
	plot(d.pca, type = "l")

	# summary method
	summary(d.pca)

	(d.pca$sdev)^2

	d.pca$x

	comp1 <- d.pca$rotation[,1]
  comp2 <- d.pca$rotation[,2]
  comp3 <- d.pca$rotation[,3]
  comp4 <- d.pca$rotation[,4]
	n <- length(comp1)
	stack(comp1[order(stack(comp1)$values)])
  stack(comp2[order(stack(comp2)$values)])
  stack(comp3[order(stack(comp3)$values)])
  stack(comp4[order(stack(comp4)$values)])


  cs1 <- stack(comp1[order(stack(comp1)$values)])
  cs2 <- stack(comp2[order(stack(comp2)$values)])

	plot(d.pca$x[,1], d.pca$x[,2]) # make a scatterplot
	text(d.pca$x[,1], d.pca$x[,2], data$gender, cex=0.4, pos=4, col="red") # add labels

  plot(comp1, comp2)
  text(comp1, comp2, stack(comp1)$ind, cex=0.4, pos=4, col="red")

	library(devtools)
	install_github("ggbiplot", "vqv")

	library(ggbiplot)
	g <- ggbiplot(d.pca, obs.scale = 1, var.scale = 1,
	              groups = data$age.breaks,
                ellipse = TRUE,
	              circle = TRUE)
	g <- g + scale_color_discrete(name = '')
	g <- g + theme(legend.direction = 'horizontal',
	               legend.position = 'top')
	print(g)

	## http://www.statmethods.net/advstats/factor.html
	fit <- princomp(data.scaled[,vars.index], cor=TRUE)
	summary(fit) # print variance accounted for
	lds <- loadings(fit) # pc loadings
	plot(fit,type="lines") # scree plot
	fit$scores # the principal components
	biplot(fit)



	g <- ggbiplot(fit, obs.scale = 1, var.scale = 1,
	              groups = data$gender, ellipse = TRUE,
	              circle = TRUE)
	g <- g + scale_color_discrete(name = '')
	g <- g + theme(legend.direction = 'horizontal',
	               legend.position = 'top')
	print(g)

	# Varimax Rotated Principal Components
	# retaining 5 components
	library(psych)
	fit <- principal(data.scaled[,vars.index], nfactors=4, rotate="varimax")
	fit # print results

	# Maximum Likelihood Factor Analysis
	# entering raw data and extracting 3 factors,
	# with varimax rotation
	fit <- factanal(data.scaled[,vars.index], 5, rotation="varimax")
	print(fit, digits=2, cutoff=.3, sort=TRUE)
	# plot factor 1 by factor 2
	load <- fit$loadings[,1:2]
	plot(load,type="n") # set up plot
	text(load,labels=names(data.scaled[,vars.index]),cex=.7) # add variable names

	# PCA Variable Factor Map
	library(FactoMineR)
	result <- PCA(data.scaled[,vars.index]) # graphs generated automatically


	# http://little-book-of-r-for-multivariate-analysis.readthedocs.io/en/latest/src/multivariateanalysis.html
}



randomFunctions <- function() {

	# Correlations for interest columns
	cor(data[,vars.interests.totals])

	# Pairwise scatterplot for interest columns
	pairs(~total.437+total.341+total.352+total.353+total.430,data=data,
      main="Simple Scatterplot Matrix")

	# Scatterplot of 2 variables
	p <- plot(data$total.437, data$total.341, main="Scatterplot Example",
  		xlab="Gen Interests", ylab="Difference Seeking", pch=19)

	# Add fit lines
	abline(lm(data$total.437~data$total.341), col="red") # regression line (y~x)
	lines(lowess(data$total.341,data$total), col="blue") # lowess line (x,y)
	p

	# ggplot variant
	# http://www.cookbook-r.com/Graphs/Scatterplots_(ggplot2)/
	ggplot(data, aes(x=total.437, y=total.341)) +
	    geom_point(shape=1) +    # Use hollow circles
	    geom_smooth(method=lm)   # Add linear regression line
	                             #  (by default includes 95% confidence region)

	# http://www.cookbook-r.com/Graphs/Scatterplots_(ggplot2)/#set-colorshape-by-another-variable
	ggplot(data, aes(x=total.437, y=total.341, color=gender)) +
	    geom_point(shape=1) +    # Use hollow circles
	    geom_smooth(method=lm)   # Add linear regression line

}
