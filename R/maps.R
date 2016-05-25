# Testing with SA4 data
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
require("rgeos")
require("plotly")
require("dplyr")


obtainSA4 <- function(postcode) {
  value <- as.character(head(sa4s[sa4s$POSTCODE == postcode, "SA4_NAME_2011"], 1))
  if (is.null(value) | length(value) == 0) {
    value <- ""
  }
  else {
    value <- as.character(value)
  }
  return (value)
}


# Initial work
initMaps <- function() {
  aus <- readOGR(dsn="data", layer="SA4_2011_AUST")
  aus.sim <- gSimplify(aus, tol=0.01, topologyPreserve=FALSE)
  aus.sim <- as(aus.sim, "SpatialPolygonsDataFrame")
  aus.sim@data$SA4_NAME11 <- aus@data$SA4_NAME11
  aus.sim@data$id = rownames(aus.sim@data)
  aus.points = fortify(aus.sim, region="id") #replaced aus.buffered with aus
  aus.df = join(aus.points, aus.sim@data, by="id")

  sa4s = read.csv("data/1270055006_CG_POSTCODE_2011_SA4_2011.csv")
  sa4s = sa4s[5:3115, 1:6]
  names = c("POSTCODE",
            "POSTCODE",
            "SA4_CODE_2011",
            "SA4_NAME_2011",
            "RATIO",
            "PERCENTAGE")
  names(sa4s) = names
  sa4s = sa4s[3:3107,]
  sa4s = sa4s[,2:5]
  sa4s$POSTCODE = as.character(sa4s$POSTCODE)

}



# Generates a set of maps by SA4 for a given variable
generateSA4Map <- function(x, vars, func, palette = yawcrcPalette) {
	if (x > 0) {
		ind.names <- obtainIndicatorNames(vars)
		var.name <- vars[x]
		ind.name <- ind.names[x]

		metadata <- expandedIndicators[which(ind.name == expandedIndicators$DCI.ID),]
	}
	else {
		var.name <- vars[1]
		ind.name <- gsub("Q", "", var.name)

		metadata <- indicators[which(ind.name == indicators$DCI.ID),]
		# For consistency
		metadata$Name <- as.character(metadata$Indicator...Variable)
	}

  # Summarise data by SA4 area for a question
  merged2 <- aggregate(augmented.data[,var.name], by = list(augmented.data$SA4_NAME_2011), median)
  merged2$median <- merged2$x
  merged2$x <- NULL
  merged3 <- aggregate(augmented.data[,var.name], by = list(augmented.data$SA4_NAME_2011), mean)
  merged3$mean <- merged3$x
  merged3$x <- NULL
  merged4 <- merge(merged2, merged3, by="Group.1")
  merged4$SA4_NAME_2011 <- merged4$Group.1
  merged4$Group.1 <- NULL
  merged5 <- left_join(aus.sim@data, merged4, by = c("SA4_NAME11" = "SA4_NAME_2011"))
  merged5$id <- as.character(merged5$id)
  #merged = augmented.data %>%
  #  group_by( SA4_NAME_2011 ) %>%
  #  summarise( median = median(Q74_1),
  #             average = mean(Q74_1) ) %>%
  #  left_join( aus.sim@data, ., by = c("SA4_NAME11" = "SA4_NAME_2011") )



  aus.df = join(aus.points, merged5, by="id")

  # Plotting with merged data
  g <- ggplot(aus.df) +
    aes(x = long, y = lat, group = group, fill = median) +
    #Don't want a legend with 150 variables so suppress the legend
    geom_polygon() +
    #for some reason it maps too much ocean so limit coords (EDIT: due to Christmas Island)
    coord_equal(xlim = c(110,155)) +
    scale_fill_gradient( ) +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )

  full.file <- paste("./figs/maps/", var.name, "_clusters.png", sep="")

	# Save the plot
	if (PRINTING) {
		ggsave(file = full.file,
		  width = png.width,
		  height = png.height
		)
	}

	return (g)

}

generateSA4MapForVariable <- function(vars, func) {
	sapply(seq(1:length(vars)), generateSA4Map, vars, func )
}