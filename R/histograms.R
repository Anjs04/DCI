
# Libraries
library(ggplot2)
library(scales)
library(reshape2)
library(ggthemes)
library(dplyr)


# Global variables for graphs
title.size <- 0.8
character.size <- 0.6
graph.title.size <- 18
axis.title.size <- 12
axis.text.size <- 10
x.axis.vjust <- -0.25
y.axis.vjust <- 0.5
x.axis.text.vjust <- 1.0
png.width = 8
png.height = 6


## DECLARE COMMON COLOURS AND PALETTES

# Set colours
# Quasi-BTE
# background <- '#F0D2AF'
# foreground <- '#D08728'
# text.color <- '#888888'

# Experiment 1
# background.color <- '#F6FFC7'
# foreground.color <- '#668E39'
# title.color <- '#667566'
# text.color <- '#202020'

# Experiment 2
# background.color <- '#FFFBE3'
# foreground.color <- '#FF7260'
# title.color <- '#129793'
# text.color <- '#202020'

background.color <- '#FFFFFF'
foreground.color <- '#DFDFDF'
title.color <- '#000000'
text.color <- '#000000'

# Colour-friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# blackPalette <- c("#000000")
yawcrcPalette1 <- c("#139DEA", "#56CDFF", "#949494", "#F07899", "#EA3568", "#FFFFFF", "#000000")
yawcrcPalette2 <- c("#139DEA", "#56CDFF", "#949494", "#F07899", "#EA3568", "#E8A81C", "#000000")
yawcrcPalette3 <- c("#032767", "#139DEC", "#6BCEFF", "#FFFBE2", "#FFF88A", "#E8A81C", "#000000")
yawcrcPalette4 <- c("#032767", "#139DEC", "#DFDFDF", "#FFFBE2", "#FFF88A", "#000000", "#A8A8A8")
yawcrcPalette5 <- c("#032767", "#139DEC", "#6BCEFF", "#FFFBE2", "#FFF88A", "#F8B84C")
yawcrcPalette6 <- c("#032767", "#139DEC", "#6BCEFF", "#FFF88A", "#F8B84C", "#888888")
yawcrcPalette <- yawcrcPalette3
yawcrcPaletteFivePoints <- yawcrcPalette4


# Generates year breaks to use on X axis
yearBreaks <- function(years) {

  # Every year
  years <- years

  return (years)

}


# Generates financial year labels for a series of years, e.g. "66/67"
yearLabels <- function(years) {

  years <- yearBreaks(years)

  # Formats the year into financial year form, e.g. "66/67"
  output <- paste(formatC((years) * 5, width = 2, format = "d", flag = "0"),
                  " - ",
                  formatC((years + 1) * 5 - 1, width = 2, format = "d", flag = "0"), sep = "")

  return (output)

}

# Frequency labels
frequencyLabels <- function() {

	labels <- c(
		"Several times each day",
		"Daily or almost daily",
		"At least every week",
		"Less often",
		"Never",
		"Don't know"
	)

	return (labels)

}
# Frequency labels (with months)
frequencyMonthLabels <- function() {

	labels <- c(
		"Several times per day",
		"Daily",
		"Weekly",
		"Monthly",
		"Less than Once a Month",
		"Never",
		"Don't know"
	)

	return (labels)

}

# Ease labels
easeLabels <- function() {

	labels <- c(
		"Very easy",
		"Easy",
		"Neither easy nor difficult",
		"Difficult",
		"Very difficult",
		"Don't know",
		"Not Applicable"
	)

	return (labels)

}


# Agreement labels
agreementLabels <- function() {

	labels <- c(
		"Strongly agree",
		"Agree",
		"Neither agree nor disagree",
		"Disagree",
		"Strongly disagree",
		"Not applicable (NA)",
		"Don't know"
	)

	return (labels)

}

# Importance labels
importanceLabels <- function() {

	labels <- c(
		"Extremely important",
		"Fairly important",
		"A little important",
		"Not important at all"
	)

	return (labels)

}

# Yes/No labels
yesNoLabels <- function() {

	labels <- c(
		"Yes",
		"No"
	)

	return (labels)

}




# Load indicator list, for mapping DCI.IDs to variable names
loadIndicators <- function() {

	results <- read.csv("data/Indicators.csv", header = TRUE, strip.white = TRUE, na.strings = c("", " "))

	return (results)

}


# Load survey results supplied by Pure Profile
loadSurveyResults <- function() {

	results <- read.csv("data/DCI_DATA.csv", header = TRUE, strip.white = TRUE, na.strings = c("", " "))

	return (results)

}


## Provides a single function for generating bar charts
standardBarChart <- function(data, file.name, title, x.label, y.label, func, use.years = TRUE) {

  # Ensure the order remains the same
  if (use.years==FALSE) {
    data$Group.1 <- factor(data$Group.1, as.character(data$Group.1))
  }

  # Change the X axis for time series
  if (use.years==TRUE) {
    x.scale <- scale_x_continuous(name = x.label,
                                  breaks = yearBreaks(data$Group.1),
                                  labels = yearLabels(data$Group.1))
  } else {
    x.scale <- xlab(x.label)
  }

  y.scale <- scale_y_continuous(name=y.label,
  								breaks=c(1, 2, 3, 4, 5, 6),
  								labels=func())

  p <- ggplot(data, aes(x=Group.1, y = x)) +
          geom_bar(width=0.75, stat="identity")

	p <- p +
	  ggtitle(title) +
	  x.scale +
	  y.scale

	p <- augmentChart(p, prop.freq.melted$Var1, func())

  p +
      theme_tufte() +    # FOR CONSISTENT LOOK AND FEEL
	  scale_fill_manual(values = yawcrcPalette) +
      ggtitle(title) +
      x.scale +
      y.scale +

      theme(
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
    )


  # Save the plot
  if (PRINTING) {
	  ggsave(file=paste("./figs/", file.name, ".png", sep=""),
	      width = png.width,
	      height = png.height
	    )
	  print()
  }

  return (p)

}

## Augment chart with standard look and feel
augmentChart <- function(p, breaks, labels, palette = yawcrcPalette) {
	p <- p +
		scale_fill_manual(name="",
				 values=palette,
                 breaks=unique(breaks),
                 labels=labels) +
	  theme(
	    # GRID
	    panel.grid.minor.y = element_blank(),
	    panel.grid.major.y = element_blank(),
	    # panel.grid.major.y = element_line(colour = foreground.color),
	    panel.grid.minor.x = element_blank(),
	    panel.grid.major.x = element_line(colour = foreground.color, linetype = "dashed"),

	    # BACKGROUND
	    # panel.background = element_rect(fill = background.color, colour = foreground.color),

	    # TITLE
	    # plot.title = element_text(colour = title.color, lineheight=1.0, face="bold", size=graph.title.size),
	    axis.title = element_text(color=title.color, lineheight=1.0, size = axis.title.size),
	    # axis.title = element_text(lineheight=1.0, size = axis.title.size),
	    axis.title.x = element_text(size = axis.title.size, vjust = x.axis.vjust),
	    axis.title.y = element_text(size = axis.title.size, vjust = y.axis.vjust),

	    # LINE
	    axis.line = element_line(colour = "black"),

	    # TEXT
	    axis.text.x = element_text(color=text.color, angle=0, vjust=1.0, hjust=0.5, size = axis.text.size * 0.8),
	    axis.text.y = element_text(color=text.color, angle=0, vjust=1.0, hjust=1.0, size = axis.text.size * 0.8),
	    # axis.text.x = element_text(angle=45, vjust=1.0, hjust=1.0, size = axis.text.size),
	    # axis.text.y = element_text(angle=45, size = axis.text.size)

		panel.background = element_rect(fill = "white")
	)



	return (p)
}

## Provides a single function for generating frequency distribution bar charts
freqDistChart <- function(freq.table, file.name, title, x.label, y.label, func, palette = yawcrcPalette, use.years = TRUE) {

	# Assume the columns contain decade breaks
	prop.freq.table <- prop.table(freq.table, 2)

	# Melt data for graphing
	prop.freq.melted <- melt(prop.freq.table)

	# Change the X axis for time series
	if (use.years==TRUE) {
		years <- yearBreaks(unique(prop.freq.melted$Var2))
		x.scale <- scale_x_continuous(name = x.label,
	                              breaks = years,
	                              labels = yearLabels(years))
	} else {
		x.scale <- xlab(x.label)
	}

	y.scale <- scale_y_continuous(name = y.label,
									breaks = seq(0.0, 1.0, by = 0.2),
									labels = paste(seq(0, 100, by = 20), "%", sep = ""))

	# Graph so that x = 2nd variable (age.breaks), y = data, fill = responses
	p <- ggplot(data = prop.freq.melted,
		aes(x = Var2, y = value, fill = factor(Var1))) +
	    geom_bar(width=0.75, stat="identity") +
	    coord_flip()

	p <- p +
	  # ggtitle(title) +
	  x.scale +
	  y.scale

	p <- augmentChart(p, prop.freq.melted$Var1, func(), palette)

	full.file <- paste("./figs/", file.name, ".png", sep="")

	# Save the plot
	if (PRINTING) {
		ggsave(file = full.file,
		  width = png.width,
		  height = png.height
		)
	}

	# Open the file if on Mac - TODO: check other OS'es
	if (Sys.info()['sysname'] == "Darwin") {
		#system2("open", full.file)
	}

	return (p)

}



ageChart <- function(augmented.data) {

	age.breaks <- sort(unique(augmented.data$age.breaks))
	age.breaks.labels <- sort(unique(augmented.data$age.breaks))

	x.scale <- scale_x_discrete(name = "Ages",
									breaks = age.breaks,
									labels = age.breaks.labels)

	p <- ggplot(augmented.data, aes(x=age.breaks)) +
				geom_bar(width=0.5) +
				x.scale +
	    		coord_flip()

	p <- augmentChart(p, augmented.data$age.breaks, unique(augmented.data$age.breaks))

	return (p)
}

top4AgesAsPercentage <- function(){
	c <- count(augmented.data$age.breaks)
	c$rel.freq <- c$freq * 100 / length(augmented.data$age.breaks)
	p <- sum(head(sort(c$rel.freq, decreasing = TRUE), 4))
	p <- paste(round(p, 1), "%", sep = "")
	return (p)
}



genderChart <- function(augmented.data) {

	p <- ggplot(augmented.data, aes(x=gender)) +
				geom_bar(width=0.5) +
				# x.scale +
	    		coord_flip()

	p <- augmentChart(p, augmented.data$gender, unique(augmented.data$gender))

	return (p)
}


genderAndAgeChart <- function(augmented.data) {

	age.breaks <- sort(unique(augmented.data$age.breaks))
	# a <- sort(unique(augmented.data$age.breaks))
	# age.breaks.labels <- paste(a, " - ", a + 4, sep = "")
	x.scale <- scale_x_discrete(name = "age.breaks",
									breaks = age.breaks,
									labels = age.breaks)

	ad <- table(augmented.data$gender, augmented.data$age.breaks)
	ad <- melt(ad)

	p <- ggplot(ad, aes(x=Var2, y=value, fill=Var1)) +
				geom_bar(width=0.5, stat="identity", position="dodge") +
				x.scale +
	    		coord_flip()

	p <- augmentChart(p, augmented.data$gender, unique(augmented.data$gender))

	return (p)
}



stateChart <- function(augmented.data) {

	p <- ggplot(augmented.data,
				aes(x = factor(state,
		                  levels=names(sort(table(state),
                          decreasing=TRUE))))) +
				xlab("State") +
				geom_bar(width=0.5) +
	    		coord_flip()

	p <- augmentChart(p, augmented.data$state, unique(augmented.data$state))

	return (p)
}



locationChart <- function(augmented.data) {

	p <- ggplot(augmented.data,
				aes(x = factor(location,
		                  levels=names(sort(table(location),
                          decreasing=TRUE))))) +
				geom_bar(width=0.5) +
				xlab("Location") +
				xlab("Percentage") +
	    		coord_flip()

	p <- augmentChart(p, augmented.data$location, unique(augmented.data$location))

	return (p)
}

chartVariableByAge <- function(data, filename, metadata, labelsY) {
	p <- standardBarChart(data,
					filename,
					paste(metadata$Name, " by Age"),
					"Age by Decade",
					metadata$Name,
					labelsY,
					FALSE
					)
	comment <- paste("Printed graph of ", metadata$Name, " to ./figs/", filename, ".png", sep="")
	if (PRINTING) {
		print(comment)
	}
	comment <- paste("Type 'open ./figs/", filename, ".png' from the terminal to view the file.", sep="")
	if (PRINTING) {
		print(comment)
	}
	return (p)
}

chartFrequencies <- function(data, filename, metadata, labelsY, desc1, desc2, palette = yawcrcPalette, use.years = TRUE) {
	p <- freqDistChart(data,
					filename,
					desc1,
					desc2,
					metadata$Label,
					labelsY,
					palette,
					use.years
					)
	comment <- paste("Printed graph of ", metadata$Name, " to ./figs/", filename, ".png", sep="")
	if (PRINTING == TRUE) {
		print(comment)
	}
	# comment <- paste("Type 'open ./figs/", filename, ".png' from the terminal to view the file.", sep="")
	# print(comment)
	return (p)
}




# Obtain indicator names
# vars.names <- gsub("Q([[:digit:]]*)_([[:digit:]]*)", "\\1.\\2", vars)

generateChartsForVariable <- function(var) {
	var.name <- gsub("Q", "", var)
	var.parts <- unlist(strsplit(c(var.name), "_"))
	v1 <- var.parts[1]
	v2 <- var.parts[2]
	var.name <- gsub("Q([[:digit:]]*)_([[:digit:]]*)", "\\1.\\2", var)
	if (PRINTING) {
		print(var.name)
	}
}



obtainIndicatorName <- function(var) {
	var.name <- gsub("Q", "", var)
	var.parts <- unlist(strsplit(c(var.name), "_"))
	v1 <- as.integer(var.parts[1])
	v2 <- as.integer(var.parts[2])
	if (!is.na(v1)) {
		ind.name <- paste(v1, ".", v2, sep = "")
	}
	else {
		ind.name <- v1
	}
	return (ind.name)
}

obtainIndicatorNames <- function(vars) {
	v1.last <- 0
	v.counter <- 0
	ind.names <- c()
	for (i in 1:length(vars)) {
		var <- vars[i]
		var.name <- gsub("Q", "", var)
		var.parts <- unlist(strsplit(c(var.name), "_"))
		v1 <- as.integer(var.parts[1])
		v2 <- as.integer(var.parts[2])
		if (!is.na(v1)) {
			if (v1 != v1.last) {
				v.counter <- 1
			}
			v2 <- v.counter
			ind.name <- paste(v1, ".", v2, sep = "")
			v1.last <- v1
		}
		else {
			v.counter <- 1
			ind.name <- var.name
		}
		ind.names <- c(ind.names, ind.name)
		v.counter <- v.counter + 1
	}

	return(ind.names)
}



generateSingleAgeFrequency <- function(x, vars, func, palette = yawcrcPalette) {
	if (x > 0) {
		ind.names <- obtainIndicatorNames(vars)
		var.name <- vars[x]
		ind.name <- ind.names[x]

		freqs <- table(augmented.data[,var.name], augmented.data$age.breaks)
		metadata <- expandedIndicators[which(ind.name == expandedIndicators$DCI.ID),]
	}
	else {
		var.name <- vars[1]
		ind.name <- gsub("Q", "", var.name)

		freqs <- table(augmented.data[,var.name], augmented.data$age.breaks)
		metadata <- indicators[which(ind.name == indicators$DCI.ID),]
		# For consistency
		metadata$Name <- as.character(metadata$Indicator...Variable)
	}

	p <- chartFrequencies(freqs,
							paste("age/", var.name, "_freqs", sep = ""),
							metadata,
							func,
							"by Age",
							"Age",
							palette,
							FALSE)
	return (p)
}

generateAgeFrequencies <- function(vars, func) {
	sapply(seq(1:length(vars)), generateSingleAgeFrequency, vars, func )
}


generateSingleGenderFrequency <- function(x, vars, func, palette = yawcrcPalette) {
	if (x > 0) {
		ind.names <- obtainIndicatorNames(vars)
		var.name <- vars[x]
		ind.name <- ind.names[x]

		freqs <- table(augmented.data[,var.name], augmented.data$gender)
		metadata <- expandedIndicators[which(ind.name == expandedIndicators$DCI.ID),]
	}
	else {
		var.name <- vars[1]
		ind.name <- gsub("Q", "", var.name)

		freqs <- table(augmented.data[,var.name], augmented.data$gender)
		metadata <- indicators[which(ind.name == indicators$DCI.ID),]
		# For consistency
		metadata$Name <- as.character(metadata$Indicator...Variable)
	}
	p <- chartFrequencies(freqs,
							paste("gender/", var.name, "_freqs", sep = ""),
							metadata,
							func,
							"by Gender",
							"Gender",
							palette,
							FALSE)
	return (p)
}

generateGenderFrequencies <- function(vars, func) {
	sapply(seq(1:length(vars)), generateSingleGenderFrequency, vars, func)
}

generateSingleStateFrequency <- function(x, vars, func, palette = yawcrcPalette) {
	if (x > 0) {
		ind.names <- obtainIndicatorNames(vars)
		var.name <- vars[x]
		ind.name <- ind.names[x]

		freqs <- table(augmented.data[,var.name], augmented.data$state)
		metadata <- expandedIndicators[which(ind.name == expandedIndicators$DCI.ID),]
	}
	else {
		var.name <- vars[1]
		ind.name <- gsub("Q", "", var.name)

		freqs <- table(augmented.data[,var.name], augmented.data$state)
		metadata <- indicators[which(ind.name == indicators$DCI.ID),]
		# For consistency
		metadata$Name <- as.character(metadata$Indicator...Variable)
	}
	p <- chartFrequencies(freqs,
							paste("state/", var.name, "_freqs", sep = ""),
							metadata,
							func,
							"by State",
							"State",
							palette,
							FALSE)
	return (p)
}

generateStateFrequencies <- function(vars, func) {
	sapply(seq(1:length(vars)), generateSingleStateFrequency, vars, func)
}

generateSingleLocationFrequency <- function(x, vars, func, palette = yawcrcPalette) {
	if (x > 0) {
		ind.names <- obtainIndicatorNames(vars)
		var.name <- vars[x]
		ind.name <- ind.names[x]

		freqs <- table(augmented.data[,var.name], augmented.data$location)
		metadata <- expandedIndicators[which(ind.name == expandedIndicators$DCI.ID),]
	}
	else {
		var.name <- vars[1]
		ind.name <- gsub("Q", "", var.name)

		freqs <- table(augmented.data[,var.name], augmented.data$location)
		metadata <- indicators[which(ind.name == indicators$DCI.ID),]
		# For consistency
		metadata$Name <- as.character(metadata$Indicator...Variable)
	}
	p <- chartFrequencies(freqs,
							paste("location/", var.name, "_freqs", sep = ""),
							metadata,
							func,
							"by Location",
							"Location",
							palette,
							FALSE)
	return (p)
}

generateLocationFrequencies <- function(vars, func) {
	sapply(seq(1:length(vars)), generateSingleLocationFrequency, vars, func)
}

generateSingleEducationFrequency <- function(x, vars, func, palette = yawcrcPalette) {
	if (x > 0) {
		ind.names <- obtainIndicatorNames(vars)
		var.name <- vars[x]
		ind.name <- ind.names[x]

		freqs <- table(augmented.data[,var.name], augmented.data$level.education)
		metadata <- expandedIndicators[which(ind.name == expandedIndicators$DCI.ID),]
	}
	else {
		var.name <- vars[1]
		ind.name <- gsub("Q", "", var.name)

		freqs <- table(augmented.data[,var.name], augmented.data$level.education)
		metadata <- indicators[which(ind.name == indicators$DCI.ID),]
		# For consistency
		metadata$Name <- as.character(metadata$Indicator...Variable)
	}
	p <- chartFrequencies(freqs,
							paste("education/", var.name, "_freqs", sep = ""),
							metadata,
							func,
							"by Education",
							"Education",
							palette,
							FALSE)
	return (p)
}

generateEducationFrequencies <- function(vars, func) {
	sapply(seq(1:length(vars)), generateSingleEducationFrequency, vars, func)
}

generateSingleOccupationFrequency <- function(x, vars, func, palette = yawcrcPalette) {
	if (x > 0) {
		ind.names <- obtainIndicatorNames(vars)
		var.name <- vars[x]
		ind.name <- ind.names[x]

		freqs <- table(augmented.data[,var.name], augmented.data$main.activities)
		metadata <- expandedIndicators[which(ind.name == expandedIndicators$DCI.ID),]
	}
	else {
		var.name <- vars[1]
		ind.name <- gsub("Q", "", var.name)

		freqs <- table(augmented.data[,var.name], augmented.data$main.activities)
		metadata <- indicators[which(ind.name == indicators$DCI.ID),]
		# For consistency
		metadata$Name <- as.character(metadata$Indicator...Variable)
	}
	p <- chartFrequencies(freqs,
							paste("occupation/", var.name, "_freqs", sep = ""),
							metadata,
							func,
							"by Occupation",
							"Occupation",
							palette,
							FALSE)
	return (p)
}

generateOccupationFrequencies <- function(vars, func) {
	sapply(seq(1:length(vars)), generateSingleOccupationFrequency, vars, func)
}



sumVariable <- function() {
	vars.of.interest = vars.competencies.online.activities.74
	means <- sapply(vars.of.interest, function(x) {
		mean(augmented.data[,x])
	} )
	print(mean(means))
}


histogram <- function(cols, name, file.name) {
	aggMeans <- data.frame(round(rowSums(augmented.data[,cols])))
	colnames(aggMeans)[1] <- name
	x.scale <- scale_x_continuous(name = name)
	p <- ggplot(data = aggMeans, aes(x = aggMeans[1])) +
		geom_histogram(binwidth = 0.5) +
		geom_density(aes(y=1.0*..count..), colour="red", adjust=4) +
		x.scale

	full.file <- paste("./figs/", file.name, ".png", sep="")

	# Save the plot
	if (PRINTING) {
		ggsave(file = full.file,
		  width = png.width,
		  height = png.height
		)
	}

	return (p)
}


# vars.connectedness.maintenance.287, "Importance", importanceLabels, "maintaining-connections-287"
# Generic function that returns relative frequencies of sub questions (items) as a graph
graphSubQuestionFrequencies  <- function(vars, legend.name, legendBreakFunc, file.name, y.right.offset = -8.0, palette = yawcrcPalette) {
	# Add dummy column, to allow melt to work with single columns
	data$dummy <- NA
	vars <- c(vars, "dummy")
	# Obtain a melted, long version of the column data
	m <- melt(data[,vars], id.vars = c(), na.rm = TRUE)
	# Generate counts of the item data
	cm <- plyr::count(m, c("variable", "value"))
	# Recode, to solve problem with ggplotly and scale_fill_manual
	cm$value.coded <- mapvalues(cm$value, from = 1:length(legendBreakFunc()), to = legendBreakFunc())
	cm$value.coded.f <- factor(cm$value.coded, levels = legendBreakFunc())
	# Relative frequencies (Q10_159 is age)
	cm$rel.freq <- cm$freq / length(data$Q10_159)
	# Rename for simplicity - TODO: push up to the global variable
	ex <- expandedIndicators
	# Complicated code that loads truncated item names into the melted data
	var.names <- c(unique(cm$variable))
	# If the length of variables is more than 1, we need proper labels
	if (length(unique(cm$variable)) > 1) {
		var.names = as.character(unlist(sapply(obtainIndicatorNames(unique(cm$variable)), function(lbl) {
			v <- ex[which(ex$DCI.ID == lbl),2]
			# Assume a name like "74 - 1 - XXX"
			v <- unlist(strsplit(as.character(v), "-"))[3]
			# Truncate if too long
			v <- substring(v, 1, 120)
			# Add back the ID, for reference
			# v <- paste(lbl, v, sep = " ")
			return (v)
		})))
	}
	# Construct scales
	x.scale <- scale_x_discrete(name = " ",
									breaks = unique(cm$variable),
									labels = var.names)
	y.scale <- scale_y_continuous(name = "Percentage",
									breaks = seq(0.0, 1.0, by = 0.2),
									labels = paste(seq(0, 100, by = 20), "%", sep = ""))

	fill.scale <- scale_fill_manual(
						name=legend.name,
				 		values=palette)
	# Works around problem with plot.ly - see http://stackoverflow.com/questions/35369309/plotly-legend-problems-with-ggplot2
				# , breaks=seq(1:length(legendBreakFunc()))
				# , labels=legendBreakFunc()
	# Generate plot
	w <- 0.5
	height <- 0.4 + 10 * length(cm$variable)
	p <- ggplot(
		data = cm,
		aes(x = variable, y = rel.freq, fill = value.coded.f, label = variable)) +
	    geom_bar(width = w, stat = "identity") +
	    coord_flip() +
	    x.scale +
	    y.scale +
	    fill.scale
	# p <- p + theme(
	# 	axis.text.y = element_text(color=text.color, angle=45, vjust=1.0, hjust=1.0, size = axis.text.size * 0.8)
	# )
	p <- p +
	  theme(
	    # GRID
	    # panel.grid.minor.y = element_blank(),
	    # panel.grid.major.y = element_blank(),
	    # panel.grid.major.y = element_line(colour = foreground.color),
	    # panel.grid.minor.y = element_line(colour = foreground.color),
	    panel.grid.major.x = element_line(colour = foreground.color, linetype = "dashed"),
	    # panel.grid.major.x = element_line(colour = foreground.color),
	    # panel.grid.minor.x = element_blank(),
	    # panel.grid.major.x = element_blank(),

	    # BACKGROUND
	    # panel.background = element_rect(fill = background.color, colour = foreground.color),

	    # TITLE
	    # plot.title = element_text(colour = title.color, lineheight=1.0, face="bold", size=graph.title.size),
	    axis.title = element_text(color=title.color, lineheight=1.0, size = axis.title.size),
	    # axis.title = element_text(lineheight=1.0, size = axis.title.size),
	    axis.title.x = element_text(size = axis.title.size, vjust = x.axis.vjust),
	    axis.title.y = element_text(size = axis.title.size, vjust = y.axis.vjust),

	    # LINE
	    axis.line = element_line(colour = "black"),

	    # TEXT
	    axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0, unit ="cm"), color=text.color, angle=0, vjust=0.0, hjust=0.5, size = axis.text.size * 1.0),
	    axis.text.y = element_text(margin = margin(t = 0, r = y.right.offset, b = 0, l = 0.5, unit ="in"), color=text.color, angle=0, vjust=-1.5, hjust=0.0, size = axis.text.size * 1.0),
	    axis.ticks.x = element_line(colour = "white", size = 0.1),
	    axis.ticks.y = element_line(colour = "white", size = 0.1),
	    # axis.text.x = element_text(angle=45, vjust=1.0, hjust=1.0, size = axis.text.size),
	    # axis.text.y = element_text(angle=45, size = axis.text.size)

	    legend.position="bottom",
	    legend.direction="vertical",
	    plot.margin = unit(c(0,0,0,0), "cm"),
	    panel.background = element_rect(fill = "white")
	)

	full.file <- paste("./figs/gen/", file.name, ".png", sep="")

	# Save the plot
	if (PRINTING) {
		ggsave(file = full.file,
		  width = png.width,
		  height = png.height
		)
		comment <- paste("Printed graph to ", full.file, sep="")
		print(comment)
	}

	return (p)
}

chartWrap <- function(p) {
	if (PLOTLY) {
		library(plotly)
		return (ggplotly(p))
	}
	else {

		return (p)
	}
}

freqAlls <- function(cols, labels) {
	means <- round(rowMeans(augmented.data[,cols]))
	dt <- data.frame(table(means))
	dt$labels <- labels()
	dt$rel.freq <- format(100 * dt$Freq / sum(dt$Freq), digits = 2)
	return (dt)
}

freqs <- function(index, cols, labels) {
	col <- cols[index]
	dt <- data.frame(table(augmented.data[,c(col)]))
	dt$labels <- labels()
	dt$rel.freq <- format(100 * dt$Freq / sum(dt$Freq), digits = 2)
	return (dt)
}

freqAges <- function(index, cols, labels) {
	col <- cols[index]
	dt <- data.frame(table(augmented.data[,c("age.breaks", col)]))
	dt <- dt[order(dt$age.breaks),]
	dt$labels <- labels()
	gt <- aggregate(Freq ~ age.breaks, dt, sum)
	dt <- merge(dt[,], gt, by="age.breaks")
	dt$rel.freq <- format(100 * dt$Freq.x / dt$Freq.y, digits = 2)
	dt <- dt[,c("age.breaks", "labels", "rel.freq")]
	return (dt)
}

getLabel <- function(index, cols) {
	ind.names <- obtainIndicatorNames(cols)
	ind.name <- ind.names[index]
	metadata <- expandedIndicators[which(ind.name == expandedIndicators$DCI.ID),]
	return (metadata$Label)
}

questionCategory <- function(var.name) {

	# Remove 'Q' from name
	ind.name <- gsub("Q", "", var.name)

	# Remove 'total.' from name
	ind.name <- gsub("total.", "", ind.name)

	# Replace underscores for lookups
	ind.name <- gsub("_", ".", ind.name)

	metadata <- indicators[which(ind.name == indicators$DCI.ID),c("Indicator...Variable")]
	# For consistency
	category <- as.character(unlist(metadata))
	if (length(category) == 0) {
		metadata <- expandedIndicators[which(ind.name == expandedIndicators$DCI.ID),c("Label")]
		category <- as.character(unlist(metadata))
	}

	return (category)
}

questionText <- function(var.name) {
	ind.name <- gsub("Q", "", var.name)

	metadata <- indicators[which(ind.name == indicators$DCI.ID),]
	# For consistency
	metadata$Name <- as.character(metadata$Question..Adult)

	return (metadata$Name)
}
