
generateIndexForCompetencies <- function() {
	# Competencies are correctly coded: low values mean higher competencies
	vars.competencies.for.index <- vars.competencies
	histogram(vars.competencies.for.index, 'Competencies', 'gen/index-competencies')
}


generateIndexForInterests <- function() {
	# Interests are correctly coded: low values mean higher competencies
	vars.interest.for.index <- vars.interest
	histogram(vars.interest.for.index, 'Interests', 'gen/index-interests')
}

generateIndexForResilience <- function() {
	# Resilience is more complex; high scores on some responses can be
	# interpreted as more resilient, others as less resilient

	# Reverse harm  results - these are interpreted as meaning *less* resilient
	vars.resilience.harm.events.434.for.index <- c()
	for (i in 1:length(vars.resilience.harm.events.434)) {
		var.name <- vars.resilience.harm.events.434[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 6 - c
		d <- replace(d, d == -1, 0)
		vars.resilience.harm.events.434.for.index <- c(vars.resilience.harm.events.434.for.index, var.name)
		augmented.data[new.var] <<- d
	}

	vars.resilience.for.index <- c(
		vars.resilience.harm.events.434.for.index,
		vars.resilience.harms.agreement.435
		# TODO: Determine what this means in terms of resilience
		# vars.resilience.engage.with.others.428
	)
	histogram(vars.resilience.for.index, 'Resilience', 'gen/index-resilience')
}

generateIndexForConnectedness <- function() {

	# Similar to vars.connectedness
	# TODO: remove or reverse responses to 'tech attitudes'
	vars.connectedness.for.index <- c(
		vars.connectedness.helping.others.277,
		vars.connectedness.sought.help.from.others.280,
		vars.connectedness.maintenance.287,
		vars.connectedness.events.343,
		vars.connectedness.tech.attitudes.429
	)
	histogram(vars.connectedness.for.index, 'Connectedness', 'gen/index-connectedness')
}

generateIndexForAll <- function() {
	vars.all.for.index <- vars.index
	histogram(vars.all.for.index, 'All Measures', 'gen/index-all')
}

generateIndexes <- function() {
	generateIndexForInterests()
	generateIndexForCompetencies()
	generateIndexForResilience()
	generateIndexForConnectedness()
	generateIndexForAll()
}

generateAll <- function() {

	# Specific question frequences by age, gender, state, location
	generateFrequencies()
	# Aggregate frequences by age, gender, state, location
	generateAggregateFrequences()

	# Sub question frequences
	generateSubQuestionCharts()

	# Index functions
	generateIndexes()

}

sumCompetencies <- function() {
	# Competencies are correctly coded: low values mean higher competencies
	vars.competencies.for.index <- c()

	# Totals
	total.74 <- 0
	total.431 <- 0

	# Score frequency
	for (i in 1:length(vars.competencies.online.activities.74)) {
		var.name <- vars.competencies.online.activities.74[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 5 - c
		# Convert "Don't know" to "Less often" - conservative guess
		d <- replace(d, d == -1, 1)
		vars.competencies.for.index <- c(vars.competencies.for.index, new.var)
		augmented.data[new.var] <<- d
		total.74 <- total.74 + d
	}
	# Score ease
	for (i in 1:length(vars.competencies.431)) {
		var.name <- vars.competencies.431[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 5 - c
		# Convert "Don't know" to "Neither easy nor difficult" - conservative guess
		# Convert "Not applicable" to "Neither easy nor difficult" - conservative guess
		d <- replace(d, d == -1, 2)
		d <- replace(d, d == -2, 2)
		vars.competencies.for.index <- c(vars.competencies.for.index, new.var)
		augmented.data[new.var] <<- d
		total.431 <- total.431 + d
	}

	augmented.data$total.74 <<- total.74
	augmented.data$total.431 <<- total.431

	# Max value: (Q74) +  (Q431)
	# Max value: 15 * 4 + 27 * 4
	divisor <- 168
	augmented.data$competencies.index <<- 100 * rowSums(augmented.data[,vars.competencies.for.index]) / divisor
	return(augmented.data$competencies.index)
}

sumInterests <- function() {
	# Score frequency
	vars.interests.for.index <- c()

	# Totals
	total.437 <- 0
	total.341 <- 0
	total.352 <- 0
	total.353 <- 0
	total.430 <- 0

	for (i in 1:length(vars.interests.general.437)) {
		var.name <- vars.interests.general.437[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 6 - c
		# Convert "Don't know" to "Less than Once a Month" - conservative guess
		d <- replace(d, d == -1, 1)
		vars.interests.for.index <- c(vars.interests.for.index, new.var)
		augmented.data[new.var] <<- d
		total.437 <- total.437 + d
	}
	for (i in 1:length(vars.interests.difference.seeking.341)) {
		var.name <- vars.interests.difference.seeking.341[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 5 - c
		# Convert "Don't know" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -1, 2)
		# Convert "Not Applicable" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -2, 2)
		vars.interests.for.index <- c(vars.interests.for.index, new.var)
		augmented.data[new.var] <<- d
		total.341 <- total.341 + d
	}
	for (i in 1:length(vars.interests.fitness.352)) {
		var.name <- vars.interests.fitness.352[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 6 - c
		# Convert "Don't know" to "Less than Once a Month" - conservative guess
		d <- replace(d, d == -1, 1)
		vars.interests.for.index <- c(vars.interests.for.index, new.var)
		augmented.data[new.var] <<- d
		total.352 <- total.352 + d
	}
	for (i in 1:length(vars.interests.health.improvement.353)) {
		var.name <- vars.interests.health.improvement.353[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 5 - c
		# Convert "Don't know" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -1, 2)
		# Convert "Not Applicable" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -2, 2)
		vars.interests.for.index <- c(vars.interests.for.index, new.var)
		augmented.data[new.var] <<- d
		total.353 <- total.353 + d
	}
	for (i in 1:length(vars.interests.keeping.in.touch.430)) {
		var.name <- vars.interests.keeping.in.touch.430[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 4 - c
		vars.interests.for.index <- c(vars.interests.for.index, new.var)
		augmented.data[new.var] <<- d
		total.430 <- total.430 + d
	}

	augmented.data$total.437 <<- total.437
	augmented.data$total.341 <<- total.341
	augmented.data$total.352 <<- total.352
	augmented.data$total.353 <<- total.353
	augmented.data$total.430 <<- total.430

	# Max value: (Q437) + (Q341) + (Q352) + (Q353) + (Q341)
	# Max value: 11 * 5 + 7 * 4  + 10 * 5  + 2 * 4 + 14 * 3
	divisor <- 183
	augmented.data$interests.index <<- 100 * rowSums(augmented.data[,vars.interests.for.index]) / divisor
	return(augmented.data$interests.index)

}

sumResilience <- function() {
	# Score frequency
	vars.resilience.for.index <- c()

	total.434 <- 0
	total.435 <- 0
	total.428 <- 0

	for (i in 1:length(vars.resilience.harm.events.434)) {
		var.name <- vars.resilience.harm.events.434[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 6 - c
		# Convert "Don't know" to "Less than Once a Month" - conservative guess
		d <- replace(d, d == -1, 1)
		vars.resilience.for.index <- c(vars.resilience.for.index, new.var)
		augmented.data[new.var] <<- d
		total.434 <- total.434 + d
	}
	for (i in 1:length(vars.resilience.harms.agreement.435)) {
		var.name <- vars.resilience.harms.agreement.435[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 5 - c
		# Convert "Don't know" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -1, 2)
		# Convert "Not Applicable" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -2, 2)
		vars.resilience.for.index <- c(vars.resilience.for.index, new.var)
		augmented.data[new.var] <<- d
		total.435 <- total.435 + d
	}

	# TODO: Meaning of 106-109 for resilience? Maybe connectedness?

	# QUESTION: When I am going through a difficult time, I go online less often
	var.name <- "Q428_110"
	new.var <- paste(var.name, ".for.index", sep = "")
	c <- augmented.data[,var.name]
	# Normalise - Strongly agree means *less* resilient
	d <- c - 1
	# Convert "Don't know" to "Neither agree nor disagree" - conservative guess
	d <- replace(d, d == 5, 2)
	# Convert "Not Applicable" to "Neither agree nor disagree" - conservative guess
	d <- replace(d, d == 6, 2)
	vars.resilience.for.index <- c(vars.resilience.for.index, new.var)
	augmented.data[new.var] <<- d
	total.428 <- total.428 + d

	# QUESTION: When I am going through a difficult time, going online makes me feel better
	var.name <- "Q428_111"
	new.var <- paste(var.name, ".for.index", sep = "")
	c <- augmented.data[,var.name]
	# Normalise - Strongly agree means *more* resilient
	d <- 5 - c
	# Convert "Don't know" to "Neither agree nor disagree" - conservative guess
	d <- replace(d, d == -2, 2)
	# Convert "Not Applicable" to "Neither agree nor disagree" - conservative guess
	d <- replace(d, d == -1, 2)
	vars.resilience.for.index <- c(vars.resilience.for.index, new.var)
	augmented.data[new.var] <<- d
	total.428 <- total.428 + d

	augmented.data$total.434 <<- total.434
	augmented.data$total.435 <<- total.435
	augmented.data$total.428 <<- total.428

	# Max value: (Q434) + (Q435) + (Q428)
	# Max value: 11 * 5 + 7 * 4  + 2 * 4
	divisor <- 91
	augmented.data$resilience.index <<- 100 * rowSums(augmented.data[,vars.resilience.for.index]) / divisor
	return(augmented.data$resilience.index)

}

sumConnectedness <- function() {
	# Score frequency
	vars.connectedness.for.index <- c()

	total.277 <- 0
	total.280 <- 0
	total.287 <- 0
	total.343 <- 0
	total.429 <- 0

	for (i in 1:length(vars.connectedness.helping.others.277)) {
		var.name <- vars.connectedness.helping.others.277[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- c - 1
		vars.connectedness.for.index <- c(vars.connectedness.for.index, new.var)
		augmented.data[new.var] <<- d
		total.277 <- total.277 + d
	}
	for (i in 1:length(vars.connectedness.sought.help.from.others.280)) {
		var.name <- vars.connectedness.sought.help.from.others.280[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- c - 1
		vars.connectedness.for.index <- c(vars.connectedness.for.index, new.var)
		augmented.data[new.var] <<- d
		total.280 <- total.280 + d
	}
	for (i in 1:length(vars.connectedness.maintenance.287)) {
		var.name <- vars.connectedness.maintenance.287[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 3 - c
		# Convert "Don't know" to "Not important at all" - conservative guess
		d <- replace(d, d == -1, 0)
		vars.connectedness.for.index <- c(vars.connectedness.for.index, new.var)
		augmented.data[new.var] <<- d
		total.287 <- total.287 + d
	}
	for (i in 1:length(vars.connectedness.events.343)) {
		var.name <- vars.connectedness.events.343[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 6 - c
		# Convert "Don't know" to "Less than Once a Month" - conservative guess
		d <- replace(d, d == -1, 1)
		vars.connectedness.for.index <- c(vars.connectedness.for.index, new.var)
		augmented.data[new.var] <<- d
		total.343 <- total.343 + d
	}
	for (i in 1:length(vars.connectedness.tech.attitudes.429)) {
		var.name <- vars.connectedness.tech.attitudes.429[i]
		new.var <- paste(var.name, ".for.index", sep = "")
		c <- augmented.data[,var.name]
		d <- 5 - c
		# Convert "Don't know" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -1, 2)
		# Convert "Not Applicable" to "Neither agree nor disagree" - conservative guess
		d <- replace(d, d == -2, 2)
		vars.connectedness.for.index <- c(vars.connectedness.for.index, new.var)
		augmented.data[new.var] <<- d
		total.429 <- total.429 + d
	}

	augmented.data$total.277 <<- total.277
	augmented.data$total.280 <<- total.280
	augmented.data$total.287 <<- total.287
	augmented.data$total.343 <<- total.343
	augmented.data$total.429 <<- total.429

	# Max value: (Q277) + (Q280) + (Q287) + (Q343) + (Q429)
	# Max value: 13 * 1 + 13 * 1 + 6 * 2  + 8 * 5  + 9 * 4
	divisor <- 114
	augmented.data$connectedness.index <<- 100 * rowSums(augmented.data[,vars.connectedness.for.index]) / divisor
	return(augmented.data$connectedness.index)

}

sumIndex <- function() {
	sct <- sumCompetencies()
	sin <- sumInterests()
	sre <- sumResilience()
	scn <- sumConnectedness()
	augmented.data$combined.index <<- ( sct + sin + sre + scn ) / 4
	return ( augmented.data$combined.index )
}

generateIndexChart <- function() {

	sc <- data.frame(sort(sumCompetencies()))
	si <- data.frame(sort(sumInterests()))
	sr <- data.frame(sort(sumResilience()))
	ss <- data.frame(sort(sumConnectedness()))
	sa <- data.frame(sort(sumIndex()))
	sall <- cbind(sa, ss, sr, si, sc)
	colnames(sall)[1] <- "Combined"
	colnames(sall)[2] <- "Connectedness"
	colnames(sall)[3] <- "Resilience"
	colnames(sall)[4] <- "Interests"
	colnames(sall)[5] <- "Competencies"
	sall$id <- seq(1:length(sall$Competencies))

	msc <- melt(sall, id = "id")

	p <- ggplot(data=msc, aes(x = variable, y = value, fill = value)) +
		  geom_bar(stat="identity", position = "fill") +
  	  scale_y_continuous(breaks=seq(0 ,1, 0.25), labels=seq(0, 100, 25)) +
	    scale_fill_gradientn(colours=yawcrcPalette5) +
	    coord_flip() +
			labs(y = "Percentile scores", x = "", fill = "") +
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
        axis.text.x = element_text(color=text.color, size = axis.text.size),
        axis.text.y = element_text(color=text.color, size = axis.text.size)
        # axis.text.x = element_text(angle=45, vjust=1.0, hjust=1.0, size = axis.text.size),
        # axis.text.y = element_text(angle=45, size = axis.text.size)
    )
    return (p)
}
