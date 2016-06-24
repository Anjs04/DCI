# Sources
source("R/installDeps.R", FALSE)
source("R/utils.R", FALSE)

source("R/samplesForExpandedIndicators.R", FALSE)
source("R/histograms.R", FALSE)
INIT_MAPS <- FALSE
source("R/maps.R", FALSE)
source("R/indexFunctions.R", FALSE)
source("R/multivariateAnalysis.R", FALSE)

# Install dependencies, if they are not available
installDeps()

# Imports
library(gdata)
library(reshape2)
library(plyr)

print("Loading DCI data")

# Settings
PRINTING <- TRUE

# Name variables

vars <- c(

	"Q74_1", "Q74_2", "Q74_3", "Q74_4", "Q74_5", "Q74_6", "Q74_7", "Q74_8", "Q74_9", "Q74_10", "Q74_11", "Q74_12", "Q74_13", "Q74_14", "Q74_15",
			"Q431_16", "Q431_17", "Q431_18", "Q431_19", "Q431_20", "Q431_21", "Q431_22", "Q431_23", "Q431_24", "Q431_25", "Q431_26", "Q431_27", "Q431_28", "Q431_29", "Q431_30", "Q431_31", "Q431_32", "Q431_33", "Q431_34", "Q431_35", "Q431_36", "Q431_37", "Q431_38", "Q431_39", "Q431_40", "Q431_41", "Q431_42",
			"Q437_43", "Q437_44", "Q437_45", "Q437_46", "Q437_47", "Q437_49", "Q437_50", "Q437_51", "Q437_52", "Q437_53", "Q437_54",
			"Q341_55", "Q341_56", "Q341_57", "Q341_58", "Q341_59", "Q341_60", "Q341_61",
			"Q352_62", "Q352_63", "Q352_64", "Q352_65", "Q352_66", "Q352_67", "Q352_68", "Q352_69", "Q352_70", "Q352_71",
			"Q353_72", "Q353_73",
			"Q430_74", "Q430_75", "Q430_76", "Q430_77", "Q430_78", "Q430_79", "Q430_80", "Q430_81", "Q430_82", "Q430_83", "Q430_84", "Q430_85", "Q430_86", "Q430_87",
			"Q434_88", "Q434_89", "Q434_90", "Q434_91", "Q434_92", "Q434_93", "Q434_94", "Q434_95", "Q434_96", "Q434_97", "Q434_98",
			"Q435_99", "Q435_100", "Q435_101", "Q435_102", "Q435_103", "Q435_104", "Q435_105",
			"Q428_106", "Q428_107", "Q428_108", "Q428_109", "Q428_110", "Q428_111",
			"Q277_112", "Q277_113", "Q277_114", "Q277_115", "Q277_116", "Q277_117", "Q277_118", "Q277_119", "Q277_120", "Q277_121", "Q277_122", "Q277_123",
			"Q280_124", "Q280_125", "Q280_126", "Q280_127", "Q280_128", "Q280_129", "Q280_130", "Q280_131", "Q280_132", "Q280_133", "Q280_134", "Q280_135", "Q280_136",
			"Q287_137", "Q287_138", "Q287_139", "Q287_140", "Q287_141", "Q287_142",
			"Q343_143", "Q343_144", "Q343_145", "Q343_146", "Q343_147", "Q343_148", "Q343_149", "Q343_150",
			"Q429_151", "Q429_152", "Q429_153", "Q429_154", "Q429_155", "Q429_156", "Q429_157", "Q429_158", "Q429_159",
			"Q24_160", "Q24_161", "Q24_162", "Q24_163", "Q24_164", "Q24_165", "Q24_166", "Q24_167", "Q24_168", "Q24_169", "Q24_170",
			"Q30_170", "Q30_171",
			"Q30DK",
			"Q45",
			"Q425",
			"Q427",
			"Q436",
			"Q15A_1", "Q15A_2", "Q15A_3", "Q15A_4", "Q15A_5", "Q15A_6", "Q15A_7", "Q15A_8",
			"Q15Y_1", "Q15Y_2", "Q15Y_3", "Q15Y_4", "Q15Y_5", "Q15Y_6", "Q15Y_7", "Q15Y_8", "Q15Y_9", "Q15Y_10", "Q15Y_11", "Q15Y_12", "Q15Y_13", "Q15Y_14", "Q15Y_15", "Q15Y_16",
	"Q5_1", "Q5_2", "Q5_3", "Q5_4",
			"Q8",
			"Q9_212", "Q9_213", "Q9_214", "Q9_215", "Q9_216", "Q9_217", "Q9_218", "Q9_219", "Q9_220"
)

vars.competencies.online.activities.74 <- c("Q74_1", "Q74_2", "Q74_3", "Q74_4", "Q74_5", "Q74_6", "Q74_7", "Q74_8", "Q74_9", "Q74_10", "Q74_11", "Q74_12", "Q74_13", "Q74_14", "Q74_15")
vars.connectedness.maintenance.287 <- c("Q287_137", "Q287_138", "Q287_139", "Q287_140", "Q287_141", "Q287_142")
vars.interests.difference.seeking.341 <- c("Q341_55", "Q341_56", "Q341_57", "Q341_58", "Q341_59", "Q341_60", "Q341_61")
vars.connectedness.events.343 <- c("Q343_143", "Q343_144", "Q343_145", "Q343_146", "Q343_147", "Q343_148", "Q343_149", "Q343_150")
vars.interests.fitness.352 <- c("Q352_62", "Q352_63", "Q352_64", "Q352_65", "Q352_66", "Q352_67", "Q352_68", "Q352_69", "Q352_70", "Q352_71")
vars.interests.health.improvement.353 <- c("Q353_72", "Q353_73")
vars.resilience.engage.with.others.428 <- c("Q428_106", "Q428_107", "Q428_108", "Q428_109", "Q428_110", "Q428_111")
vars.connectedness.tech.attitudes.429 <- c("Q429_151", "Q429_152", "Q429_153", "Q429_154", "Q429_155", "Q429_156", "Q429_157", "Q429_158", "Q429_159")
vars.interests.keeping.in.touch.430 <- c("Q430_74", "Q430_75", "Q430_76", "Q430_77", "Q430_78", "Q430_79", "Q430_80", "Q430_81", "Q430_82", "Q430_83", "Q430_84", "Q430_85", "Q430_86", "Q430_87")
vars.competencies.431 <- c("Q431_16", "Q431_17", "Q431_18", "Q431_19", "Q431_20", "Q431_21", "Q431_22", "Q431_23", "Q431_24", "Q431_25", "Q431_26", "Q431_27", "Q431_28", "Q431_29", "Q431_30", "Q431_31", "Q431_32", "Q431_33", "Q431_34", "Q431_35", "Q431_36", "Q431_37", "Q431_38", "Q431_39", "Q431_40", "Q431_41", "Q431_42")
vars.resilience.harm.events.434 <- c("Q434_88", "Q434_89", "Q434_90", "Q434_91", "Q434_92", "Q434_93", "Q434_94", "Q434_95", "Q434_96", "Q434_97", "Q434_98")
vars.resilience.harms.agreement.435 <- c("Q435_99", "Q435_100", "Q435_101", "Q435_102", "Q435_103", "Q435_104", "Q435_105")
vars.interests.general.437 <- c("Q437_43", "Q437_44", "Q437_45", "Q437_46", "Q437_47", "Q437_49", "Q437_50", "Q437_51", "Q437_52", "Q437_53", "Q437_54")

# Binary
vars.connectedness.helping.others.277 <- c("Q277_112", "Q277_113", "Q277_114", "Q277_115", "Q277_116", "Q277_117", "Q277_118", "Q277_119", "Q277_120", "Q277_121", "Q277_122", "Q277_123")
vars.connectedness.sought.help.from.others.280 <- c("Q280_124", "Q280_125", "Q280_126", "Q280_127", "Q280_128", "Q280_129", "Q280_130", "Q280_131", "Q280_132", "Q280_133", "Q280_134", "Q280_135", "Q280_136")


vars.competencies <- c(
	vars.competencies.online.activities.74,
	vars.competencies.431
)
vars.interest <- c(
	vars.interests.general.437,
	vars.interests.difference.seeking.341,
	vars.interests.fitness.352,
	vars.interests.health.improvement.353,
	vars.interests.keeping.in.touch.430
)
vars.resilience <- c(
	vars.resilience.engage.with.others.428,
	vars.resilience.harm.events.434,
	vars.resilience.harms.agreement.435
)
vars.connectedness <- c(
	vars.connectedness.helping.others.277,
	vars.connectedness.sought.help.from.others.280,
	vars.connectedness.maintenance.287,
	vars.connectedness.events.343,
	vars.connectedness.tech.attitudes.429
)

vars.index <- c(
	vars.competencies,
	vars.interest,
	vars.resilience,
	vars.connectedness
)


vars.index.quals <- vars.index[(c(grep("Q277_*", vars.index), grep("Q280_*", vars.index)))]
vars.index.no.quals <- setdiff(vars.index, vars.index.quals)

# Group variables by type
vars.frequency <- c(
	vars.competencies.online.activities.74
)

vars.frequency.months <- c(
	vars.interests.general.437,
	vars.interests.fitness.352,
	vars.resilience.harm.events.434,
	vars.connectedness.events.343
)

vars.ease <- c(
	vars.competencies.431
)
vars.agreement <- c(
	vars.interests.difference.seeking.341,
	vars.interests.health.improvement.353,
	vars.resilience.harms.agreement.435,
	vars.resilience.engage.with.others.428,
	vars.connectedness.tech.attitudes.429
)

vars.importance <- c(
	vars.interests.keeping.in.touch.430,
	vars.connectedness.maintenance.287
)
vars.yes_no <- c(
	vars.connectedness.helping.others.277,
	vars.connectedness.sought.help.from.others.280
)

vars.all <- c(
	vars.frequency,
	vars.frequency.months,
	vars.ease,
	vars.agreement,
	vars.importance,
	vars.yes_no
)

vars.competency.totals <- c(
	"total.74",
	"total.431"
)
vars.interests.totals <- c(
	"total.437",
	"total.341",
	"total.352",
	"total.353",
	"total.430"
)
vars.resilience.totals <- c(
	"total.434",
	"total.435",
	"total.428"
)
vars.connectedness.totals <- c(
	"total.277",
	"total.280",
	"total.287",
	"total.343",
	"total.429"
)
vars.totals <- c(
	vars.competency.totals,
	vars.interests.totals,
	vars.resilience.totals,
	vars.connectedness.totals
)
vars.totals.quals <- vars.totals[(c(grep(".277", vars.totals), grep(".280", vars.totals)))]
vars.totals.no.quals <- setdiff(vars.totals, vars.totals.quals)



indicators <- loadIndicators()

# Expand valid indicators to include options, as sub-indicators
expandedIndicators <- generateExpandedVariableSet_Looped()

results <- loadSurveyResults()

# Show column names
# print(colnames(results))

age <- "Q10_159"
gender <- "Q1" # 193 = "Male", 194 = "Female", 195 = "Other", 196 = "Refused"
postcode <- "Q2_197_OTHER"
state <- "STATE1"
location <- "LOCATION1"
pressure.fin <- "Q45"
pressure.digital.access <- "Q425"
current.neighbourhood <- "Q427"
level.education <- "Q436"
main.activities <- "Q8"
minority.group.cultural.diverse <- "Q9_212"
minority.group.refugees <- "Q9_213"
minority.group.low.income <- "Q9_214"
minority.group.sole.parent <- "Q9_215"
minority.group.seniors <- "Q9_216"
minority.group.unemployed <- "Q9_217"
minority.group.indigenous <- "Q9_218"
minority.group.remote <- "Q9_219"
minority.group.homeless <- "Q9_220"
child.consent <- "Q5A"
child.age <- "Q5B"
augmented.data <- results[,c(age,
								gender,
								postcode,
								state,
								location,
								pressure.fin,
								pressure.digital.access,
								current.neighbourhood,
								level.education,
								main.activities,
								minority.group.cultural.diverse,
								minority.group.refugees,
								minority.group.low.income,
								minority.group.sole.parent,
								minority.group.seniors,
								minority.group.unemployed,
								minority.group.indigenous,
								minority.group.remote,
								minority.group.homeless,
								child.consent,
								child.age,
								vars)]

# Recode variables
augmented.data$age <- augmented.data$Q10_159
augmented.data$age[augmented.data$Q5A == 1 & !is.na(augmented.data$Q5A)] <- augmented.data$Q5B[augmented.data$Q5A == 1 & !is.na(augmented.data$Q5A)] + 11
# Find a better way, e.g. http://www.kkuniyuk.com/RTutorial1.pdf
augmented.data$age.breaks <- floor(augmented.data$Q10_159 / 5.0)
augmented.data$age.breaks <- NA
augmented.data$age.breaks[augmented.data$age >= 12 & augmented.data$age <= 17] <- "12 - 17"
augmented.data$age.breaks[augmented.data$age >= 18 & augmented.data$age <= 34] <- "18 - 34"
augmented.data$age.breaks[augmented.data$age >= 35 & augmented.data$age <= 49] <- "35 - 49"
augmented.data$age.breaks[augmented.data$age >= 50 & augmented.data$age <= 64] <- "50 - 64"
augmented.data$age.breaks[augmented.data$age >= 65] <- "65+"

augmented.data$gender <- NA
augmented.data$gender[augmented.data$Q1 == 193] <- "Male"
augmented.data$gender[augmented.data$Q1 == 194] <- "Female"
augmented.data$gender[augmented.data$Q1 == 195] <- "Other"
augmented.data$gender[augmented.data$Q1 == 196] <- "Refused"
augmented.data$state[augmented.data$STATE1 == 1] <- "ACT"
augmented.data$postcode <- as.character(augmented.data$Q2_197_OTHER)
# Add leading '0', to match SA4 codes for 3-digit postcodes
augmented.data[nchar(augmented.data$postcode)==3,]$postcode <- paste("0",augmented.data[nchar(augmented.data$postcode)==3,]$postcode, sep="")
augmented.data$state[augmented.data$STATE1 == 2] <- "NSW"
augmented.data$state[augmented.data$STATE1 == 3] <- "VIC"
augmented.data$state[augmented.data$STATE1 == 4] <- "QLD"
augmented.data$state[augmented.data$STATE1 == 5] <- "SA"
augmented.data$state[augmented.data$STATE1 == 6] <- "WA"
augmented.data$state[augmented.data$STATE1 == 7] <- "NT"
augmented.data$state[augmented.data$STATE1 == 8] <- "TAS"
augmented.data$location[augmented.data$LOCATION1 == 1] <- "Urban"
augmented.data$location[augmented.data$LOCATION1 == 2] <- "Regional/Rural"
augmented.data$pressure.financial <- NA
augmented.data$pressure.financial[augmented.data$Q45 == 171] <- "No pressure - my household rarely or never considers the costs of basic necessities."
augmented.data$pressure.financial[augmented.data$Q45 == 172] <- "Some pressure - my household monitors its budget closely, though is generally able to afford the costs of basic necessit"
augmented.data$pressure.financial[augmented.data$Q45 == 173] <- "Considerable pressure - my household monitors its budget closely and sometimes struggles to cover the costs of basic nec"
augmented.data$pressure.financial[augmented.data$Q45 == 174] <- "Extreme pressure - my household regularly struggles to cover the costs of basic necessities."
augmented.data$pressure.digital.access <- NA
augmented.data$pressure.digital.access[augmented.data$Q425 == 175] <- "No pressure at all."
augmented.data$pressure.digital.access[augmented.data$Q425 == 176] <- "Some pressure - my household notices the financial costs of our digital life."
augmented.data$pressure.digital.access[augmented.data$Q425 == 177] <- "Considerable pressure - my household monitor mobile phone usage, download limits and electricity use closely."
augmented.data$pressure.digital.access[augmented.data$Q425 == 178] <- "Extreme pressure - my household sometimes sacrifices other necessities to pay mobile phone or broadband expenses."
augmented.data$current.neighbourhood <- NA
augmented.data$current.neighbourhood[augmented.data$Q427 == 179] <- "Less than one week."
augmented.data$current.neighbourhood[augmented.data$Q427 == 180] <- "Less than one month."
augmented.data$current.neighbourhood[augmented.data$Q427 == 181] <- "Less than six months."
augmented.data$current.neighbourhood[augmented.data$Q427 == 182] <- "Less than three years."
augmented.data$current.neighbourhood[augmented.data$Q427 == 183] <- "More than three years."
augmented.data$level.education <- NA
augmented.data$level.education[augmented.data$Q436 == 179] <- "No school."
augmented.data$level.education[augmented.data$Q436 == 180] <- "Primary school."
augmented.data$level.education[augmented.data$Q436 == 181] <- "Some secondary school."
augmented.data$level.education[augmented.data$Q436 == 182] <- "Finished secondary school."
augmented.data$level.education[augmented.data$Q436 == 183] <- "Trade training."
augmented.data$level.education[augmented.data$Q436 == 184] <- "University or college (undergraduate)."
augmented.data$level.education[augmented.data$Q436 == 185] <- "University or college (postgraduate)."
augmented.data$level.education[augmented.data$Q436 == 186] <- "Don't know."
augmented.data$level.education[augmented.data$Q436 == 187] <- "Refused."
augmented.data$main.activities <- NA
augmented.data$main.activities[augmented.data$Q8 == 200] <- "Full-time work greater than or equal to 30 hours paid employment per week"
augmented.data$main.activities[augmented.data$Q8 == 201] <- "Part-time work less than 30 hours paid employment per week"
augmented.data$main.activities[augmented.data$Q8 == 202] <- "Unemployed/looking for work"
augmented.data$main.activities[augmented.data$Q8 == 203] <- "Home duties"
augmented.data$main.activities[augmented.data$Q8 == 204] <- "Have a job but not at work due to illness, vacation etc"
augmented.data$main.activities[augmented.data$Q8 == 205] <- "Not working and currently receiving sickness allowance/disability support pension."
augmented.data$main.activities[augmented.data$Q8 == 206] <- "Volunteer work"
augmented.data$main.activities[augmented.data$Q8 == 207] <- "Student attending school"
augmented.data$main.activities[augmented.data$Q8 == 208] <- "Student attending university, TAFE or other tertiary education provider"
augmented.data$main.activities[augmented.data$Q8 == 209] <- "Other"
augmented.data$main.activities[augmented.data$Q8 == 210] <- "Don't know"
augmented.data$main.activities[augmented.data$Q8 == 211] <- "Refused"
# Minority groupsaugmented.data$current.neighbourhood <- NA
augmented.data$culturally.and.linguistically.diverse[augmented.data$Q9_212 == 1] <- "YES"
augmented.data$culturally.and.linguistically.diverse[augmented.data$Q9_212 == 2] <- "NO"
augmented.data$refugees.and.asylum.seekers[augmented.data$Q9_213 == 1] <- "YES"
augmented.data$refugees.and.asylum.seekers[augmented.data$Q9_213 == 2] <- "NO"
augmented.data$low.income.households[augmented.data$Q9_214 == 1] <- "YES"
augmented.data$low.income.households[augmented.data$Q9_214 == 2] <- "NO"
augmented.data$sole.parent.families[augmented.data$Q9_215 == 1] <- "YES"
augmented.data$sole.parent.families[augmented.data$Q9_215 == 2] <- "NO"
augmented.data$seniors[augmented.data$Q9_216 == 1] <- "YES"
augmented.data$seniors[augmented.data$Q9_216 == 2] <- "NO"
augmented.data$unemployed.or.underemployed[augmented.data$Q9_217 == 1] <- "YES"
augmented.data$unemployed.or.underemployed[augmented.data$Q9_217 == 2] <- "NO"
augmented.data$indigenous.communities[augmented.data$Q9_218 == 1] <- "YES"
augmented.data$indigenous.communities[augmented.data$Q9_218 == 2] <- "NO"
augmented.data$people.in.remote.communities[augmented.data$Q9_219 == 1] <- "YES"
augmented.data$people.in.remote.communities[augmented.data$Q9_219 == 2] <- "NO"
augmented.data$homeless[augmented.data$Q9_220 == 1] <- "YES"
augmented.data$homeless[augmented.data$Q9_220 == 2] <- "NO"

# Household size
household.spouse.or.partner <- "Q24_160"
household.children <- "Q24_161"
household.father <- "Q24_162"
household.mother <- "Q24_163"
household.fathers.partner.or.step.parent <- "Q24_164"
household.mothers.partner.or.step.parent <- "Q24_165"
household.older.siblings <- "Q24_166"
household.younger.siblings <- "Q24_167"
household.grandparents <- "Q24_168"
household.other.relatives <- "Q24_169"
household.other.people <- "Q24_170"
household.vars <- c(
	household.spouse.or.partner,
	household.children,
	household.father,
	household.mother,
	household.fathers.partner.or.step.parent,
	household.mothers.partner.or.step.parent,
	household.older.siblings,
	household.younger.siblings,
	household.grandparents,
	household.other.relatives,
	household.other.people
)
augmented.data$household.size <- rowSums(subset(augmented.data, select = household.vars))
# Remove rows where household size > 10
augmented.data <- augmented.data[augmented.data$household.size <= 10,]
augmented.data$household.with.children.5.to.17 <- ifelse(augmented.data$Q5_2 == 1 | augmented.data$Q5_3 == 1, 1, 0)
# sum(augmented.data$household.with.children.5.to.17, na.rm = TRUE)

# Create sums
augmented.data$Q74 <- round(rowMeans(augmented.data[,vars.competencies.online.activities.74])	)
augmented.data$Q287 <- round(rowMeans(augmented.data[,vars.connectedness.maintenance.287]))
augmented.data$Q341 <- round(rowMeans(augmented.data[,vars.interests.difference.seeking.341]))
augmented.data$Q343 <- round(rowMeans(augmented.data[,vars.connectedness.events.343]))
augmented.data$Q352 <- round(rowMeans(augmented.data[,vars.interests.fitness.352]))
augmented.data$Q353 <- round(rowMeans(augmented.data[,vars.interests.health.improvement.353]))
augmented.data$Q428 <- round(rowMeans(augmented.data[,vars.resilience.engage.with.others.428]))
augmented.data$Q429 <- round(rowMeans(augmented.data[,vars.connectedness.tech.attitudes.429]))
augmented.data$Q430 <- round(rowMeans(augmented.data[,vars.interests.keeping.in.touch.430]))
augmented.data$Q431 <- round(rowMeans(augmented.data[,vars.competencies.431]))
augmented.data$Q434 <- round(rowMeans(augmented.data[,vars.resilience.harm.events.434]))
augmented.data$Q435 <- round(rowMeans(augmented.data[,vars.resilience.harms.agreement.435]))
augmented.data$Q437 <- round(rowMeans(augmented.data[,vars.interests.general.437]))

## Generate totals
sumIndex()

## Add coordinates

# Make sure initMaps() is called first. Turn off when debugging
# initMaps()
augmented.data$SA4_NAME_2011 <- unlist(sapply(augmented.data$postcode, obtainSA4))
augmented.data.with.coords <- initAugmentedDataWithCoords()

# Shorthand
data <- augmented.data

# Generate scaled versions of data
data.scaled <- cbind(data[,!(names(data) %in% c(vars.index, vars.totals))], scale(data[,c(vars.index, vars.totals)]))



## Demographcs

sampleSize <- function() {
	return (length(augmented.data[,1]))
}


ageBreakdown <- function() {
	return (length(augmented.data[,1]))
}

generateFrequencies <- function() {
	generateAgeFrequencies(vars.frequency, frequencyLabels)
	generateAgeFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateAgeFrequencies(vars.ease, easeLabels)
	generateAgeFrequencies(vars.agreement, agreementLabels)
	generateAgeFrequencies(vars.importance, importanceLabels)
	generateAgeFrequencies(vars.yes_no, yesNoLabels)

	generateGenderFrequencies(vars.frequency, frequencyLabels)
	generateGenderFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateGenderFrequencies(vars.ease, easeLabels)
	generateGenderFrequencies(vars.agreement, agreementLabels)
	generateGenderFrequencies(vars.importance, importanceLabels)
	generateGenderFrequencies(vars.yes_no, yesNoLabels)

	generateStateFrequencies(vars.frequency, frequencyLabels)
	generateStateFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateStateFrequencies(vars.ease, easeLabels)
	generateStateFrequencies(vars.agreement, agreementLabels)
	generateStateFrequencies(vars.importance, importanceLabels)
	generateStateFrequencies(vars.yes_no, yesNoLabels)

	generateLocationFrequencies(vars.frequency, frequencyLabels)
	generateLocationFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateLocationFrequencies(vars.ease, easeLabels)
	generateLocationFrequencies(vars.agreement, agreementLabels)
	generateLocationFrequencies(vars.importance, importanceLabels)
	generateLocationFrequencies(vars.yes_no, yesNoLabels)

	generateEducationFrequencies(vars.frequency, frequencyLabels)
	generateEducationFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateEducationFrequencies(vars.ease, easeLabels)
	generateEducationFrequencies(vars.agreement, agreementLabels)
	generateEducationFrequencies(vars.importance, importanceLabels)
	generateEducationFrequencies(vars.yes_no, yesNoLabels)

	generateOccupationFrequencies(vars.frequency, frequencyLabels)
	generateOccupationFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateOccupationFrequencies(vars.ease, easeLabels)
	generateOccupationFrequencies(vars.agreement, agreementLabels)
	generateOccupationFrequencies(vars.importance, importanceLabels)
	generateOccupationFrequencies(vars.yes_no, yesNoLabels)

	generateMinorityFrequencies(vars.frequency, frequencyLabels)
	generateMinorityFrequencies(vars.frequency.months, frequencyMonthLabels)
	generateMinorityFrequencies(vars.ease, easeLabels)
	generateMinorityFrequencies(vars.agreement, agreementLabels)
	generateMinorityFrequencies(vars.importance, importanceLabels)
	generateMinorityFrequencies(vars.yes_no, yesNoLabels)

}


generateAgeFrequenciesForAggregate <- function() {
	generateSingleAgeFrequency(0, c("Q74"), frequencyLabels)
	generateSingleAgeFrequency(0, c("Q287"), importanceLabels)
	generateSingleAgeFrequency(0, c("Q341"), agreementLabels)
	generateSingleAgeFrequency(0, c("Q343"), frequencyMonthLabels)
	generateSingleAgeFrequency(0, c("Q352"), frequencyMonthLabels)
	generateSingleAgeFrequency(0, c("Q353"), agreementLabels)
	generateSingleAgeFrequency(0, c("Q428"), agreementLabels)
	generateSingleAgeFrequency(0, c("Q429"), agreementLabels)
	generateSingleAgeFrequency(0, c("Q430"), importanceLabels)
	generateSingleAgeFrequency(0, c("Q431"), easeLabels)
	generateSingleAgeFrequency(0, c("Q434"), frequencyMonthLabels)
	generateSingleAgeFrequency(0, c("Q435"), agreementLabels)
	generateSingleAgeFrequency(0, c("Q437"), frequencyMonthLabels)
}

generateGenderFrequenciesForAggregate <- function() {

	generateSingleGenderFrequency(0, c("Q74"), frequencyLabels)
	generateSingleGenderFrequency(0, c("Q287"), importanceLabels)
	generateSingleGenderFrequency(0, c("Q341"), agreementLabels)
	generateSingleGenderFrequency(0, c("Q343"), frequencyMonthLabels)
	generateSingleGenderFrequency(0, c("Q352"), frequencyMonthLabels)
	generateSingleGenderFrequency(0, c("Q353"), agreementLabels)
	generateSingleGenderFrequency(0, c("Q428"), agreementLabels)
	generateSingleGenderFrequency(0, c("Q429"), agreementLabels)
	generateSingleGenderFrequency(0, c("Q430"), importanceLabels)
	generateSingleGenderFrequency(0, c("Q431"), easeLabels)
	generateSingleGenderFrequency(0, c("Q434"), frequencyMonthLabels)
	generateSingleGenderFrequency(0, c("Q435"), agreementLabels)
	generateSingleGenderFrequency(0, c("Q437"), frequencyMonthLabels)

}

generateStateFrequenciesForAggregate <- function() {

	generateSingleStateFrequency(0, c("Q74"), frequencyLabels)
	generateSingleStateFrequency(0, c("Q287"), importanceLabels)
	generateSingleStateFrequency(0, c("Q341"), agreementLabels)
	generateSingleStateFrequency(0, c("Q343"), frequencyMonthLabels)
	generateSingleStateFrequency(0, c("Q352"), frequencyMonthLabels)
	generateSingleStateFrequency(0, c("Q353"), agreementLabels)
	generateSingleStateFrequency(0, c("Q429"), agreementLabels)
	generateSingleStateFrequency(0, c("Q430"), importanceLabels)
	generateSingleStateFrequency(0, c("Q431"), easeLabels)
	generateSingleStateFrequency(0, c("Q434"), frequencyMonthLabels)
	generateSingleStateFrequency(0, c("Q435"), agreementLabels)
	generateSingleStateFrequency(0, c("Q437"), frequencyMonthLabels)

}


generateLocationFrequenciesForAggregate <- function() {

	generateSingleLocationFrequency(0, c("Q74"), frequencyLabels)
	generateSingleLocationFrequency(0, c("Q287"), importanceLabels)
	generateSingleLocationFrequency(0, c("Q341"), agreementLabels)
	generateSingleLocationFrequency(0, c("Q343"), frequencyMonthLabels)
	generateSingleLocationFrequency(0, c("Q352"), frequencyMonthLabels)
	generateSingleLocationFrequency(0, c("Q353"), agreementLabels)
	generateSingleLocationFrequency(0, c("Q428"), agreementLabels)
	generateSingleLocationFrequency(0, c("Q429"), agreementLabels)
	generateSingleLocationFrequency(0, c("Q430"), importanceLabels)
	generateSingleLocationFrequency(0, c("Q431"), easeLabels)
	generateSingleLocationFrequency(0, c("Q434"), frequencyMonthLabels)
	generateSingleLocationFrequency(0, c("Q435"), agreementLabels)
	generateSingleLocationFrequency(0, c("Q437"), frequencyMonthLabels)

}


generateEducationFrequenciesForAggregate <- function() {

	generateSingleEducationFrequency(0, c("Q74"), frequencyLabels)
	generateSingleEducationFrequency(0, c("Q287"), importanceLabels)
	generateSingleEducationFrequency(0, c("Q341"), agreementLabels)
	generateSingleEducationFrequency(0, c("Q343"), frequencyMonthLabels)
	generateSingleEducationFrequency(0, c("Q352"), frequencyMonthLabels)
	generateSingleEducationFrequency(0, c("Q353"), agreementLabels)
	generateSingleEducationFrequency(0, c("Q428"), agreementLabels)
	generateSingleEducationFrequency(0, c("Q429"), agreementLabels)
	generateSingleEducationFrequency(0, c("Q430"), importanceLabels)
	generateSingleEducationFrequency(0, c("Q431"), easeLabels)
	generateSingleEducationFrequency(0, c("Q434"), frequencyMonthLabels)
	generateSingleEducationFrequency(0, c("Q435"), agreementLabels)
	generateSingleEducationFrequency(0, c("Q437"), frequencyMonthLabels)

}


generateOccupationFrequenciesForAggregate <- function() {

	generateSingleOccupationFrequency(0, c("Q74"), frequencyLabels)
	generateSingleOccupationFrequency(0, c("Q287"), importanceLabels)
	generateSingleOccupationFrequency(0, c("Q341"), agreementLabels)
	generateSingleOccupationFrequency(0, c("Q343"), frequencyMonthLabels)
	generateSingleOccupationFrequency(0, c("Q352"), frequencyMonthLabels)
	generateSingleOccupationFrequency(0, c("Q353"), agreementLabels)
	generateSingleOccupationFrequency(0, c("Q428"), agreementLabels)
	generateSingleOccupationFrequency(0, c("Q429"), agreementLabels)
	generateSingleOccupationFrequency(0, c("Q430"), importanceLabels)
	generateSingleOccupationFrequency(0, c("Q431"), easeLabels)
	generateSingleOccupationFrequency(0, c("Q434"), frequencyMonthLabels)
	generateSingleOccupationFrequency(0, c("Q435"), agreementLabels)
	generateSingleOccupationFrequency(0, c("Q437"), frequencyMonthLabels)

}


generateMinorityFrequenciesForAggregate <- function() {

	generateSingleMinorityFrequency(0, c("Q74"), frequencyLabels)
	generateSingleMinorityFrequency(0, c("Q287"), importanceLabels)
	generateSingleMinorityFrequency(0, c("Q341"), agreementLabels)
	generateSingleMinorityFrequency(0, c("Q343"), frequencyMonthLabels)
	generateSingleMinorityFrequency(0, c("Q352"), frequencyMonthLabels)
	generateSingleMinorityFrequency(0, c("Q353"), agreementLabels)
	generateSingleMinorityFrequency(0, c("Q428"), agreementLabels)
	generateSingleMinorityFrequency(0, c("Q429"), agreementLabels)
	generateSingleMinorityFrequency(0, c("Q430"), importanceLabels)
	generateSingleMinorityFrequency(0, c("Q431"), easeLabels)
	generateSingleMinorityFrequency(0, c("Q434"), frequencyMonthLabels)
	generateSingleMinorityFrequency(0, c("Q435"), agreementLabels)
	generateSingleMinorityFrequency(0, c("Q437"), frequencyMonthLabels)

}


generateAggregateFrequences <- function() {

	generateAgeFrequenciesForAggregate()
	generateGenderFrequenciesForAggregate()
	generateStateFrequenciesForAggregate()
	generateLocationFrequenciesForAggregate()
	generateEducationFrequenciesForAggregate()
	generateOccupationFrequenciesForAggregate()
	generateMinorityFrequenciesForAggregate()

}


generateAllSA4Maps <- function() {

	# Generate maps for single statements
	generateSA4MapForVariable(vars.frequency, frequencyLabels)
	print("generateSA4MapForVariable(vars.frequency, frequencyLabels)")
	generateSA4MapForVariable(vars.frequency.months, frequencyMonthLabels)
	print("generateSA4MapForVariable(vars.frequency.months, frequencyMonthLabels)")
	generateSA4MapForVariable(vars.ease, easeLabels)
	print("generateSA4MapForVariable(vars.ease, easeLabels)")
	generateSA4MapForVariable(vars.agreement, agreementLabels)
	print("generateSA4MapForVariable(vars.agreement, agreementLabels)")
	generateSA4MapForVariable(vars.importance, importanceLabels)
	print("generateSA4MapForVariable(vars.importance, importanceLabels)")
	generateSA4MapForVariable(vars.yes_no, yesNoLabels)
	print("generateSA4MapForVariable(vars.yes_no, yesNoLabels)")

	# Generate maps for variable aggregates
	generateSA4Map(0, c("Q74"), frequencyLabels)
	print("generateSA4Map(0, c(Q74), frequencyLabels)")
	generateSA4Map(0, c("Q287"), importanceLabels)
	print("generateSA4Map(0, c(Q287), importanceLabels)")
	generateSA4Map(0, c("Q341"), agreementLabels)
	print("generateSA4Map(0, c(Q341), agreementLabels)")
	generateSA4Map(0, c("Q343"), frequencyMonthLabels)
	print("generateSA4Map(0, c(Q343), frequencyMonthLabels)")
	generateSA4Map(0, c("Q352"), frequencyMonthLabels)
	print("generateSA4Map(0, c(Q352), frequencyMonthLabels)")
	generateSA4Map(0, c("Q353"), agreementLabels)
	print("generateSA4Map(0, c(Q353), agreementLabels)")
	generateSA4Map(0, c("Q428"), agreementLabels)
	print("generateSA4Map(0, c(Q428), agreementLabels)")
	generateSA4Map(0, c("Q429"), agreementLabels)
	print("generateSA4Map(0, c(Q429), agreementLabels)")
	generateSA4Map(0, c("Q430"), importanceLabels)
	print("generateSA4Map(0, c(Q430), importanceLabels)")
	generateSA4Map(0, c("Q431"), easeLabels)
	print("generateSA4Map(0, c(Q431), easeLabels)")
	generateSA4Map(0, c("Q434"), frequencyMonthLabels)
	print("generateSA4Map(0, c(Q434), frequencyMonthLabels)")
	generateSA4Map(0, c("Q435"), agreementLabels)
	print("generateSA4Map(0, c(Q435), agreementLabels)")
	generateSA4Map(0, c("Q437"), frequencyMonthLabels)
	print("generateSA4Map(0, c(Q437), frequencyMonthLabels)")

}


generateAllScatterMaps <- function() {

	# Generate maps for single statements
	generateScatterMapForVariable(vars.frequency, frequencyLabels, yawcrcPalette8)
	print("generateScatterMapForVariable(vars.frequency, frequencyLabels)")
	generateScatterMapForVariable(vars.frequency.months, frequencyMonthLabels, yawcrcPalette7)
	print("generateScatterMapForVariable(vars.frequency.months, frequencyMonthLabels)")
	generateScatterMapForVariable(vars.ease, easeLabels, yawcrcPalette7)
	print("generateScatterMapForVariable(vars.ease, easeLabels)")
	generateScatterMapForVariable(vars.agreement, agreementLabels, yawcrcPalette7)
	print("generateScatterMapForVariable(vars.agreement, agreementLabels)")
	generateScatterMapForVariable(vars.importance, importanceLabels, yawcrcPalette7)
	print("generateScatterMapForVariable(vars.importance, importanceLabels)")
	generateScatterMapForVariable(vars.yes_no, yesNoLabels, yawcrcPalette7)
	print("generateScatterMapForVariable(vars.yes_no, yesNoLabels)")

	# Generate maps for variable aggregates
	generateScatterMap(0, c("Q74"), frequencyLabels, yawcrcPalette8)
	print("generateScatterMap(0, c(Q74), frequencyLabels)")
	generateScatterMap(0, c("Q287"), importanceLabels, yawcrcPalette7)
	print("generateScatterMap(0, c(Q287), importanceLabels)")
	generateScatterMap(0, c("Q341"), agreementLabels, yawcrcPalette7)
	print("generateScatterMap(0, c(Q341), agreementLabels)")
	generateScatterMap(0, c("Q343"), frequencyMonthLabels, yawcrcPalette7)
	print("generateScatterMap(0, c(Q343), frequencyMonthLabels)")
	generateScatterMap(0, c("Q352"), frequencyMonthLabels, yawcrcPalette7)
	print("generateScatterMap(0, c(Q352), frequencyMonthLabels)")
	generateScatterMap(0, c("Q353"), agreementLabels, yawcrcPalette7)
	print("generateScatterMap(0, c(Q353), agreementLabels)")
	generateScatterMap(0, c("Q428"), agreementLabels, yawcrcPalette7)
	print("generateScatterMap(0, c(Q428), agreementLabels)")
	generateScatterMap(0, c("Q429"), agreementLabels, yawcrcPalette7)
	print("generateScatterMap(0, c(Q429), agreementLabels)")
	generateScatterMap(0, c("Q430"), importanceLabels, yawcrcPalette7)
	print("generateScatterMap(0, c(Q430), importanceLabels)")
	generateScatterMap(0, c("Q431"), easeLabels, yawcrcPalette7)
	print("generateScatterMap(0, c(Q431), easeLabels)")
	generateScatterMap(0, c("Q434"), frequencyMonthLabels, yawcrcPalette7)
	print("generateScatterMap(0, c(Q434), frequencyMonthLabels)")
	generateScatterMap(0, c("Q435"), agreementLabels, yawcrcPalette7)
	print("generateScatterMap(0, c(Q435), agreementLabels)")
	generateScatterMap(0, c("Q437"), frequencyMonthLabels, yawcrcPalette7)
	print("generateScatterMap(0, c(Q437), frequencyMonthLabels)")

}



generateAllMaps <- function() {

	generateAllSA4Maps()
	generateAllScatterMaps()

}


# Generate all subquestion charts
generateSubQuestionCharts <- function() {

	graphSubQuestionFrequencies(vars.competencies.online.activities.74, "Frequency", frequencyLabels, "online-activities-74")
	graphSubQuestionFrequencies(vars.competencies.431, "Ease", easeLabels, "competencies-ease-of-tasks-431")

	graphSubQuestionFrequencies(vars.interests.difference.seeking.341, "Agreement", agreementLabels, "interests-difference-seeking-341")
	graphSubQuestionFrequencies(vars.interests.fitness.352, "Frequency", frequencyMonthLabels, "interests-fitness-352")
	graphSubQuestionFrequencies(vars.interests.health.improvement.353, "Agreement", agreementLabels, "interests-health-improvement-353")
	graphSubQuestionFrequencies(vars.interests.general.437, "Frequency", frequencyMonthLabels, "interests-general-437")
	graphSubQuestionFrequencies(vars.interests.keeping.in.touch.430, "Importance", importanceLabels, "keeping-in-touch-430")

	graphSubQuestionFrequencies(vars.resilience.engage.with.others.428, "Agreement", agreementLabels, "resilience-engage-with-others-428")
	graphSubQuestionFrequencies(vars.resilience.harm.events.434, "Frequency", frequencyMonthLabels, "resilience-harm-events-434")
	graphSubQuestionFrequencies(vars.resilience.harms.agreement.435, "Agreement", agreementLabels, "resilience-harms-agreement-435")

	graphSubQuestionFrequencies(vars.connectedness.tech.attitudes.429, "Agreement", agreementLabels, "connectedness-tech-attitudes-429")
	graphSubQuestionFrequencies(vars.connectedness.maintenance.287, "Importance", importanceLabels, "maintaining-connections-287")
	graphSubQuestionFrequencies(vars.connectedness.events.343, "Frequency", frequencyMonthLabels, "connectedness-events-343")

}


generatePrebuiltCorrelations <- function() {

  generateCorrelation("total.431", "Q436", "skills-education")
	generateCorrelation("total.437", "Q436", "interests-education")
	# Need t-tests instead
	generateCorrelation("total.277", "total.431", "helping-others-skills")
	generateCorrelation("total.277", "total.74", "helping-others-use")
	generateCorrelation("total.280", "total.431", "seeking-help-skills")
	generateCorrelation("total.280", "total.74", "seeking-help-use")
	generateCorrelation("total.437", "total.74", "helping-others-use")

	generateCorrelation("total.74", "total.435", "use-resilience")
	generateCorrelation("total.74", "total.434", "use-harms")
	generateCorrelation("total.431", "total.435", "skills-resilience")
	generateCorrelation("Q431_32", "total.435", "information-resilience")
	generateCorrelation("Q434_91", "total.435", "attitude-harm-1-resilience")
	generateCorrelation("Q434_92", "total.435", "attitude-harm-2-resilience")
	generateCorrelation("Q434_93", "total.435", "attitude-harm-3-resilience")

}


generateAllIndividualCorrelations <- function() {

	generateCorrelationsExploratory()

}


runAll <- function() {

	generateFrequencies()
	generateAggregateFrequences()
	generateSubQuestionCharts()
	generateAllMaps()
	generatePrebuiltCorrelations()
	generateAllIndividualCorrelations()

}
