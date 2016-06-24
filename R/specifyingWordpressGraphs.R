vars.resilience.harm.events.434       <- c("Q434_88", "Q434_89", "Q434_90", "Q434_91", "Q434_92", "Q434_93", 
                                           "Q434_94", "Q434_95", "Q434_96", "Q434_97", "Q434_98")
vars.resilience.harm.events.434.part1 <- c("Q434_88", "Q434_89", "Q434_90", "Q434_91", "Q434_92", "Q434_93")
vars.resilience.harm.events.434.part2 <- c("Q434_94", "Q434_95", "Q434_96", "Q434_97", "Q434_98")


gg <- graphSubQuestionFrequencies(vars.resilience.harm.events.434, "Frequency", frequencyMonthLabels, "resilience-harm-events-434", -8.5)

gg <- graphSubQuestionFrequencies(vars.resilience.harm.events.434.part1, "Frequency", frequencyMonthLabels, "resilience-harm-events-434-steps-to-address-risks", -7)
gg <- graphSubQuestionFrequencies(vars.resilience.harm.events.434.part2, "Frequency", frequencyMonthLabels, "resilience-harm-events-434-harmful-events", -7)

attitudes.to.risks <- c("Q435_103", "Q435_104", "Q435_105")

png.height = 4
gg <- graphSubQuestionFrequencies(attitudes.to.risks, "Agreement", agreementLabels, "attitudes-to-risks-435", -5)

