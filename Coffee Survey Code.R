install.packages("gtsummary")
library(gtsummary)
table(coffee_survey$cups)
table(coffee_survey$favorite)
table(coffee_survey$style)
table(coffee_survey$strength)
table(coffee_survey$roast_level)
table(coffee_survey$caffeine)
table(coffee_survey$expertise)
table(coffee_survey$gender)
table(coffee_survey$gender_specify)
tbl_summary(
	coffee_survey,
	by = age,
	include = c(cups, favorite, style, strength, roast_level, caffeine))

tbl_summary(
	coffee_survey,
	by = age,
	include = c(cups, favorite, style, strength, roast_level, caffeine),
	label = list(
		cups ~ "Cups of coffee per day",
		favorite ~ "Favorite coffee drink",
		style ~ "Preferred coffee style",
		strength ~ "Preferred coffee strength",
		roast_level ~ "Preferred coffee roast level",
		caffeine ~ "Preffered caffeine amount"
	),
	missing_text = "Missing")

tbl_summary(
	coffee_survey,
	by = cups_bin,
	include = c(strength, roast_level, caffeine),
	label = list(
		strength ~ "Preferred coffee strength",
		roast_level ~ "Preferred coffee roast level",
		caffeine ~ "Preffered caffeine amount"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")

install.packages("janitor")
library(janitor)
install.packages("speff2trial")
library(speff2trial)
install.packages("fitdistrplus")
library(fitdistrplus)
library(tidyverse)

coffee_survey1 <- drop_na(coffee_survey, age)
coffee_survey2 <- drop_na(coffee_survey1, strength)
coffee_survey3 <- drop_na(coffee_survey2, roast_level)
coffee_survey4 <- drop_na(coffee_survey3, caffeine)

coffee_survey$cups_bin <- ifelse(coffee_survey$cups <= 2, 0, 1)

tbl_uvregression(
	coffee_survey,
	y = cups_bin,
	include = c(strength, roast_level, caffeine),
	method = lm)

barplot(table(coffee_survey$roast_level),
				main = "Barplot of Roast Levels",
				xlab = "Roast Level",
				ylab = "Frequency",
				col = "lightblue")

table(coffee_survey$number_children, coffee_survey$cups_bin)

poisson_model <- glm(cups_bin ~ strength + roast_level + caffeine,
										 data = coffee_survey,
										 family = poisson)
tbl_regression(
	poisson_model,
	exponentiate = TRUE,
	label = list(
		strength ~ "Strength",
		roast_level ~ "Roast Level",
		caffeine ~ "Caffeine Amount"
	)
)








