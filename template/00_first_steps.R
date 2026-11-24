# Run one section at a time:
# Ctrl+Enter (Windows/Linux) or Cmd+Return (Mac).
# Open the workshop's RStudio project first.

# Basic calculations and vectors
2 + 3

publications <- c(40, 75, 60)
staff <- c(10, 15, 20)

publications / staff
mean(publications)

# These three universities are invented.
toy <- data.frame(
  university = c("A", "B", "C"),
  publications,
  staff
)

toy$papers_per_staff <- toy$publications / toy$staff

toy
subset(toy, staff >= 15)

# Read the prepared university panel.
# No Excel import or data preparation is needed here.
getwd()
file.exists("teaching_panel_2020_2023.csv")

d <- read.csv("teaching_panel_2020_2023.csv")

head(d[c("id", "year", "publications", "faculty")])
str(d[c("id", "year", "publications", "faculty")])

dim(d)
table(d$year)
anyDuplicated(d[c("id", "year")])
summary(d$publications)
sum(is.na(d$publications))

# Plot the 2023 cross section.
cs <- subset(d, year == 2023)

plot(
  log(cs$faculty),
  log(cs$publications),
  xlab = "Log faculty",
  ylab = "Log publications",
  pch = 19
)

mean_fit <- lm(
  log(publications) ~ log(faculty),
  data = cs
)

abline(mean_fit, col = "darkgreen", lwd = 2)
summary(mean_fit)

# The slope measures an association.
# Staffing has not been randomly assigned.

# Save an example table in the same working folder.
write.csv(
  head(d),
  "results_my_first_table.csv",
  row.names = FALSE
)

# Save your script with Ctrl+S / Cmd+S.
# Do not type the printed ">" Console prompt.
# For help, try ?mean or ?lm.