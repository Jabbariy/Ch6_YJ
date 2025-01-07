
library(jmv)

data <- read_excel("C:/Users/yasam/Desktop/Judy/Github Ch6/Ch6_YJ.xlsx")


#  repeated-measures ANOVA for accuracy
accuracy_anova <- jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label = "Reference frame",
      levels = c("Ego", "Allo"))),
  rmCells = list(
    list(
      measure = "AccSame",
      cell = "Ego"),
    list(
      measure = "AccDiff",
      cell = "Allo")),
  bs = LandmarkCondition,
  cov = DriveFrequency,
  rmTerms = ~ `Reference frame`,
  bsTerms = ~ LandmarkCondition + DriveFrequency,
  postHoc = list(
    "Reference frame",
    "LandmarkCondition")
)

print(accuracy_anova)


#  repeated-measures ANOVA for reaction time
reaction_time_anova <- jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label = "Reference frame",
      levels = c("Ego", "Allo"))),
  rmCells = list(
    list(
      measure = "rt_test_same",
      cell = "Ego"),
    list(
      measure = "rt_test_diff",
      cell = "Allo")),
  bs = LandmarkCondition,
  rmTerms = ~ `Reference frame`,
  bsTerms = ~ LandmarkCondition,
  postHoc = list(
    "Reference frame")
)

print(reaction_time_anova)

