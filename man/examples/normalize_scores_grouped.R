# setup - create necessary objects #
suppressMessages({
  age_grouping <- GroupConditions(
    conditions_category = "Age",
    "below 22" ~ age < 22,
    "23-60" ~ age >= 23 & age <= 60,
    "above 60" ~ age > 60
  )
  sex_grouping <- GroupConditions(
    conditions_category = "Sex",
    "Male" ~ sex == "M",
    "Female" ~ sex == "F"
  )
  NEU_gft <- GroupedFrequencyTable(
    data = IPIP_NEO_300,
    conditions = list(age_grouping, sex_grouping),
    var = "N"
  )
  NEU_gst <- GroupedScoreTable(
    NEU_gft,
    scale = list(STEN, STANINE)
  )
})

#### normalize scores ####
# to Z score or quantile using GroupedFrequencyTable
normalized_to_quan <- normalize_scores_grouped(
  IPIP_NEO_300,
  vars = "N",
  NEU_gft,
  what = "quan",
  retain = c("sex", "age")
)

# only 'sex' and 'age' are retained
head(normalized_to_quan)

# to StandardScale attached to GroupedScoreTable
normalized_to_STEN <- normalize_scores_grouped(
  IPIP_NEO_300,
  vars = "N",
  NEU_gst,
  what = "stanine",
  retain = FALSE,
  group_col = "sex_age_group"
)

# none is retained, 'sex_age_group' is created
head(normalized_to_STEN)
