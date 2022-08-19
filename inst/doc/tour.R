## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----SLCS_overview------------------------------------------------------------
library(stenR)
str(SLCS)

## ----preprocessing-ScaleSpec--------------------------------------------------
# create ScaleSpec objects for sub-scales
SL_spec <- ScaleSpec(
  name = "Self-Liking",
  item_names = c("SLCS_1", "SLCS_3", "SLCS_5", "SLCS_6", "SLCS_7", 
                 "SLCS_9", "SLCS_11", "SLCS_15"),
  min = 1,
  max = 5,
  reverse = c("SLCS_1", "SLCS_6", "SLCS_7", "SLCS_15")
)

SC_spec <- ScaleSpec(
  name = "Self-Competence",
  item_names = c("SLCS_2", "SLCS_4", "SLCS_8", "SLCS_10", "SLCS_12",
                 "SLCS_13", "SLCS_14", "SLCS_16"),
  min = 1,
  max = 5,
  reverse = c("SLCS_8", "SLCS_10", "SLCS_13")
)

# create CombScaleSpec object for general scale using single-scale 
# specification
GS_spec <- CombScaleSpec(
  name = "General Score",
  SL_spec,
  SC_spec
)

print(SL_spec)
print(SC_spec)
print(GS_spec)

## ----preprocessing-sum_items_to_scale-----------------------------------------
summed_data <- sum_items_to_scale(
  data = SLCS,
  SL_spec,
  SC_spec,
  GS_spec,
  retain = c("user_id", "sex")
)

str(summed_data)

## ----table_construction-FreqTable---------------------------------------------
# Create the FrequencyTables
SL_ft <- FrequencyTable(summed_data$`Self-Liking`)
SC_ft <- FrequencyTable(summed_data$`Self-Competence`)
GS_ft <- FrequencyTable(summed_data$`General Score`)

## ----table_construction-ScoreTable--------------------------------------------
# Check what is the STEN *StandardScale* definition
print(STEN)

# Calculate the ScoreTables
SL_st <- ScoreTable(SL_ft, STEN)
SC_st <- ScoreTable(SC_ft, STEN)
GS_st <- ScoreTable(GS_ft, STEN)

## ----normalize_scores---------------------------------------------------------
# normalize each of the scores in one call
normalized_at_once <- normalize_scores_df(
  summed_data,
  vars = c("Self-Liking", "Self-Competence", "General Score"),
  SL_st,
  SC_st,
  GS_st,
  what = "sten",
  retain = c("user_id", "sex")
)

str(normalized_at_once)

# or normalize scores individually
SL_sten <- 
  normalize_score(summed_data$`Self-Liking`,
                  table = SL_st,
                  what = "sten")

SC_sten <- 
  normalize_score(summed_data$`Self-Competence`,
                  table = SC_st,
                  what = "sten")

GC_sten <- 
  normalize_score(summed_data$`General Score`,
                  table = GS_st,
                  what = "sten")

# check the structure
str(list(SL_sten, SC_sten, GC_sten))


