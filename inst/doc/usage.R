## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4,
  fig.align = "center"
)

## ----library_load-------------------------------------------------------------
library(stenR)

## ----SLCS_structure-----------------------------------------------------------
str(SLCS)

## ----SLCS_ScaleSpecs----------------------------------------------------------
SL_spec <- ScaleSpec(
  name = "SelfLiking", min = 1, max = 5,
  item_names = c("SLCS_1", "SLCS_3", "SLCS_5", "SLCS_6", "SLCS_7", 
                 "SLCS_9", "SLCS_11", "SLCS_15"),
  reverse = c("SLCS_1", "SLCS_6", "SLCS_7", "SLCS_15")
)
SC_spec <- ScaleSpec(
  name = "SelfCompetence", min = 1, max = 5,
  item_names = c("SLCS_2", "SLCS_4", "SLCS_8", "SLCS_10", "SLCS_12",
                 "SLCS_13", "SLCS_14", "SLCS_16"),
  reverse = c("SLCS_8", "SLCS_10", "SLCS_13")
)

## ----SLCS_CombScaleSpec-------------------------------------------------------
GS_spec <- CombScaleSpec(
  name = "GeneralScore",
  SL_spec, SC_spec
)

# subscales can be also reversed
GS_with_rev <- CombScaleSpec(
  name = "rev_example",
  SL_spec, SC_spec,
  reverse = "SelfCompetence"
)

## ----SLCS_summarize-----------------------------------------------------------
summed_SCLS <- sum_items_to_scale(
  SLCS,
  SL_spec,
  SC_spec,
  GS_spec,
  GS_with_rev
)

str(summed_SCLS)

## ----HEXACO_str---------------------------------------------------------------
str(HEXACO_60)

## ----ungrouped_freqtable------------------------------------------------------
HEX_C_ft <- FrequencyTable(HEXACO_60$HEX_C)
HEX_E_ft <- FrequencyTable(HEXACO_60$HEX_E)

## ----plot_freqtable-----------------------------------------------------------
plot(HEX_E_ft)

## ----ungrouped_scoretables----------------------------------------------------
HEX_C_st <- ScoreTable(
  ft = HEX_C_ft,
  scale = STEN
)
HEX_E_st <-ScoreTable(
  ft = HEX_E_ft,
  scale = STEN
)

## ----ungrouped_normalization_base---------------------------------------------
HEX_C_norm <- normalize_score(
  HEXACO_60$HEX_C,
  table = HEX_C_st,
  what = "sten"
)
HEX_E_norm <- normalize_score(
  HEXACO_60$HEX_E,
  table = HEX_E_st,
  what = "sten"
)
summary(HEX_C_norm)
summary(HEX_E_norm)

## ----ungrouped_normalization_df-----------------------------------------------
HEX_CE_norm <- normalize_scores_df(
  data = HEXACO_60,
  vars = c("HEX_C", "HEX_E"),
  HEX_C_st,
  HEX_E_st,
  what = "sten",
  # by default no other variables will be retained
  retain = FALSE
)
summary(HEX_CE_norm)
str(HEX_CE_norm)

## ----ScoringTable_export, echo=FALSE------------------------------------------
C_ScoringTable <- tempfile(fileext = ".csv")
E_ScoringTable <- tempfile(fileext = ".csv")

export_ScoringTable(
  to_ScoringTable(HEX_C_st,
                  min_raw = 10,
                  max_raw = 50),
  C_ScoringTable,
  "csv",
  
)

export_ScoringTable(
  to_ScoringTable(HEX_E_st,
                  min_raw = 10,
                  max_raw = 50),
  E_ScoringTable,
  "csv"
)

## ----Scoring_ungrouped_import-------------------------------------------------
HEX_C_Scoring <- import_ScoringTable(
  source = C_ScoringTable,
  method = "csv"
)
HEX_E_Scoring <- import_ScoringTable(
  source = E_ScoringTable,
  method = "csv"
)
summary(HEX_C_Scoring)
summary(HEX_E_Scoring)

## ----normalize_Scoring_ungrouped----------------------------------------------
HEX_CE_norm <- normalize_scores_scoring(
  data = HEXACO_60,
  vars = c("HEX_C", "HEX_E"),
  HEX_C_Scoring,
  HEX_E_Scoring
)
summary(HEX_CE_norm)
str(HEX_CE_norm)

## ----GroupConditions----------------------------------------------------------
sex_grouping <- GroupConditions(
  conditions_category = "Sex",
  "M" ~ sex == "M",
  "F" ~ sex == "F"
)
age_grouping <- GroupConditions(
  conditions_category = "Age",
  "to 30" ~ age < 30,
  "above 30" ~ age >= 31
)
sex_grouping
age_grouping

## ----IPIP_NEO_str-------------------------------------------------------------
str(IPIP_NEO_300)

## ----ungrouped_tables, fig.align = "center"-----------------------------------
N_gft <- GroupedFrequencyTable(
  data = IPIP_NEO_300,
  conditions = list(age_grouping, sex_grouping),
  var = "N",
  # By default, norms are are also computed for '.all' groups. These are
  # used if by any reason observation can't be assigned to any group
  # in corresponding condition category
  .all = TRUE
)
N_gst <- GroupedScoreTable(N_gft, scale = STEN)
plot(N_gst)

## ----grouped_table_normalize--------------------------------------------------
NEO_norm <- normalize_scores_grouped(
  data = IPIP_NEO_300,
  vars = "N",
  N_gst,
  what = "sten",
  group_col = "Group"
)
str(NEO_norm)
table(NEO_norm$Group)

## ----grouped_scoring_export---------------------------------------------------
ST_csv <- tempfile(fileext = ".csv")
cond_csv <- tempfile(fileext = ".csv")

N_ST <- to_ScoringTable(
  table = N_gst,
  min_raw = 60,
  max_raw = 300
)

summary(N_ST)

export_ScoringTable(
  table = N_ST,
  out_file = ST_csv,
  method = "csv",
  # you can also export GroupConditions to seperate csv file
  cond_file = cond_csv
)

## ----import_grouped_scoring---------------------------------------------------
imported_ST <- import_ScoringTable(
  source = ST_csv,
  method = "csv",
  conditions = list(age_grouping, sex_grouping)
)

summary(imported_ST)

## ----normalize_imported_scoring-----------------------------------------------
NEO_norm <- normalize_scores_scoring(
  data = IPIP_NEO_300,
  vars = "N",
  imported_ST,
  group_col = "Group"
)
str(NEO_norm)
table(NEO_norm$Group)

## ----StandardScale------------------------------------------------------------
new_scale <- StandardScale("my_scale", 10, 3, 0, 20)

# let's see if everything is correct
new_scale

# how does its distribution looks like?
plot(new_scale)

## ----init_CompScoreTable------------------------------------------------------
# attach during initialization
HexCST <- CompScoreTable$new(
  tables = list(HEX_E = HEX_E_ft),
  scales = STEN
)

# attach later
altCST <- CompScoreTable$new()
altCST$attach_FrequencyTable(HEX_E_ft, "HEX_E")
altCST$attach_StandardScale(STEN)

# there are no visible differences in objects structure
summary(HexCST)
summary(altCST)

## ----expand_CST---------------------------------------------------------------
# add new FrequencyTable
HexCST$attach_FrequencyTable(FrequencyTable(HEXACO_60$HEX_C), "HEX_C")
summary(HexCST)

# add new StandardScale
HexCST$attach_StandardScale(STANINE)
summary(HexCST)

## ----CST_standardize----------------------------------------------------------
# standardize the Honesty-Humility and Consciousness
HexCST$standardize(
  data = head(HEXACO_60),
  what = "sten",
  vars = c("HEX_E", "HEX_C")
)

# you can also do this easily with pipes!
HEXACO_60[1:5, c("HEX_E", "HEX_C")] |>
  # no need to specify 'vars', as the correct columns are already selected
  HexCST$standardize("sten")

## ----CST_append---------------------------------------------------------------
# check the current state of the object
summary(HexCST)

# now, standardize and recalculate!
HEXACO_60[1:5, c("HEX_H", "HEX_C")] |>
  HexCST$standardize("sten", calc = TRUE)

# check the new state
summary(HexCST)

## ----CST_export---------------------------------------------------------------
# export as ScoreTables
st_list <- HexCST$export_ScoreTable()
summary(st_list)

# export as FrequencyTables
ft_list <- HexCST$export_ScoreTable(strip = T)
summary(ft_list)

## ----SimTables----------------------------------------------------------------
sim_ft <- SimFrequencyTable(min = 10, max = 50, M = 31.04, 
                            SD = 6.7, skew = -0.3, kurt = 2.89, seed = 2678)

class(sim_ft)

plot(sim_ft)

## ----check_SimComp, error=TRUE------------------------------------------------
SimCST <- CompScoreTable$new(
  tables = list("simmed" = sim_ft),
  scales = STEN
)

SimCST$standardize(
  data = data.frame(simmed = round(runif(10, 10, 50), 0)),
  what = "sten",
  calc = TRUE)


