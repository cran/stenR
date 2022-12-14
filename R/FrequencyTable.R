#' Create a FrequencyTable
#' 
#' @param data vector of raw scores. Double values are coerced to integer
#' @description 
#' Normalizes the distribution of raw scores. It can be used to construct 
#' [ScoreTable()] with the use of some [StandardScale()] to normalize and
#' standardize the raw discrete scores.
#' 
#' `plot.FrequencyTable` method requires `ggplot2` package to be installed.
#' @return 
#' FrequencyTable object. Consists of:
#' 
#' - table: data.frame with number of observations (`n`), frequency in sample 
#' (`freq`), quantile (`quan`) and normalized Z-score (`Z`) for each point in 
#' raw score 
#' - status: list containing the total number of simulated observations (`n`) 
#' and information about raw scores range completion (`range`): complete or incomplete 
#' @seealso [SimFrequencyTable()]
#' @importFrom cli cli_abort cli_warn
#' @export

FrequencyTable <- function(data) {
  
  if (!is.numeric(data)) 
    cli_abort("Vector of non-numeric values were provided to {.val data}.",
              class = cli_class$error$Type)
  if (!is.integer(data)) {
    cli_warn("Non-integer values were coerced to integers.",
             class = cli_class$warning$Type)
    raw <- as.integer(data)
  }
  
  # calculate statistics for frequency table
  H <- table(data)
  #Hcum <- cumsum(H)
  h <- as.numeric(prop.table(H))
  hcum <- cumsum(h)
  
  # create whole frequency table
  comp <- data.frame(
    score = names(H),
    h = h,
    hcum = hcum
  )
  
  comp[["lag_hcum"]] <- c(0, comp[1:nrow(comp) - 1, "hcum"])
  comp[["props"]] <- comp[["lag_hcum"]] + comp[["h"]]/2
  comp[["Z_val"]] <- stats::qnorm(comp[["props"]])
  
  table <- data.frame(
    n = as.numeric(H),
    score = comp$score,
    freq = as.numeric(comp$h * 100),
    quan = as.numeric(comp$props * 100),
    Z = as.numeric(comp$Z_val)
  )
  
  # check if there are any scores between without values
  first_score <- as.numeric(table$score[1])
  last_score <- as.numeric(table$score[length(table$score)])
  
  # if there are any, there is a need to add missing values
  if(!(length(table$score) == last_score - first_score + 1)){
    
    # generate table with all score values
    complete_table <- data.frame(score = first_score:last_score)
    complete_table$score <- as.character(complete_table$score)
    complete_table <- dplyr::left_join(complete_table, table, by = "score")
    
    # if there is a score with missing values, get them from the row before
    for (row in 1:nrow(complete_table)) {
      if (is.na(complete_table[row, "n"])) {
        complete_table[row, "n"] <- 0
        complete_table[row, "freq"] <- 0
        complete_table[row, c("quan", "Z")] <- complete_table[row - 1, c("quan", "Z")]
      }
    }
    table <- complete_table
    
    #generate warning and update status correctly
    status <- list(range = "incomplete",
                   incompletes = table[which(table$n == 0), "score"],
                   n = sum(table$n))
    FQ_incomplete_message(status$incompletes, nrow(table))
    
  } else {
    status <- list(range = "complete",
                   incompletes = NULL,
                   n = sum(table$n))
  }
  
  table$score <- as.numeric(table$score)
  table <- table[, c("score", "n", "freq", "quan", "Z")]
  
  output <- list(table = table,
                 status = status)
  
  class(output) <- "FrequencyTable"
  
  return(output)
  
}

#' @param x A `FrequencyTable` object
#' @param ... further arguments passed to or from other methods.
#' @importFrom cli cli_inform
#' @rdname FrequencyTable
#' @export
print.FrequencyTable <- function(x, ...) {
  
  cli_text("{.cls FrequencyTable} computed on {.val {x$status$n}} observations")
  if (is.Simulated(x))
    cli_text("Distribution is {.strong Simulated}")
  else
    cli_text("computed on {.val {x$status$n}} observations")
  
  cli_text("range: {.emph {x$status$range}}")
  
  invisible(x)
}

#' @param x A `FrequencyTable` object
#' @param ... further arguments passed to or from other methods.
#' @rdname FrequencyTable
#' @export
plot.FrequencyTable <- function(x, ...) {
  
  rlang::check_installed("ggplot2")
  
  sds <- factor(ifelse(x$table$Z < -2 | x$table$Z > 2, ">2SD",
                ifelse(x$table$Z < -1 | x$table$Z > 1, "1SD-2SD", "<1SD")),
                levels = c("<1SD", "1SD-2SD", ">2SD"))
  
  i <- which(abs(x$table$Z) == min(abs(x$table$Z)))
  
  Z_label <- paste("Z =", round(x$table$Z[i], 2))
  
  ggplot2::ggplot(data = x$table, ggplot2::aes(x = score, y = n)) + 
    ggplot2::geom_col(ggplot2::aes(fill = sds), color = "black", alpha = 0.3) +
    ggplot2::scale_fill_manual("Normalized\ndistribution",
                                 values = c("green", "blue", "red")) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = x$table$score[i],
                                     color = Z_label), size = 0.5) +
    ggplot2::scale_color_manual("Closest to\ncenter", values = "#000000") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(name = "Number of observations")

}

#' @rdname FrequencyTable
#' @param object A `FrequencyTable` object
#' @param ... further arguments passed to or from other methods.
#' @return `data.frame` of descriptive statistcs
#' @export
summary.FrequencyTable <- function(object, ...) {
  
  whole_vec <- rep(object$table$score, object$table$n)
  
  summaries <- data.frame(
    n = length(whole_vec),
    min = min(whole_vec),
    max = max(whole_vec),
    mean = mean(whole_vec),
    median = stats::median(whole_vec),
    sd = stats::sd(whole_vec),
    skewness = moments::skewness(whole_vec),
    kurtosis = moments::kurtosis(whole_vec),
    incomplete = length(object$status$incompletes))
  
  return(summaries)
  
}

#' Generate FrequencyTable using simulated distribution
#' 
#' @description It is always best to use raw scores for computing the `FrequencyTable`.
#' They aren't always available - in that case, this function can be used
#' to simulate the distribution given its descriptive statistics.
#' 
#' This simulation should be always treated as an estimate.
#' 
#' The distribution is generated using the **Fleishmann** method from
#' [SimMultiCorrData::nonnormvar1()] function. The 
#' `SimMultiCorrData` package needs to be installed.
#' 
#' @param min minimum value of raw score
#' @param max maximum value of raw score
#' @param M mean of the raw scores distribution 
#' @param SD standard deviation of the raw scores distribution
#' @param skew skewness of the raw scores distribution. Defaults to `0` for 
#' normal distribution
#' @param kurt kurtosis of the raw scores distribution. Defaults to `3` for
#' normal distribution
#' @param n number of observations to simulate. Defaults to `10000`, but greater 
#' values could be used to generate better estimates. Final number of observations
#' in the generated Frequency Table may be less - all values lower than `min` and
#' higher than `max` are filtered out.
#' @param seed the seed value for random number generation
#' @return 
#' FrequencyTable object created with simulated data. Consists of:
#' 
#' - table: data.frame with number of observations (`n`), frequency in sample 
#' (`freq`), quantile (`quan`) and normalized Z-score (`Z`) for each point in 
#' raw score 
#' - status: list containing the total number of simulated observations (`n`) 
#' and information about raw scores range completion (`range`): complete or incomplete 
#' @export
SimFrequencyTable <- function(
  min, max, M, SD, skew = 0, kurt = 3, n = 10000, seed = NULL
) {

  rlang::check_installed("SimMultiCorrData")
  
  if (is.null(seed))
    seed <- as.numeric(paste(round(stats::runif(6, 0, 9), 0), collapse = ""))
  
  suppressMessages({
    simulated <- SimMultiCorrData::nonnormvar1(
      method = "Fleishman",
      means = M,
      vars = SD^2,
      skews = skew,
      skurts = kurt-3,
      n = n,
      seed = seed
    )$continuous_variable$V1
  })
  
  simulated <- as.integer(round(simulated, 0))
  simulated <- simulated[simulated >= min & simulated <= max]
  
  ft <- FrequencyTable(simulated)
  class(ft) <- c(class(ft), "Simulated")
  return(ft)
  
}

#' @title Create GroupedFrequencyTable
#' @description Using [GroupConditions()] object and source `data.frame` compute
#' a set of [FrequencyTable()]s for single variable
#' @param data source `data.frame`
#' @param conditions up to two `GroupConditions` objects. These objects will be 
#' passed along during creation of higher-level objects and used when 
#' [normalize_scores_grouped()] will be called. If two objects are provided,
#' then intersection of groups will be made.
#' @param var name of variable to compute `GroupedFrequencyTable` for
#' @param force_disjoint It is recommended to keep it as default
#' `FALSE`, unless the sample size is very big and it is completely mandatory
#' to have the groups disjointed.
#' @param .all should *.all* or *.all1* and *.all2* groups
#' be generated. If they are not generated, all score normalization
#' procedures will fail if the observation can't be assigned to any of the
#' provided conditions (eg. because of missing data), leaving it's score as `NA`. 
#' Defaults to `TRUE`
#' @details `force_exhaustive` will always be checked as `FALSE` during the
#' calculations. It is mandatory for validity of the created *FrequencyTables*
#' @seealso plot.GroupedFrequencyTable
#' @importFrom cli cli_abort
#' @export

GroupedFrequencyTable <- function(data,
                                  conditions,
                                  var,
                                  force_disjoint = FALSE,
                                  .all = TRUE) {
  
  if (!is.data.frame(data))
    cli_abort("Object of {.cls data.frame} need to be provided to {.var data}",
              class = cli_class$error$Class)
  if (!is.character(var) || length(var) != 1 || !var %in% names(data))
    cli_abort("Name of one variable present in {.val data} needs to be passed to {.val var}.",
              class = cli_class$error$NoValidVars)
  
  if (is.GroupConditions(conditions))
    conditions <- list(conditions)
  if (!all(sapply(conditions, is.GroupConditions)))
    cli_abort("Object of class {.cls GroupConditions} or list of two of them need to be provided to {.var conditions}.",
              class = cli_class$error$Class)
  if (length(conditions) > 2)
    cli_abort("Up to two {.cls GroupConditions} can be provided",
              class = cli_class$error$TooManyConditions)

  if (length(conditions) == 2) {
    
    suppressWarnings(
      indices <- intersect_GroupAssignment(
        GA1 = GroupAssignment(data, conditions = conditions[[1]],
                              force_exhaustive = FALSE,
                              force_disjoint = force_disjoint,
                              .all = isTRUE(.all)),
        GA2 = GroupAssignment(data, conditions = conditions[[2]],
                              force_exhaustive = FALSE,
                              force_disjoint = force_disjoint,
                              .all = isTRUE(.all)),
        force_disjoint = force_disjoint,
        force_exhaustive = FALSE),
      classes = cli_class$warning$NonExhaustive
    )
    
  } else 
    suppressWarnings(
      indices <- GroupAssignment(data = data,
                                 conditions = conditions[[1]],
                                 force_exhaustive = FALSE,
                                 force_disjoint = force_disjoint,
                                 .all = isTRUE(.all)),
      classes = cli_class$warning$NonExhaustive
    )

  
  FTs <- list()
  
  for (group in indices) {
    
    if (length(group$els) > 0)
      suppressMessages({
        FTs[[paste(group$group, collapse = ":")]] <-
          FrequencyTable(data[group$els, var])
      }, classes = cli_class$message$IncompleteRange)
  }
  
  inc_groups <- names(FTs)[sapply(FTs, \(ft) ft$status$range == "incomplete")]
  inc_vals <- lapply(FTs[sapply(FTs, \(ft) ft$status$range == "incomplete")],
                     \(x) x$status$incompletes)
  inc_totals <- sapply(FTs[sapply(FTs, \(ft) ft$status$range == "incomplete")],
                       \(x) nrow(x$table))
  
  if (length(inc_groups) > 0)
    GFQ_incomplete_message(inc_groups,
                           inc_vals,
                           inc_totals)
  
  attr(FTs, "all") <- isTRUE(.all)
  attr(FTs, "conditions") <- conditions
  
  class(FTs) <- c("GroupedFrequencyTable", if (length(conditions) == 2) "Intersect")
  
  return(FTs)
  
}

#' @title Gerenic plot of the GroupedFrequencyTable
#' @description Generic plot using `ggplot2`. It plots FrequencyTables for all 
#' groups by default, or only chosen ones using when `group_names` argument is specified. 
#' @param x A `GroupedFrequencyTable` object
#' @param group_names vector specifying which groups should appear in the plots
#' @param strict_names If `TRUE`, then intersected groups are filtered
#' using *strict* strategy: `group_names` need to be provided in form: `"group1:group2"`. If
#' `FALSE`, then intersected groups will be taken into regard separately, so 
#' eg. when `"group1"` is provided to `group_names`, all of: `"group1:group2"`, 
#' `"group1:group3"`, `"group1:groupN"`  will be plotted. Defaults to `TRUE`
#' @param plot_grid boolean indicating if the [ggplot2::facet_grid()] should be used.
#' If `FALSE`, then [ggplot2::facet_wrap()] is used. If groups are not intersected,
#' then it will be ignored and `facet_wrap` will be used.
#' @param ... named list of additional arguments passed to `facet` function
#' used. 
#' @importFrom cli cli_abort
#' @export
plot.GroupedFrequencyTable <- function(
    x, 
    group_names = NULL,
    strict_names = TRUE,
    plot_grid = is.intersected(x),
    ...
) {
  
  rlang::check_installed("ggplot2")
  
  if (!is.null(group_names)) {
    if (isTRUE(strict_names)){
      if (!any(group_names %in% names(x)))
        cli_abort("Not all names specified in {.var group_names} point to actual groups.",
                  class = cli_class$error$WrongGroup)
   } else {
      all_names <- unique(unlist(strsplit(names(x), split = ":")))
      if (!any(group_names %in% all_names))
        cli_abort("Not all names specified in {.var group_names} point to actual groups.",
                  class = cli_class$error$WrongGroup)
    }
  }
    
  plot_data <- lapply(seq_along(x), \(i) {
    
    if (!is.null(group_names)) {
      if (isTRUE(strict_names)) {
        name_check <- names(x)[i]
        if (!name_check %in% group_names)
          return(NULL)
      } else {
        name_check <- strsplit(names(x)[i], split = ":")[[1]]
        if (!any(name_check %in% group_names))
          return(NULL)
      } 
    }
    
    name <- strsplit(names(x)[i], split = ":")[[1]]
    
    x[[i]]$table$sds <- 
      factor(ifelse(x[[i]]$table$Z < -2 | x[[i]]$table$Z > 2, ">2SD",
                    ifelse(x[[i]]$table$Z < -1 | x[[i]]$table$Z > 1, "1SD-2SD", "<1SD")),
             levels = c("<1SD", "1SD-2SD", ">2SD"))
    
    if (length(name) == 1) {
      x[[i]]$table$group1 <- name
    } else if (length(name) == 2) {
      x[[i]]$table$group1 <- name[1]
      x[[i]]$table$group2 <- name[2]
    }
    
    return(x[[i]]$table)
    
  })
  
  plot_data <- data.table::rbindlist(plot_data)
  
  if (is.intersected(x)) {
    
    plot_data$group1 <- factor(plot_data$group1, 
                               levels = c(if (attr(x, "all")) ".all1", attr(attr(x, "conditions")[[1]], "groups")))
    plot_data$group2 <- factor(plot_data$group2, 
                               levels = c(if (attr(x, "all")) ".all2", attr(attr(x, "conditions")[[2]], "groups")))
    
  }
  
  if (isTRUE(plot_grid) && is.intersected(x)) {
    
    grp1_row <- length(unique(plot_data$group2)) < length(unique(plot_data$group1))
    
    plot <- 
      ggplot2::ggplot(data = plot_data, ggplot2::aes(x = score, y = n)) + 
      ggplot2::geom_col(ggplot2::aes(fill = sds), alpha = 0.3) +
      ggplot2::scale_fill_manual("Normalized\ndistribution",
                                 values = c("green", "blue", "red")) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = "Number of observations") 
    
    plot_args <- list(
      rows = if (grp1_row) ggplot2::vars(group1) else ggplot2::vars(group2),
      cols = if (grp1_row) ggplot2::vars(group2) else ggplot2::vars(group1)
    )
    
    add_args <- list(...)
    
    plot <- plot +
      do.call(ggplot2::facet_grid,
              args = c(plot_args[!names(plot_args) %in% names(add_args)],
                       add_args))
    
    return(plot)
    
  } else {

    plot <- 
      ggplot2::ggplot(data = plot_data, ggplot2::aes(x = score, y = n)) + 
      ggplot2::geom_col(ggplot2::aes(fill = sds), alpha = 0.3) +
      ggplot2::scale_fill_manual("Normalized\ndistribution",
                                 values = c("green", "blue", "red")) +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(name = "Number of observations")
    
    if (is.intersected(x))
      plot_args <- list(
        facets = ggplot2::vars(group1, group2)
      )  
    else
      plot_args <- list(
        facets = ggplot2::vars(group1)
      )  
    
    add_args <- list(...)
    
    plot <- plot +
      do.call(ggplot2::facet_wrap, args = c(plot_args[!names(plot_args) %in% names(add_args)],
                                            add_args))
      
    return(plot)
    
  }
}

#' @param x A `GroupedFrequencyTable` object
#' @param ... further arguments passed to or from other methods.
#' @rdname GroupedFrequencyTable
#' @importFrom cli cli_text cli_ol cli_ul cli_li cli_end
#' @export

print.GroupedFrequencyTable <- function(x, ...) {
  
    cli_text("{.cls GroupedFrequencyTable}")
    cli_text("{.field No. groups}: {.val {length(x)}}")
    cli_text("{.field GroupConditions}: {.val {length(attr(x, 'conditions'))}}")
    ol <- cli_ol()
    for (cond in attr(x, "conditions")) {
      cli_li("{.strong Category}: {attr(cond, 'cond_category')}")
      ul <- cli_ul()
      cli_li("{.field Tested vars}: {.val {attr(cond, 'formula_vars')}}")
      cli_li("{.field No. groups:}: {.val {length(attr(cond, 'groups'))}}")
      cli_end(ul)
    }
    cli_end(ol)
    cli_text("{.field {.bold .all} groups} included: {.val {isTRUE(attr(x, 'all'))}}")
    
}

#' @rdname GroupedFrequencyTable
#' @param object A `GroupedFrequencyTable` object
#' @param ... further arguments passed to or from other methods.
#' @importFrom cli cli_inform
#' @return `data.frame` of descriptive statistcs
#' @export
summary.GroupedFrequencyTable <- function(object, ...) {
  
  summary_all <- lapply(object, \(ft) {
    whole_vec <- rep(ft$table$score, ft$table$n)
    
    summaries <- list(n = length(whole_vec),
                      min = min(whole_vec),
                      max = max(whole_vec),
                      mean = mean(whole_vec),
                      median = stats::median(whole_vec),
                      sd = stats::sd(whole_vec),
                      skewness = moments::skewness(whole_vec),
                      kurtosis = moments::kurtosis(whole_vec),
                      incomplete = length(ft$status$incompletes))
    
    return(summaries)
  })
  
  names(summary_all) <- names(object)
  summary_all <- as.data.frame(dplyr::bind_rows(summary_all, .id = "group"))
  
  return(summary_all)
  
}
