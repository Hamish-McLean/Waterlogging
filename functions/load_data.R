#' @title Load Data from Excel File
#'
#' @description
#' This function loads data from an Excel file, processes it,
#' and combines it into a single data.table.
#'
#' @param dir The directory where the excel file is located.
#' @param filename The name of the excel file to load.
#' @param sheets A vector of sheet names to load.
#' @return A single data.table containing the data from all sheets.
#'
#' @import data.table
#' @importFrom readxl read_excel
#' @import dplyr
#'
load_data <- function(dir, filename, sheets = NULL) {

  require(data.table)
  require(readxl)
  require(dplyr)

  # Check if the file exists
  file_path <- file.path(dir, filename)
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }

  # If sheets are not specified, detect all sheets or use the single sheet
  if (is.null(sheets)) {
    sheets <- readxl::excel_sheets(file_path)
    if (length(sheets) == 1) {
      sheets <- sheets[1] # Use the single sheet if only one exists
    } else {
      stop("Multiple sheets detected. Please specify the sheets to load.")
    }
  }

  # Read the sheets into a list of data.tables
  data_list <- setNames(lapply(sheets, function(sheet) {
    setDT(read_excel(file_path, sheet = sheet,
      na = c("", "na", "NA", "N/A", "#N/A", "<NA>", "NaN", "NULL")
    ))
  }), sheets)

  # Convert tables to long format
  data_list <- lapply(names(data_list), function(name) {
    dt <- data_list[[name]]
    if (all(c("A", "B", "C") %in% colnames(dt))) {
      id_cols <- setdiff(colnames(dt), c("A", "B", "C"))
      melt(dt, id.vars = id_cols, variable.name = "inoculation", value.name = name)
    } else {
      dt
    }
  })

  # Add tree ID column by combining pot with inoculation
  data_list <- lapply(data_list, function(dt) {
    if ("inoculation" %in% colnames(dt)) {
      dt[, tree := paste0(pot, inoculation)]
    }
    dt
  })

  # Replace date with year and assessment (A for Jan-July, B for Aug-Dec)
  data_list <- lapply(data_list, function(dt) {
    if ("date" %in% colnames(dt)) {
      dt[, year := as.integer(format(date, "%Y"))]
      dt[, assessment := ifelse(as.integer(format(date, "%m")) <= 7, "A", "B")]
      dt[, date := NULL] # Remove the original date column
    }
    dt
  })

  # Combine the tables into a single data.table
  combined_data <- Reduce(function(x, y) {
    # Find common columns to merge on
    common_cols <- intersect(colnames(x), colnames(y))
    # Merge the data.tables on common columns
    merge(x, y, by = common_cols, all = TRUE)
  }, data_list)

  # Set factor columns
  factors <- c(
    "block", "split-plot", "plot", "pot", "genotype", "tree", "season",
    "treatment", "inoculation", "timepoint", "year", "assessment", "duration"
  )
  # Only use columns that exist
  factors <- intersect(factors, colnames(combined_data))
  combined_data[, (factors) := lapply(.SD, as.factor), .SDcols = factors]

  # Create duration column from treatment if not there already
  if (!"duration" %in% colnames(combined_data)) {
    combined_data[, duration := case_when(
      grepl("0", treatment) ~ 0,
      grepl("1", treatment) ~ 1,
      grepl("2", treatment) ~ 2,
      grepl("3", treatment) ~ 4,
      grepl("4", treatment) ~ 8,
      TRUE ~ NA_real_
    )]
  }

}
