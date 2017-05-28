#' Loads a CSV file
#'
#' @description
#' Loads as argument a CSV file \code{filename} and returns
#' a tibble. provides an error message and ends in case of incorrect path definition.
#'
#' @param filename Path to the CSV file (character)
#'
#' @return Returns a tibble (data.frame) of the CSV file.
#'
#' @examples
#' \dontrun{
#' accident_2015 <- fars_read("./data/accident_2015.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Creates a filename
#'
#' @description
#' Creates a filename for a .csv.bz2 file for the \code{year}
#' argument with the structue "accident_<year>.csv.bz2". Requires an
#' integer as input or ends with an error message, otherwise.
#'
#' @param year Integer: year of observation.
#'
#' @return Returns a character string as "accident_<year>.csv.bz2"
#' to be used as a filename
#'
#' @examples
#' \dontrun{
#' makefilename(2015)
#' }
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reads month and year from accident files
#'
#' @description
#' Accepts a vector or list of years and returns a list of data
#' frames with month and year columns of "accident_<year>.csv.bz2
#' files.
#'
#' @param years A vector or list of years in integer format.
#'
#' @return Returns a list of tibbles (data frames) with the same number of rows
#' as the data in "accident_<year>.csv.bz2" files and two columns: month and
#' year. Returns NULL and a warning if the file does not exist.
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' fars_read_years(list(2013, 2014))
#'
#' # Results in a warning
#' fars_read_years(2016)
#' }
#'
#' @importFrom dplyr %>% mutate select
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat, year = ~year) %>%
        dplyr::select_("MONTH", "year")
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Counts number of accidents per month and year
#'
#' Calculates the number of accidents based on the list of years by months.
#' Accepts years as a list or a vector.
#'
#' @param years Looks for a vector or list of years (numeric or integer) in the data.
#'
#'
#' @return Returns a pivot tibble (data frame) with months in rows and selected
#' years in columns, containing the number of accidents. Returns a warning for
#' every year that does not exist in the datasets. Returns an error message in case of
#' non-numeric or non-integer input.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2016)
#' }
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_("year", "MONTH") %>%
    dplyr::summarize_(n = ~n()) %>%
    tidyr::spread_("year", "n")
}

#' Plots accidents on a US state map
#'
#' Accepts a state index and year as input (integer or numerical). Plots accidents on a map.
#' Returns error message and terminates if no match in the FARS data or no corresponding data file is found.
#'
#'
#' @param state.num The FARS state index (numeric or integer)
#'
#' @param year The year (numeric or integer)
#'
#' @return Returns a plot of the accidents of \code{state.num} and
#' \code{year}. Returns an error message if no match for state or year is found in the data.
#'
#' @examples
#' \dontrun{
#' fars_map_state(45, 2015)
#'
#' # Results in an error
#' fars_map_state(45, 2016)
#' fars_map_state(60, 2015)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
