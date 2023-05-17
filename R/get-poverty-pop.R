#' Returns a merged dataframe with poverty and population data by state for specified age range.
#'
#' This function retrieves poverty data and population data grouped by age from the Census API, filters the
#' data and processes it to return a merged dataframe with relevant information.
#'
#' @param key A character string containing valid value of user's Census API key.
#' @param vars_pov A character vector that specifies the variables to be retrieved for poverty data.
#' @param vars_pop A character vector that specifies the variables to be retrieved for population data.
#' @param filt_age An integer vector of length two specifying the minimum and maximum age group to filter population data.
#' @return A merged dataframe with poverty and population count for each state.
#'
#' @import tidycensus
#' @import dplyr
#' @importFrom utils globalVariables
#' @export
#' @examples
#' data <- getPovertyPop("my_census_key", c("SAEMHI_PT"), c("POP"), c(12, 18))
getPovertyPop = function(key, vars_pov, vars_pop, filt_age) {

  # Get poverty data
  saipe = getCensus(name = "timeseries/poverty/saipe",
                    key = key,
                    vars = vars_pov,
                    region = "state:*",
                    time = "2020")

  saipe = saipe %>%
    mutate(location = state) %>%
    select(-c(state, time))

  # Get population by age group
  n_pop = getCensus(name = "pep/charagegroups",
                    key = key,
                    vars = vars_pop,
                    vintage = "2019",
                    region = "state:*")

  n_pop = n_pop %>%
    mutate(AGEGROUP = as.integer(AGEGROUP),
           location = state) %>%
    filter(AGEGROUP >= filt_age[1] & AGEGROUP <= filt_age[2]) %>% #ages 12-18
    group_by(location) %>%
    summarise(age_pop = sum(POP))

  # Merge dataframes
  result = merge(saipe, n_pop)

  return(result)
}
