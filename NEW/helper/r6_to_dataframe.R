#' @author Heinrich Guilhem, \email{guilhem.heinrich@gmail.fr}




#' Extract the given names field from the environment as a vector.
#' 
#' @param environment An environment
#' @param ... A list of name
#' @return The vector build upon the given environment's name subset.
extract_from_environment <- function(environment, ...) {
  all_keys <- c(...)
  return(unlist(mget(all_keys, envir = environment)))
}

#' Extract the given name from all the environment in the list, and return it as a data.frame. If no names are given, extract all the primitive bounded variable
#' 
#' @param environmentList An environment list
#' @param ... A list of name
#' @return The dataset build upon the given environment's name subset. If omitted, this subset is build by all names referencing a primitive datatype

EnvironmentList_to_dataframe <- function(environmentList, ...) {
  all_keys <- c(...)
  if (length(all_keys) == 0) {
    all_keys <- get_attributes(environmentList[[1]])
  } 
  result <- sapply(environmentList, (function (env) extract_from_environment(env, unlist(all_keys))), simplify="array")
  return(data.frame(t(result)))
}

#' Return the primitive attributes of an environment
#' 
#' @detail The considered primitive types are
#'    "logical" | "character" | "numeric" | "complex" | "integer"
#'
#' @param environment An environment
#' @return The name's subset of the environment primitive variable's type 
get_attributes <- function(environment) {
  all_properties <- ls(envir = environment)
  all_types <- sapply(all_properties, (function (prop) typeof(get(prop, envir = environment))))
  all_attributes <- all_properties[all_types=="logical" | all_types=="character" | all_types=="numeric" | all_types=="complex" | all_types=="integer" ]
  return(all_attributes)
}


