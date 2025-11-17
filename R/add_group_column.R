#' Add Group Column
#'
#' This function adds a new column to a data frame with a specified name. The new column indicates groups based on the grouping variables provided. When no grouping variable is provided, a default value is assigned to the new column.
#'
#' @param data A data frame.
#' @param name The name of the new column to be added.
#' @param group_by A vector of strings specifying the column names used for grouping. Default is NULL.
#'
#' @return A data frame with the new column added.
#' @examples
#' \dontrun{
#' data <- data.frame(x = c(1,2,3,1,2,3), y = c("a","b","c","a","b","c"))
#' data %>% add_group_column(name = "group_id", group_by = c("x", "y"))
#' }
#'
#' @export
#'
add_group_column <- function(data, name, group_by = NULL) {
  if (is.null(group_by)) {
    data[[name]] <- as.factor("-")
    return(data)
  } else {
    data <- data %>%
      group_by(across(all_of(group_by))) %>%
      mutate(temp____col = paste0(name, stringr::str_pad(cur_group_id(), width = 2, pad = "0")) %>% as.factor()) %>%
      ungroup()

    names(data)[which(names(data) == "temp____col")] <- name
    return(data)
  }
}
