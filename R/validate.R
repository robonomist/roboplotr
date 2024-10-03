#' Print method for roboplotr objects
#'
#' This is the print method for objects of class \code{roboplotr}. It temporarily
#' removes the \code{roboplotr} class attribute and related classes before calling
#' the next method in the method chain, ensuring that the default print method is
#' used.
#'
#' @param x An object of class \code{roboplotr}. This object can be any type that
#' also includes the class \code{roboplotr}.
#' @param ... Additional arguments passed to the \code{print} method.
#' @returns The function does not return a value. It is used for its side effect
#' of printing.
#' @export
#' @noRd
print.roboplotr <- function(x, ...) {
  old_class <- class(x)
  class(x) <- subset(old_class, !str_detect(old_class, "^roboplotr"))
  NextMethod("print")
  class(x) <- old_class
}


#' @importFrom rlang as_name enquo eval_tidy quo quo_is_call quo_is_null sym
#' @importFrom stringr str_glue
roboplotr_check_valid_var <- function(var,names,where = NULL) {
  this_var <- as_name(substitute(var))
  wrn <- "The variable '{this_var}'{where}must be a length 1 string or symbol found among the names of argument `d` of `roboplot()`."
  if(quo_is_null(var) == T) {
    return(NULL)
  }

  if(quo_is_call(var)) {
    var <- var |> eval_tidy()
    if(length(var) > 1) {
      stop(str_glue(wrn), call. = F)
    }
    var <- enquo(var)
  }

  var <- quo(!!sym(as_name(var)))

  if(length(as_name(var)) > 1 | !as_name(var) %in% names) {
    if (!is.null(where)) {
      where <- str_glue(" in {where}() ")
    } else {
      where <- ""
      }
    stop(str_glue(wrn), call. = F)
  }

  var

}

#' @importFrom methods as
#' @importFrom purrr map2_chr
#' @importFrom stringr str_c str_glue str_remove
roboplotr_typecheck <- function(var, types, size = 1, allow_null = TRUE, allow_na = FALSE, extra = NULL) {

  get_type <- function(x, y) {
    if(length(x) != size) return(F)
    if (any(is.na(x))) return(ifelse(allow_na, T, F))
    class(x) == y
  }

  if(is.null(extra)) {
    extra <- ""
  } else {
    extra <- str_glue(" {extra}")
  }
  .what <- first(as.character(substitute(var)))

  if (is.null(var)) {
    if (!allow_null) { stop(str_glue("{.what}{extra} cannot be NULL."), call. = F)
    } else {
        return()
      }
  }

  if (!is.list(var) & any(is.na(var))) {
    if (!allow_na) {
      stop(str_glue("{.what}{extra} cannot be NA."), call. = F)
    }
  }

  type_valid <- ifelse(is.null(types), TRUE, FALSE)

  if(!is.null(types)) {
    types <- str_replace(types,"^(?=(set|create))","roboplotr.")
    for (.type in types) {
      if (!any(inherits(var, .type, which = T) == 0)) {
        type_valid <- TRUE
        break
      } else {
        try({
          .var <- as(var, .type)
          if (get_type(.var, .type)) {
            type_valid <- TRUE
            break
          }
        }, silent = TRUE)
      }
    }
  }

  if (!type_valid) {
    .types <- str_replace(types, "roboplotr.","call of roboplotr::")
    .many <- ifelse(length(.types) > 1, 'one of ','')
    stop(str_glue("{.what}{extra} must be {.many}{roboplotr_combine_words(.types, and = ' or ')}!"), call. = F)
  }

  if (!is.null(size) && length(var) != size && !is.list(var)) {
    stop(str_glue("{.what}{extra} must have length of {size}!"), call. = F)
  }

}


#' @importFrom stringr str_glue
roboplotr_valid_strings <- function(strings_to_validate, valid_values, .fun = all, msg = NULL) {
  if(!is.null(strings_to_validate)) {
    if(!.fun(valid_values %in% strings_to_validate)) {
      if(is.null(msg)) {
        msg <- str_glue("'{deparse(substitute(strings_to_validate))}'")
      }
      .valids <- roboplotr_combine_words(str_replace_all(str_c("'",valid_values,"'"),'\\\\', '\\\\\\\\'))
      stop (str_glue("{msg} must be among {.valids}!"), call. = F)
    }
  }
}

#' @importFrom stringr str_glue
roboplotr_valid_colors <- function(colors_to_validate, message = NULL) {
  if(!is.null(colors_to_validate)) {
    if(!all(roboplotr_are_colors(unlist(colors_to_validate)))) {
      if(is.null(message)) {
        message <- str_glue("\'{deparse(substitute(colors_to_validate))}\'")
      }
      stop (str_glue("{message} must be hexadecimal colors or valid css colors!"), call. = F)
    }
  }
}

roboplotr_is_between <- function(check, where, lims = c(0,1)) {
  what <- as_name(substitute(check))
  if(!between(check, min(lims), max(lims))) {
    stop(str_glue("{where} param '{what}' must be between {min(lims)} and max(lims)!"), call. = F)
  }
}