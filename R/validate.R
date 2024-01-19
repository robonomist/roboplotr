#' @importFrom rlang as_name enquo eval_tidy quo quo_is_call quo_is_null sym
#' @importFrom stringr str_glue
roboplotr_check_valid_var <- function(var,names,where = NULL) {
  this_var <- as_name(substitute(var))
  wrn <- "The variable '{this_var}'{where}must be a length 1 string or symbol, or a function resulting in length 1 string or symbol that is among the names of argument 'd' of roboplotr::roboplot()."
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


#' @importFrom methods is
#' @importFrom purrr map_lgl
#' @importFrom stringr str_c str_replace str_remove
roboplotr_check_param <- function(var, type, size = 1, allow_null = T, allow_na = F, f.name = NULL, extra = NULL) {

  if(!is.null(extra)) { extra <- str_c(extra, " ")}
  length.ok <- T
  type.ok <- T
  names.ok <- T
  itemnames <- NULL
  allow.null <- if(allow_null == T) { ", or NULL" } else { NULL }

  type <- str_replace(type, "function","OptionalFunction")

  if(allow_null == T & is.null(var)) {
  } else if (allow_na == T & all(is.na(var))) {
  } else {

    if(!is.null(size)) {
      if(is.character(size)) {
        itemnames <- str_c(" named ",roboplotr_combine_words(size))
        if(!all(names(var) %in% size)) {
          names.ok <- F
        }
        size <- length(size)
      }
      length.ok <- length(var) == size
      length <- str_c(" of length ",size)
    } else { length <- ""}

    if(!is.null(f.name) & !is.null(var)) {
      proper.fun <- any(str_detect(as.character(f.name$fun),f.name$check))
      if(proper.fun) {length.ok <- T}
      type.ok <- all(any(map_lgl(type, ~ is(f.name$var, .x))), proper.fun)
      if(!all(type %in% "OptionalFunction")) {
        type.ok <- any(type.ok, any(map_lgl(type, ~ is(var, .x))))
      } else {
      }
    } else {
        if(!"any type" %in% type) {
          type.ok <- any(map_lgl(type, ~ is(var, .x)))
        }
        }

    # print(str_c("length: ", length.ok, " type: ",type.ok))
    if(any(!length.ok,!type.ok,!names.ok) == T) {
      type <- str_replace(type, "OptionalFunction","function")
      err <- substitute(var) |> as.character() |> first()
      final.string <- ifelse(!is.null(f.name), str_c(" call of ",f.name$check,"()"), length)
      type <- ifelse(length(type) == 1, type, str_c(" either ",ifelse(!is.null(f.name), str_glue("{length} "),""),roboplotr_combine_words(type, and = ", or ")))
      stop(paste0(extra,"'",err,"' must be a ",type,final.string,itemnames,allow.null,"."), call. = F)
    }
  }
}

#' @importFrom stringr str_c
roboplotr_valid_strings <- function(strings_to_validate, valid_values, .fun = all, msg = NULL) {
  if(!is.null(strings_to_validate)) {
    if(!.fun(valid_values %in% strings_to_validate)) {
      if(is.null(msg)) {
        msg <- str_glue("'{deparse(substitute(strings_to_validate))}'")
      }
      stop (str_c(msg," must be among ",roboplotr_combine_words(str_replace_all(valid_values,"\\\\", "\\\\\\\\")),"!"), call. = F)
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
