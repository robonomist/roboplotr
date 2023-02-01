#' @importFrom rlang empty_env get_env quo_is_null quo_name
#' @importFrom stringr str_subset
roboplotr_check_valid_var <- function(var = NULL, names = NULL, extra_msg = NULL) {

  this_var <- substitute(var) |> as.character() |> str_subset("enquo", negate = T)

  invalid <- all(identical(get_env(var),empty_env()), !quo_is_null(var))

  if(invalid == T) { stop(str_c("The argument '",this_var,"' must be unquoted."), call. = F) } else {

    if(quo_is_null(var)) { return(NULL) }

    if(!is.null(names)) {
      if (!quo_name(var) %in% names) {
        stop("The argument '",this_var,"' must be a variable name in argument 'd'",ifelse(is.null(extra_msg), "",str_c(" ",extra_msg)),".", call. = F)
      } else {
        var
      }
    } else {
      var
    }
  }
}


#' @importFrom methods is
#' @importFrom purrr map_lgl
#' @importFrom rlang as_string
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
      type.ok <- all(any(map_lgl(type, ~ is(f.name$var, .x))), any(str_detect(as.character(f.name$fun),f.name$check)))
      if(!all(type %in% "OptionalFunction")) {
        type.ok <- any(type.ok, any(map_lgl(type, ~ is(var, .x))))
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
      type <- ifelse(length(type) == 1, type, str_c(" either ",roboplotr_combine_words(type, and = " or ")))
      stop(paste0(extra,"'",err,"' must be a ",type,final.string,itemnames,allow.null,"."), call. = F)
    }
  }
}

#' @importFrom stringr str_c
roboplotr_valid_strings <- function(strings_to_validate, valid_values, .fun = all) {
  if(!is.null(strings_to_validate)) {
    if(!.fun(valid_values %in% strings_to_validate)) {
      stop (str_c("'",deparse(substitute(strings_to_validate)),"' must be among ",roboplotr_combine_words(str_replace_all(valid_values,"\\\\", "\\\\\\\\")),"!"), call. = F)
    }
  }
}

#' @importFrom stringr str_c
roboplotr_valid_colors <- function(colors_to_validate) {
  if(!is.null(colors_to_validate)) {
    if(!all(roboplotr_are_colors(unlist(colors_to_validate)))) {
      stop (str_c("'",substitute(colors_to_validate),"' must be hexadecimal colors or valid css colors!"), call. = F)
    }
  }
}
