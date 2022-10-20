#' @importFrom rlang empty_env get_env quo_is_null quo_name
#' @importFrom stringr str_subset
roboplot_check_valid_var <- function(var = NULL, names = NULL, extra_msg = NULL) {

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
roboplot_check_param <- function(var, type, var.length = 1, allow_null = T, allow_na = F, f.name = NULL) {

  length.ok <- T
  type.ok <- T

  type <- str_replace(type, "function","OptionalFunction")

  if(allow_null == T & is.null(var)) {
  } else if (allow_na == T & all(is.na(var))) {
  } else {

    if(!is.null(var.length)) {
      length.ok <- length(var) == var.length
      length <- str_c(" vector of length ",var.length)
    } else { length <- ""}

    if(!is.null(f.name)) {
      type.ok <- all(any(map_lgl(type, ~ is(f.name$var, .x))), identical(str_remove(as.character(f.name$fun),"^.*\\:{2}"), f.name$check))
      if(!all(type %in% "OptionalFunction")) {
        type.ok <- any(type.ok, any(map_lgl(type, ~ is(var, .x))))
      }
    } else {
        type.ok <- any(map_lgl(type, ~ is(var, .x)))
        }

    # print(str_c("length: ", length.ok, " type: ",type.ok))
    if(any(!length.ok,!type.ok) == T) {
      type <- str_replace(type, "OptionalFunction","function")
      err <- substitute(var) |> as.character() |> first()
      final.string <- ifelse(!is.null(f.name), str_c(" call of ",f.name$check,"()"), length)
      type <- ifelse(length(type) == 1, type, str_c(" either ",str_c(type, collapse = " or ")))
      stop(paste0("'",err,"' must be a ",type,final.string,"."), call. = F)
    }
  }
}


# roboplot_check_param2 <- function(var, type, length, name, allow_null = T, allow_na = T) {
#
#   if(allow_null == T & is.null(var)) {
#   } else if (allow_na == T & any(is.na(var))) {
#   } else {
#     if(missing(length)) {
#       ok <- is.atomic(var) && is(var, type)
#     } else
#     {
#       ok <- is.atomic(var) && is(var, type) && length(val) == length
#       }
#
#     if(ok == F) {
#       err <- ifelse(!missing(name), name, as.character(substitute(var)))
#       stop(paste0("'",err,"' must be a ",type,ifelse(!missing(length),str_c(" vector of length ",length),""),"."), call. = F)
#     }
#   }
# }
#
# roboplot_check_param(1,"fu")
# roboplot_check_param(roboplot_set_caption(text = "Tilastokeskus"),"function")
