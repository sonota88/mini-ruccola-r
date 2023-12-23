source("lib/utils.R")

print_indent <- function(nest) {
  n <- 1
  while (n <= nest) {
    u_print("  ")
    n <- n + 1
  }
}

json_print_element <- function(x, nest, pretty) {
  if (pretty) {
    print_indent(nest)
  }

  switch(u_typeof(x)
    ,"numeric"   = u_print(x)
    ,"character" = u_print(sprintf("\"%s\"", x))
    ,"list"      = json_print_list(x, nest, pretty)
    ,stop("unsupported")
  )
}

json_print_list <- function(xs, nest, pretty) {
  u_print("[")
  if (pretty) {
    u_print("\n")
  }
  n <- 1
  while (n <= length(xs)) {
    x <- xs[[n]]
    json_print_element(x, nest + 1, pretty)
    if (n < length(xs)) {
      u_print(",")
      if (!pretty) {
        u_print(" ")
      }
    }
    if (pretty) {
      u_print("\n")
    }
    n <- n + 1
  }
  if (pretty) {
    print_indent(nest)
  }
  u_print("]")
}

json_print <- function(xs) {
  json_print_list(xs, 0, FALSE)
}

json_pretty_print <- function(xs) {
  json_print_list(xs, 0, TRUE)
}

json_parse_list <- function(json) {
  c1   <- str_head(json, 1)
  rest <- str_tail(json, 1)

  if (c1 != "[") {
    stop("unexpected pattern")
  }

  xs <- list()

  while (nchar(rest) > 0) {
    c1 <- str_head(rest, 1)

    if (c1 == " " || c1 == "," || c1 == "\n") {
      rest <- str_tail(rest, 1)
    } else if (c1 == "[") {
      retval <- json_parse_list(rest)
      rest       <- retval[[1]]
      inner_list <- retval[[2]]
      xs <- list_append(xs, inner_list)
    } else if (c1 == "]") {
      rest <- str_tail(rest, 1)
      return(list(rest, xs))
    } else if (c1 == "\"") {
      size <- match_str(rest)
      val  <- str_head(rest, size)
      rest <- str_tail(rest, size)
      xs <- list_append(xs, substr(val, 2, nchar(val) - 1))
    } else if (0 < match_int(rest)) {
      size <- match_int(rest)
      val  <- str_head(rest, size)
      rest <- str_tail(rest, size)
      xs <- list_append(xs, as.numeric(val))
    } else {
      stop("unexpected pattern")
    }
  }
}

json_parse <- function(json) {
  retval <- json_parse_list(json)
  retval[[2]]
}
