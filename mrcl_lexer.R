options(warn = -1)

source("lib/utils.R")
source("lib/json.R")

match_ident <- function(rest) {
  result <- regexpr("^[a-z_][a-z0-9_]*", rest)
  attr(result, "match.length")
}

match_sym <- function(rest) {
  c2 <- str_head(rest, 2)
  if (includes(c2, c("==", "!="))) {
    return(2)
  }

  c1 <- str_head(rest, 1)
  if (includes(c1, c("(", ")", "{", "}", "=", ";", "+", "*", ","))) {
    return(1)
  } else {
    return(0)
  }
}

match_comment <- function(rest) {
  result <- regexpr("^//.*?\n", rest)
  attr(result, "match.length") - 1
}

is_kw <- function(str) {
  includes(
      str,
      c(
        "func", "set", "var", "call_set", "call", "return", "case", "when", "while",
        "_cmt", "_debug"
      )
  )
}

print_token <- function(kind, val, lineno) {
  token <- list(lineno, kind, val)
  json_print(token)
  u_print("\n")
}

# --------------------------------

src <- read_stdin_all()

rest <- src
lineno <- 1

while (nchar(rest) > 0) {
  if (str_head(rest, 1) == " ") {
    rest <- str_tail(rest, 1)
  } else if (str_head(rest, 1) == "\n") {
    rest <- str_tail(rest, 1)
    lineno <- lineno + 1
  } else if (str_head(rest, 1) == "\"") {
    size <- match_str(rest)
    val  <- str_head(rest, size)
    print_token("str", substr(val, 2, nchar(val) - 1), lineno)
    rest <- str_tail(rest, size)
  } else if (str_head(rest, 2) == "//") {
    size <- match_comment(rest)
    rest <- str_tail(rest, size)
  } else if (0 < match_sym(rest)) {
    size <- match_sym(rest)
    val  <- str_head(rest, size)
    rest <- str_tail(rest, size)
    print_token("sym", val, lineno)
  } else if (0 < match_int(rest)) {
    size <- match_int(rest)
    val  <- str_head(rest, size)
    rest <- str_tail(rest, size)
    print_token("int", val, lineno)
  } else if (0 < match_ident(rest)) {
    size <- match_ident(rest)
    val  <- str_head(rest, size)
    rest <- str_tail(rest, size)
    if (is_kw(val)) {
      print_token("kw", val, lineno)
    } else {
      print_token("ident", val, lineno)
    }
  } else {
    stop("unexpected pattern")
  }
}
