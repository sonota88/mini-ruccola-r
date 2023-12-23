u_paste <- function(s1, s2) {
  paste(s1, s2, sep = "")
}

inspect_list <- function(xs) {
  s <- "["
  n <- 1
  while (n <= length(xs)) {
    x <- xs[[n]]
    if (n >= 2) {
      s <- u_paste(s, ", ")
    }
    s <- u_paste(s, inspect(x))
    n <- n + 1
  }
  u_paste(s, "]")
}

inspect <- function(arg) {
  switch(u_typeof(arg)
    ,"numeric"   = toString(arg)
    ,"character" = sprintf("\"%s\"", gsub("\n", "\\\\n", arg))
    ,"list"      = inspect_list(arg)
    ,stop("unsupported")
  )
}

# utils print
u_print <- function(arg) {
  cat(arg, file = stdout())
}

puts <- function(arg) {
  u_print(arg)
  u_print("\n")
}

print_e <- function(arg) {
  cat(arg, file = stderr())
}

puts_e <- function(arg) {
  print_e(arg)
  print_e("\n")
}

p_e <- function(arg) {
  puts_e(inspect(arg))
}

puts_kv_e <- function(k, v) {
  puts_e(sprintf("%s (%s)", k, inspect(v)))
}

p_kv_e <- function(k, v) {
  puts_kv_e(k, inspect(v))
}

read_stdin_all <- function() {
  lines <- readLines(file("stdin"))
  paste(lines, collapse = "\n")
}

u_typeof <- function(arg) {
  if (is.list(arg)) {
    "list"
  } else if (is.character(arg)) {
    "character"
  } else if (is.numeric(arg)) {
    "numeric"
  } else {
    stop("unsupported")
  }
}

str_char_at <- function(str, n) {
  substr(str, n, n)
}

str_head <- function(str, n) {
  substr(str, 1, n)
}
str_tail <- function(str, n) {
  substr(str, n + 1, nchar(str))
}

list_append <- function(xs, x) {
  xs[[length(xs) + 1]] <- x
  xs
}

list_tail <- function(xs, n) {
  tail(xs, length(xs) - n)
}

includes <- function(x, xs) {
  !is.na(match(x, xs))
}

find_index <- function(x, xs) {
  n <- 1
  while (n <= length(xs)) {
    if (xs[[n]] == x) {
      return(n)
    }
    n <- n + 1
  }

  -1
}

match_int <- function(rest) {
  result <- regexpr("^[-0-9]+", rest)
  attr(result, "match.length")
}

match_str <- function(rest) {
  result <- regexpr("^\".*?\"", rest)
  attr(result, "match.length")
}
