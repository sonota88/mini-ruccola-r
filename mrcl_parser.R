options(warn = -1)

source("lib/utils.R")
source("lib/json.R")

g_tokens <- list()
g_pos <- 1

is_end <- function() {
  g_pos > length(g_tokens)
}

# --------------------------------

read_tokens <- function() {
  lines <- readLines(file("stdin"))
  tokens <- list()

  for (n in 1:length(lines)) {
    line <- lines[[n]]
    if (str_head(line, 1) == "[") {
      xs <- json_parse(line)
      t <- list("lieno" = xs[[1]], "kind" = xs[[2]], "val" = xs[[3]])
      tokens <- list_append(tokens, t)
    }
  }

  return(tokens)
}

peek <- function(offset = 0) {
  g_tokens[[g_pos + offset]]
}

bump <- function() {
  g_pos <<- g_pos + 1
}

peek_bump <- function() {
  t <- peek()
  bump()
  t
}

consume <- function(val) {
  if (token_val(peek()) == val) {
    bump()
  } else {
    msg <- sprintf(
      "unexpected token / exp (%s) / act (%s)",
      val,
      token_val(peek())
    )
    stop(msg)
  }
}

# --------------------------------

token_kind <- function(t) {
  t[["kind"]]
}

token_val <- function(t) {
  t[["val"]]
}

# --------------------------------

parse_arg <- function() {
  switch(token_kind(peek())
    ,"ident" = token_val(peek_bump())
    ,"int"   = as.numeric(token_val(peek_bump()))
    ,stop("unsupported")
  )
}

parse_args <- function() {
  args <- list()

  if (token_val(peek()) == ")") {
    return(args)
  }

  args <- list_append(args, parse_arg())

  while (token_val(peek()) == ",") {
    consume(",")
    args <- list_append(args, parse_arg())
  }

  args
}

parse_expr_factor <- function() {
  switch(token_kind(peek())
    ,"int"   = as.numeric(token_val(peek_bump()))
    ,"ident" = token_val(peek_bump())
    ,"sym"   = {
      consume("(")
      expr <- parse_expr()
      consume(")")
      expr
    }
    ,stop("unsupported")
  )
}

is_binop <- function(t) {
  val <- token_val(t)
  includes(val, c("+", "*", "==", "!="))
}

parse_expr <- function() {
  expr <- parse_expr_factor()

  while (is_binop(peek())) {
    op   <- token_val(peek_bump())
    rhs  <- parse_expr_factor()
    expr <- list(op, expr, rhs)
  }

  expr
}

parse_set <- function() {
  consume("set")
  var_name <- token_val(peek_bump())
  consume("=")
  expr <- parse_expr()
  consume(";")

  list("set", var_name, expr)
}

parse_funcall <- function() {
  fn_name <- token_val(peek_bump())
  consume("(")
  args <- parse_args()
  consume(")")

  append(list(fn_name), args)
}

parse_call <- function() {
  consume("call")
  funcall <- parse_funcall()
  consume(";")

  list("call", funcall)
}

parse_call_set <- function() {
  consume("call_set")
  var_name <- token_val(peek_bump())
  consume("=")
  funcall <- parse_funcall()
  consume(";")

  list("call_set", var_name, funcall)
}

parse_return <- function() {
  consume("return")

  if (token_val(peek()) == ";") {
    list("return")
  } else {
    expr <- parse_expr()
    consume(";")

    list("return", expr)
  }
}

parse_cond_stmts <- function() {

  list(cond, stmts)
}

parse_while <- function() {
  consume("while")
  consume("(")
  cond <- parse_expr()
  consume(")")
  consume("{")
  stmts <- parse_stmts()
  consume("}")

  list("while", cond, stmts)
}

parse_when <- function() {
  consume("when")
  consume("(")
  cond <- parse_expr()
  consume(")")
  consume("{")
  stmts <- parse_stmts()
  consume("}")

  append(list(cond), stmts)
}

parse_case <- function() {
  consume("case")

  when_clauses <- list()
  while (token_val(peek()) == "when") {
    when_clause <- parse_when()
    when_clauses <- list_append(when_clauses, when_clause)
  }

  append(list("case"), when_clauses)
}

parse_vm_comment <- function() {
  consume("_cmt")
  consume("(")
  cmt <- token_val(peek_bump())
  consume(")")
  consume(";")

  list("_cmt", cmt)
}

parse_vm_debug <- function() {
  consume("_debug")
  consume("(")
  consume(")")
  consume(";")

  list("_debug")
}

parse_stmt <- function() {
  switch(token_val(peek())
    ,"set"      = parse_set()
    ,"call"     = parse_call()
    ,"call_set" = parse_call_set()
    ,"return"   = parse_return()
    ,"while"    = parse_while()
    ,"case"     = parse_case()
    ,"_cmt"     = parse_vm_comment()
    ,"_debug"   = parse_vm_debug()
    ,stop("unsupported")
  )
}

parse_stmts <- function() {
  stmts <- list()

  while (token_val(peek()) != "}") {
    stmt <- parse_stmt()
    stmts <- list_append(stmts, stmt)
  }

  stmts
}

parse_var <- function() {
  consume("var")
  var_name <- token_val(peek_bump())

  if (token_val(peek()) == "=") {
    consume("=")
    expr <- parse_expr()
    consume(";")

    list("var", var_name, expr)
  } else {
    consume(";")

    list("var", var_name)
  }
}

parse_func_def <- function() {
  consume("func")
  fn_name <- token_val(peek_bump())
  consume("(")
  args <- parse_args()
  consume(")")

  consume("{")
  stmts <- list()
  while (token_val(peek()) != "}") {
    if (token_val(peek()) == "var") {
      stmt <- parse_var()
      stmts <- list_append(stmts, stmt)
    } else {
      stmt <- parse_stmt()
      stmts <- list_append(stmts, stmt)
    }
  }
  consume("}")

  list("func", fn_name, args, stmts)
}

parse_top_stmts <- function() {
  top_stmts <- list()

  while (!is_end()) {
    fn_def <- parse_func_def()
    top_stmts <- list_append(top_stmts, fn_def)
  }

  top_stmts
}

parse <- function() {
  top_stmts <- parse_top_stmts()
  append(list("top_stmts"), top_stmts)
}

# --------------------------------

g_tokens <- read_tokens()
ast <- parse()
json_pretty_print(ast)
