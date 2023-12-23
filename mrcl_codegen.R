options(warn = -1)

source("lib/utils.R")
source("lib/json.R")

g_label_id <- 0

next_label_id <- function() {
  g_label_id <<- g_label_id + 1
  g_label_id
}

# --------------------------------

asm_prologue <- function() {
  puts("  push bp")
  puts("  mov bp sp")
}

asm_epilogue <- function() {
  puts("  mov sp bp")
  puts("  pop bp")
}

to_fn_arg_disp <- function(name, fn_args) {
  n <- find_index(name, fn_args)
  n + 1
}

to_lvar_disp <- function(name, lvars) {
  n <- find_index(name, lvars)
  -n
}

gen_expr_add <- function() {
  puts("  pop reg_b")
  puts("  pop reg_a")
  puts("  add reg_a reg_b")
}

gen_expr_mul <- function() {
  puts("  pop reg_b")
  puts("  pop reg_a")
  puts("  mul reg_b")
}

gen_expr_eq_neq <- function(type) {
  puts("  pop reg_b")
  puts("  pop reg_a")

  label_id <- next_label_id()

  label_then <- sprintf("then_%d", label_id)
  label_end <- sprintf("end_%s_%d", type, label_id)

  puts("  compare")
  puts(sprintf("  jump_eq %s", label_then))

  if (type == "eq") {
    puts("  mov reg_a 0")
  } else {
    puts("  mov reg_a 1")
  }

  puts(sprintf("  jump %s", label_end))
  puts(sprintf("label %s", label_then))

  if (type == "eq") {
    puts("  mov reg_a 1")
  } else {
    puts("  mov reg_a 0")
  }

  puts(sprintf("label %s", label_end))
}

gen_expr_eq <- function(type) {
  gen_expr_eq_neq("eq")
}

gen_expr_neq <- function(type) {
  gen_expr_eq_neq("neq")
}

gen_expr_binop <- function(fn_args, lvars, expr) {
  op  <- expr[[1]]
  lhs <- expr[[2]]
  rhs <- expr[[3]]

  gen_expr(fn_args, lvars, lhs)
  puts("  push reg_a")
  gen_expr(fn_args, lvars, rhs)
  puts("  push reg_a")

  switch(op
    ,"+"  = gen_expr_add()
    ,"*"  = gen_expr_mul()
    ,"==" = gen_expr_eq()
    ,"!=" = gen_expr_neq()
    ,stop("unsupported")
  )
}

gen_expr <- function(fn_args, lvars, expr) {
  switch(u_typeof(expr)
    ,"list"      = gen_expr_binop(fn_args, lvars, expr)
    ,"numeric"   = puts(sprintf("  mov reg_a %d", expr))
    ,"character" = {
      if (includes(expr, lvars)) {
        disp <- to_lvar_disp(expr, lvars)
        puts(sprintf("  mov reg_a [bp:%d]", disp))
      } else if (includes(expr, fn_args)) {
        disp <- to_fn_arg_disp(expr, fn_args)
        puts(sprintf("  mov reg_a [bp:%d]", disp))
      } else {
        stop("unsupported")
      }
    }
    ,stop("unsupported")
  )
}

gen_funcall <- function(fn_args, lvars, funcall) {
  fn_name <- funcall[[1]]
  args <- list_tail(funcall, 1)

  n <- length(args)
  while (n >= 1) {
    gen_expr(fn_args, lvars, args[[n]])
    puts("  push reg_a")
    n <- n - 1
  }

  gen_vm_comment_common(sprintf("call  %s", fn_name))

  puts(sprintf("  call %s", fn_name))
  puts(sprintf("  add sp %d", length(args)))
}

gen_call <- function(fn_args, lvars, stmt) {
  funcall <- stmt[[2]]

  gen_funcall(fn_args, lvars, funcall)
}

gen_call_set <- function(fn_args, lvars, stmt) {
  dest    <- stmt[[2]]
  funcall <- stmt[[3]]

  gen_funcall(fn_args, lvars, funcall)

  disp <- to_lvar_disp(dest, lvars)
  puts(sprintf("  mov [bp:%d] reg_a", disp))
}

gen_set_common <- function(fn_args, lvars, dest, expr) {
  gen_expr(fn_args, lvars, expr)

  if (includes(dest, lvars)) {
    disp <- to_lvar_disp(dest, lvars)
    puts(sprintf("  mov [bp:%s] reg_a", disp))
  } else {
    stop("unsupported")
  }
}

gen_set <- function(fn_args, lvars, stmt) {
  dest <- stmt[[2]]
  expr <- stmt[[3]]

  gen_set_common(fn_args, lvars, dest, expr)
}

gen_return <- function(fn_args, lvars, stmt) {
  if (length(stmt) == 1) {
    # no return value
  } else if (length(stmt) == 2) {
    expr <- stmt[[2]]
    gen_expr(fn_args, lvars, expr)
  } else {
    stop("unsupported")
  }

  asm_epilogue()
  puts("  ret")
}

gen_while <- function(fn_args, lvars, stmt) {
  cond_expr <- stmt[[2]]
  stmts     <- stmt[[3]]

  label_id <- next_label_id()

  label_begin <- sprintf("while_%d", label_id)
  label_end   <- sprintf("end_while_%d", label_id)

  puts(sprintf("label %s", label_begin))

  gen_expr(fn_args, lvars, cond_expr)

  puts("  mov reg_b 0")
  puts("  compare")

  puts(sprintf("  jump_eq %s", label_end))

  gen_stmts(fn_args, lvars, stmts)

  puts(sprintf("  jump %s", label_begin))

  puts(sprintf("label %s", label_end))
}

gen_case <- function(fn_args, lvars, stmt) {
  when_clauses <- list_tail(stmt, 1)

  label_id <- next_label_id()

  when_idx <- -1

  label_end <- sprintf("end_case_%d", label_id)
  label_end_when_head <- sprintf("end_when_%d", label_id)

  for (when_clause in when_clauses) {
    when_idx <- when_idx + 1
    cond  <- when_clause[[1]]
    stmts <- list_tail(when_clause, 1)

    gen_expr(fn_args, lvars, cond)

    puts("  mov reg_b 0")
    puts("  compare")

    puts(sprintf("  jump_eq %s_%d", label_end_when_head, when_idx))

    gen_stmts(fn_args, lvars, stmts)

    puts(sprintf("  jump %s", label_end))

    puts(sprintf("label %s_%d", label_end_when_head, when_idx))
  }

  puts(sprintf("label %s", label_end))
}

gen_vm_comment_common <- function(cmt) {
  puts(sprintf("  _cmt %s", gsub(" ", "~", cmt)))
}

gen_vm_comment <- function(stmt) {
  cmt <- stmt[[2]]
  gen_vm_comment_common(cmt)
}

gen_vm_debug <- function() {
  puts("  _debug")
}

gen_stmt <- function(fn_args, lvars, stmt) {
  switch(stmt[[1]]
    ,"set"      = gen_set(      fn_args, lvars, stmt)
    ,"call"     = gen_call(     fn_args, lvars, stmt)
    ,"call_set" = gen_call_set( fn_args, lvars, stmt)
    ,"return"   = gen_return(   fn_args, lvars, stmt)
    ,"while"    = gen_while(    fn_args, lvars, stmt)
    ,"case"     = gen_case(     fn_args, lvars, stmt)
    ,"_cmt"     = gen_vm_comment(stmt)
    ,"_debug"   = gen_vm_debug()
    ,stop("unsupported")
  )
}

gen_stmts <- function(fn_args, lvars, stmts) {
  for (stmt in stmts) {
    gen_stmt(fn_args, lvars, stmt)
  }
}

gen_var <- function(fn_args, lvars, stmt) {
  puts("  add sp -1")

  if (length(stmt) == 3) {
    dest <- stmt[[2]]
    expr <- stmt[[3]]
    gen_set_common(fn_args, lvars, dest, expr)
  }
}

gen_func_def <- function(fn_def) {
  fn_name <- fn_def[[2]]
  fn_args <- fn_def[[3]]
  stmts   <- fn_def[[4]]

  lvars <- list()

  puts(sprintf("label %s", fn_name))
  asm_prologue()

  for (stmt in stmts) {
    if (stmt[[1]] == "var") {
      lvar <- stmt[[2]]
      lvars <- list_append(lvars, lvar)
      gen_var(fn_args, lvars, stmt)
    } else {
      gen_stmt(fn_args, lvars, stmt)
    }
  }

  asm_epilogue()
  puts("  ret")
}

gen_top_stmt <- function(top_stmt) {
  gen_func_def(top_stmt)
}

gen_top_stmts <- function(top_stmts) {
  for (top_stmt in top_stmts) {
    gen_top_stmt(top_stmt)
  }
}

gen_builtin_set_vram <- function() {
  puts("label set_vram")
  asm_prologue()
  puts("  set_vram [bp:2] [bp:3]") # vram_addr value
  asm_epilogue()
  puts("  ret")
}

gen_builtin_get_vram <- function() {
  puts("label get_vram")
  asm_prologue()
  puts("  get_vram [bp:2] reg_a") # vram_addr dest
  asm_epilogue()
  puts("  ret")
}

codegen <- function(ast) {
  puts("  call main")
  puts("  exit")

  top_stmts <- list_tail(ast, 1)
  gen_top_stmts(top_stmts)

  puts("#>builtins")
  gen_builtin_set_vram()
  gen_builtin_get_vram()
  puts("#<builtins")
}

# --------------------------------

src <- read_stdin_all()
ast <- json_parse(src)
codegen(ast)
