match_arg <- function(arg, assert = TRUE, fixed = FALSE) {

  if (is.null(arg)) return(NULL)
  sys_parent <- sys.parent()
  formal_args <- formals(sys.function(sys_parent))
  choices <- eval(formal_args[[as.character(substitute(arg))]],
                  envir = sys.frame(sys_parent))

  if (length(choices) == 1) {
    assert_choice(arg, choices = choices)
  } else if (length(arg) > 1 || identical(arg, choices)) {
    NULL
  } else {
    if (assert) assert_choice(arg, choices)
    arg
  }

}

assert_uuid <- function(x, ...) {

  assert_string(
    x,
    pattern = "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
  )

}

assert_datetime_string <- function(x, ...) {

  assert_posixct(as_datetime(x))

}

add_class <- function(obj, ...) {

  set_class(obj, union(c(...), class(obj)))

}
