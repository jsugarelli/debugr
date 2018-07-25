#' Switching debug mode on and off
#'
#' The behavior of \code{debugr}'s main function, \code{\link{dwatch}},
#' depends on whether or not the \code{debugr} \emph{debug mode} is activated or
#' not. The debug mode is turned on and off by setting the global option
#' \code{debugr.active} to \code{TRUE} and \code{FALSE}, respectively. This can
#' be accomplished with the \code{debugr_switchOn} and \code{debugr_switchOff}
#' functions, or manually by running \code{options(debugr.active = TRUE)}.
#'
#' When \code{debugr.active = TRUE} the debug mode is enabled and
#' \code{\link{dwatch}} produces debugging outputs to the console (or to a
#' file). In contrast, when the debug mode is disabled, \code{\link{dwatch}}
#' remains "silent" and no output whatsoever will be shown.
#' @name debugmode
NULL


#' @describeIn debugmode Switches on the global option for debugging
#' @export
debugr_switchOn <- function() {
  options(debugr.active = TRUE)
}


#' @describeIn debugmode Switches off the global option for debugging
#' @export
debugr_switchOff <- function() {
  if(!is.null(getOption("debugr.active"))) options(debugr.active = FALSE)
}


#' @describeIn debugmode Check if debug mode is currently active or not
#' @export
debugr_isActive <- function() {
  res <- base::getOption("debugr.active")
  if(!is.null(res)) {
    if(res == TRUE) return(TRUE)
    else return(FALSE)
  }
  else {
    return(FALSE)
  }
}


# Creates the visual frame (top and bottom lines) around the debugr debugging messages
frame.dwatch <- function(show.caption = FALSE, caption = "", sym = "-") {
  if(show.caption == FALSE) caption <- ""

  width <- getOption("width", default = 80)

  pos <- floor((width - nchar(caption))/2)
  if(nchar(caption) != 0) pos <- pos - 1
  if(pos < 0) pos <- 0

  if(show.caption == TRUE) {
    if(pos == 0) caption.msg <- paste0(caption, ":")
    else caption.msg <- paste0(" ", caption, " ")
    cat("\n")
    for(i in 1:pos) {
      cat(sym)
    }
    cat(caption.msg)
    for(i in 1:(width-pos-nchar(caption.msg))) {
      cat(sym)
    }
  }
  else {
    for(i in 1:width) {
      cat(sym)
    }
  }
  cat("\n")
}


# Tries to identify the (path and) file name of the r script that is being
# debugged. Returns empty string if file name cannot be identified
filename.dwatch <- function() {
  path <- ""
  if(rstudioapi::isAvailable()==TRUE) {
    path <- rstudioapi::getActiveDocumentContext()$path
  }
  if(path == "") {
    path <- rprojroot::thisfile_r()
    if(is.null(path)) path <- ""
  }
  return(path)
}


# Cuts off source code included in the debugr debugging messages after N
# characters (with N being the width of the R console; if the width cannot be
# identified, N is assumed to be 80). Cut-offs will be marked with "[...]".
#
# Args:
#
# v: Vector of source code lines.
#
# Return: Vector of source code lines, fitted to the width of the user's R
# console.
cutoff.dwatch <- function(v) {
  width <- getOption("width", default = 80)

  for(i in 1:length(v)) {
    if(nchar(v[i]) > width) v[i] <- paste0(substr(v[i], start=1, stop=width-6), " [...]\n")
  }
  return(v)
}


# Returns a source code range around the current call of dwatch() or the line
# number in which the dwatch() call is located in the source file.
#
# Args:

# id: The unique ID of the dwatch() call (chosen by the user as an
# argument of dwatch()).

# range: The source code range to be returned (half of the lines above the call
# to dwatch(), half of the lines below the call).
#
# line.number: Determines if, instead of the source code section around the
# dwatch() call, the line number of the dwatch call in the source file shall be
# returned (TRUE: return line number). If TRUE, argument range is ignored.
#
# Return: The source code section around the the call of dwatch() or the line
# number of  the dwatch() call in the source file.
source.dwatch <- function(id, range = 6, line.number=FALSE) {
  path <- filename.dwatch()
  res <- ""
  lno <- 0

  if(path != "") {
    conn <- file(path, open="r")
    lin <-readLines(conn)
    for (i in 1:length(lin)){
      if(!is.na(grep("dwatch", lin[i])[1]) && !is.na(grep(id, lin[i])[1])) {
        from <- max(1, i-floor(range/2))
        to <- min(length(lin), i+floor(range/2))
        res <- lin[from:to]
        lno <- i
      }
    }
    close(conn)
  }
  res <- paste0(cutoff.dwatch(res), collapse = "\n")
  if(line.number == FALSE) return(res)
  else return(lno)
}


# Produces a debugr debugger message for an object or an expression.
#
# Args:
#
# obj.name: An object the value of which is to be printed.
#
# env: The (calling) environment in which to look for the object
#
# fun: A function that shall be applied to the object. If no function is
# provided, object values are shown using print(), except in the case of
# dataframes where View() is used per default.
#
# arg: A named vector of arguments to the function fun. obj.name is assumed to
# be fun's first argument. The names of the elements of arg are the names
# of fun's arguments, arg's element values themselves are the arguments' values.
#
# expr: An expression to be evaluated in the debug message.
#
# Return: A formatted debugr debug message
print.dwatch <- function(obj.name, env, fun, arg, expr) {
  if(!is.null(obj.name)) {
    obj <- get(obj.name, envir = env)

    if(typeof(obj) != "closure") {
      if(!is.null(fun)) {
        cl <- paste0(fun, "(", obj.name)
        if(!is.null(arg)) {
          for(i in 1:length(arg)) {
            cl <- paste0(cl, ",", names(arg)[i], "=", arg[i])
          }
        }
        cl <- paste0(cl, ")")
        cat(paste0("\n** ", obj.name, ":\n"))
        print(eval(parse(text=cl), envir = env))
        cat("\n")
      }
      else {
        if(class(obj) == "data.frame") {
          cat(paste0("\n** ", obj.name, ": Dataframe shown in Data Viewer.\n"))
          cat("\n")
          utils::View(get(obj.name, envir = env), title=obj.name)
        }
        else {
          cat(paste0("\n** ", obj.name, ":\n"))
          print(obj)
          cat("\n")
        }
      }
    }
  }
  else {
    if(!is.null(expr)) {
      cat(paste0("\n** Expression: ", expr, "\n"))
      print(eval(parse(text=expr), envir = env))
    }
  }
}



#' Printing debug outputs during runtime
#'
#' Prints a debug output to the console or to a file. A debug output can consist
#' of a static text message, the values of one or more objects (potentially
#' transformed by applying some functions) or the value of one or multiple (more
#' complex) R expressions. Whether or not a debug message is displayed can be
#' made dependent on the evaluation of a criterion phrased as an R expression.
#' Generally, debug messages are only shown if the debug mode is activated. The
#' debug mode is activated and deactivated with \code{\link{debugr_switchOn}}
#' and \code{\link{debugr_switchOff}}, respectively, which change the logical
#' \code{debugr.active} value in the global options. Since debug messages are
#' only displayed in debug mode, the \code{dwatch} function calls can even
#' remain in the original code as they remain silent and won't have any effect
#' until the debug mode is switched on again.
#'
#'
#'
#' @param crit An string containing an expression that determines if any debug
#'   outputs shall be displayed at all. Only, if \code{crit} evaluates to
#'   \code{TRUE}, a debug output will be shown.
#' @param objs A vector of object names (as strings). The values of these
#'   objects will be displayed in the debug output.
#' @param funs A vector of function names (as strings) that shall be applied to
#'   the objects in \code{objs}, one function per object. \code{funs} must have
#'   the same length as \code{objs}. If no function shall be applied to an
#'   object, the respective element in the \code{funs} vector must be
#'   \code{NULL}. The functions in \code{funs} must undertake the task of
#'   printing the object.
#' @param args A list of vectors containing additional arguments for the
#'   functions in \code{funs}. It is assumed that the first argument of each
#'   function in \code{funs} is the respective object from \code{objs}.
#'   Additional arguments can then be supplied with \code{args}. The \code{args}
#'   list must have the same number of elements as \code{funs}. If a function
#'   does not receive any additional arguments, the respective element in the
#'   \code{args} list must be \code{NULL}. Each element of \code{args} is a
#'   vector of named elements. The element name is the name of the additions
#'   argument to the respective \code{funs} function, the elements value is the
#'   argument's value.
#' @param show.all Prints all objects from the (calling) environment. If set to
#'   \code{TRUE}, \code{objs} is ignored and all objects in the enviroment (with
#'   the exception of functions) are included in the debug output.
#' @param expr A vector of strings containing expressions to be evaluated and
#'   displayed in the debug output. This output comes on top of any \code{msg}
#'   or \code{objs} output.
#' @param msg A string containing a general message to be displayed.
#' @param halt If \code{TRUE}, the execution of the debugged R script is stopped
#'   after printing the output.
#' @param unique.id A unqiue string ID that can be chosen by the user. This ID
#'   is displayed in the debug output and is used to identify the code section
#'   that contains the \code{dwatch} call. By default, when a debug output is
#'   displayed, \code{dwatch} tries to show an extract from the code thats
#'   surrounds the \code{dwatch} call (this feature can be turned off by setting
#'   \code{suppress.source} to \code{TRUE}).
#' @param suppress.source If \code{TRUE} (default), \code{dwatch} tries to find
#'   the code section that includes the \code{dwatch} call and displays it as
#'   part of the debug output. Requires \code{unique.id} to be set.
#' @param show.frame If \code{TRUE} (default), a frame is displayed at the top
#'   and the bottom of the debug output.
#' @param filename If a filename is provided, all debug message are only printed
#'   to the file and not shown on the R console.
#'
#' @seealso debugr_switchOn, debugr_switchOff, debugr_isActive
#' @examples
#'
#' library(debugr)
#'
#' # --- A simple example to print the value of an object
#' myfunction <- function(x) {
#'   justastring <- "Not much information here"
#'   z <- 1
#'
#'   for(i in 1:x) {
#'     # This call can remain in your code; it is only activated when
#'     # the debug mode is switched on
#'     dwatch(crit = "z > 40000", objs = c("z"))
#'     z <- z * i
#'   }
#'   invisible(z)
#' }
#'
#' # Turn debug mode on
#' debugr_switchOn()
#'
#' # Call function for debugging
#' myfunction(10)
#'
#'
#'
#' # --- Applying a function to the object that is printed
#' myfunction <- function(x) {
#'   justastring <- "Not much information here"
#'   z <- 1
#'
#'   for(i in 1:x) {
#'     dwatch(crit = "z > 40000", objs = c("z"), funs=c("format"),
#'     args = as.list(c(big.mark = "\",\"")))
#'     z <- z * i
#'   }
#'   invisible(z)
#' }
#'
#' myfunction(10)
#'
#'
#'
#' # --- Same thing, this time with a expression
#' myfunction <- function(x) {
#'   justastring <- "Not much information here"
#'   z <- 1
#'
#'   for(i in 1:x) {
#'     dwatch(crit = "z > 40000", expr=c("format(z, big.mark = \",\")"))
#'     z <- z * i
#'   }
#'   invisible(z)
#' }
#'
#' myfunction(10)
#'
#' @export
dwatch <- function(crit = "", objs = NULL, funs = NULL, args = NULL,
                   show.all = FALSE, expr = NULL, msg = "", halt = FALSE,
                   unique.id = "", suppress.source = FALSE, show.frame = TRUE,
                   filename = ""){
  if(debugr_isActive() == TRUE) {
    if(filename != "") sink(file=filename, append = TRUE)
    go <- FALSE
    if(crit != "") {
      env <- parent.frame()
      crit.expr <- parse(text = crit)
      res <- eval(crit.expr, envir = env)
      if(!is.logical(res)) stop(paste0("\nYour criterion '", crit, "' does not evaluate as TRUE or FALSE.\n"))
      else go <- res
    }
    else go <- TRUE

    if(go == TRUE) {
      if(show.frame == TRUE) {
        if(unique.id != "") caption <- paste0("DEBUGR MESSAGE [", unique.id, "]")
        else caption <- "DEBUGR MESSAGE"
        frame.dwatch(TRUE, caption)
      }

      if(suppress.source == FALSE && unique.id != "") {
        if(filename.dwatch() != "") {
          cat("\n")
          frame.dwatch(TRUE, "Your Code", "*")
          cat(source.dwatch(unique.id))
          cat("\n")
          frame.dwatch(FALSE, sym="*")
          cat("\n")
        }
      }

      if(msg != "") cat(paste0(msg, "\n"))

      if(show.all == FALSE) {
        if(!is.null(objs)) {
          for(i in 1:length(objs)) {
            fun <- NULL
            arg <- NULL
            if(exists(objs[i], envir=env)) {
              if(!is.null(funs)) {
                if(NROW(funs) >= i) fun <- funs[i]
              }
              if(!is.null(args)) {
                if(length(args) >= i) arg <- args[i]
              }

              print.dwatch(objs[i], env, fun, arg, NULL)
            }
            else {

            }
          }
        }
      }
      else {
        env.objs <- ls(name=env)
        for(i in 1:length(env.objs)) {
          print.dwatch(env.objs[i], env, NULL, NULL, NULL)
        }
      }

      if(!is.null(expr))
      {
        for(i in 1:length(expr)) {
          print.dwatch(NULL, env, NULL, NULL, expr[i])
        }
      }

      if(halt == TRUE) stop("Code execution stopped by debugr", call. = FALSE)

      if(show.frame == TRUE) frame.dwatch(FALSE)
    }

    if(filename != "") sink()
  }
}
