`%||%` <- function(a, b) if (is.null(a)) b else a

`%nin%` <- function(a, b) !(a %in% b)

strip_extension <- function(path) {
  gsub("[.][^.]*$", "", path)
}

relative_to <- function(path, root) {
  if (length(path) == 0L) {
    return ()
  }

  R.utils::getRelativePath(path, root)
}

getMakeLazyLoadDB <- function() {
  eval(bquote(function (from, filebase, compress = TRUE, ascii = FALSE,
                        variables) {
    ascii <- as.logical(ascii)
    if (is.na(ascii))
      stop("'ascii' must be TRUE or FALSE", domain = NA)
    ascii <- as.integer(ascii)
    envlist <- function(e) {
      .(Internal)(getVarsFromFrame(ls(e, all.names = TRUE), e, FALSE))
    }
    envtable <- function() {
      idx <- 0
      envs <- NULL
      enames <- character(0L)
      find <- function(v, keys, vals) {
        for (i in seq_along(keys)) if (identical(v, keys[[i]]))
          return(vals[i])
        NULL
      }
      getname <- function(e) find(e, envs, enames)
      getenv <- function(n) find(n, enames, envs)
      insert <- function(e) {
        idx <<- idx + 1
        name <- paste("env", idx, sep = "::")
        envs <<- c(e, envs)
        enames <<- c(name, enames)
        name
      }
      list(insert = insert, getenv = getenv, getname = getname)
    }
    lazyLoadDBinsertValue <- function(value, file, ascii, compress, hook) {
      .(Internal)(lazyLoadDBinsertValue(value, file, ascii, compress, hook))
    }
    lazyLoadDBinsertListElement <- function(x, i, file, ascii, compress, hook) {
      lazyLoadDBinsertValue(x[[i]], file, ascii, compress, hook)
    }
    lazyLoadDBinsertVariable <- function(n, e, file, ascii, compress, hook) {
      x <- .(Internal)(getVarsFromFrame(n, e, FALSE))
      lazyLoadDBinsertValue(x[[1L]], file, ascii, compress, hook)
    }
    mapfile <- paste(filebase, "rdx", sep = ".")
    datafile <- paste(filebase, "rdb", sep = ".")
    close(file(datafile, "wb"))
    table <- envtable()
    varenv <- new.env(hash = TRUE)
    envenv <- new.env(hash = TRUE)
    envhook <- function(e) {
      if (is.environment(e)) {
        name <- table$getname(e)
        if (is.null(name)) {
          name <- table$insert(e)
          data <- list(bindings = envlist(e), enclos = parent.env(e),
                       attributes = attributes(e), isS4 = isS4(e),
                       locked = environmentIsLocked(e))
          key <- lazyLoadDBinsertValue(data, datafile,
                                       ascii, compress, envhook)
          assign(name, key, envir = envenv)
        }
        name
      }
    }
    if (is.null(from) || is.environment(from)) {
      if (!missing(variables))
        vars <- variables
      else vars <- ls(from, all.names = TRUE)
    }
    else if (is.list(from)) {
      vars <- names(from)
      if (length(vars) != length(from) || any(!nzchar(vars)))
        stop("source list must have names for all elements")
    }
    else stop("source must be an environment or a list")
    for (i in seq_along(vars)) {
      key <- if (is.null(from) || is.environment(from))
        lazyLoadDBinsertVariable(vars[i], from, datafile,
                                 ascii, compress, envhook)
      else lazyLoadDBinsertListElement(from, i, datafile, ascii,
                                       compress, envhook)
      assign(vars[i], key, envir = varenv)
    }
    vals <- lapply(vars, get, envir = varenv, inherits = FALSE)
    names(vals) <- vars
    rvars <- ls(envenv, all.names = TRUE)
    rvals <- lapply(rvars, get, envir = envenv, inherits = FALSE)
    names(rvals) <- rvars
    val <- list(variables = vals, references = rvals, compressed = compress)
    saveRDS(val, mapfile)
  }, where = as.environment(list(Internal = as.name(".Internal")))))
}
