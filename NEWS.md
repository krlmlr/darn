# darn 0.3-3 (2016-07-20)

- First draft of vignette.
- New `get_target_path()`, calls `file.path()` for `get_target_dir()`.
- Don't export `init_()` and `done_()`.
- Consistently use `R -q` instead of `Rscript` in the `Makefile`.
- Improve documentation for `init()` and `done()`.
- Improve order of entries in `Dependencies` file.
- Terminology: Prerequisites vs. dependencies.
- Variables specified in `Darnfile` are always overridable via environment variables.
- `Darnfile` is optional.
- Specify packages ahead of CRAN in `Remotes`.
- `R CMD check` passes.


# darn 0.3-2 (2016-07-12)

- Add simple targets (without path or extension) to `Dependencies` (#33).
- Sorting of file names is now always done in the `"C"` locale for stability (#32).


# darn 0.3-1 (2016-07-09)

- Scripts without `done()` are not part of the dependencies anymore.
- New `create_script()` that creates a script that can be sourced and executes all scripts in an appropriate order (#31).
- Script errors now lead to termination of the `make` process (#27).
- Clash for environment variables is reported only if the values differ (#26).
- Warn if search path is modified when running `init()`.
- Package installation in temporary library is done in helper.
- Avoid overly long lines in `Dependencies` file (#29).


# darn 0.3 (2016-07-07)

Bug fixes
---------

- `Dependencies` file is updated properly (#23).
- Reverse edges in `dep_graph()`.

API
---

- New argument `path` to `init()`, if automatic detection of current script doesn't work.
- New `get_target_dir()` returns target directory.
- `init()` and `done()` return path info, invisibly.

Features
---

- Calls like `x <- darn::init()` and `x <- darn::done()` are also detected in source files.
- The `Makefile` now by default calls the new `run()` function which tracks the current file in a robust fashion.
- Use `ezknitr::ezspin()` by default for rendering (#22).
- If `create_makefile()` doesn't overwrite the config file, a warning is given.

Internal
--------

- Use `call. = FALSE` in `warning()` calls to avoid confusing error messages.
- Split `make` tests for better parallelization.
- Rename `source_dir` to `src_dir` and `base_dir` to `root_dir` (#25).
- Properly clear current script name in `run()`.
- New `file_path()` to simplify working with edge cases such as empty path components and `"."` components.
- Use `rflow`.

Version 0.2 (2016-01-19)
===

- Feature-complete release.
- Allow passing environment variables to the scripts, which are then loaded into the workspace along with the data (#15).
- Default values for configuration variables are added to, and queried from, the `Darnfile` (#18).
- New `dep_graph()` returns dependency graph (#17).
- Satisfy `R CMD check`.


Version 0.1 (2016-01-18)
===

- `init()` and `done()`, which should be added as first and last statement for each script, respectively
- `create_makefile()` for creation of `Makefile` (#6), and `create_dep_file()` intended to be called by `Makefile`.
- Project configuration is stored in a `Darnfile` (fixed name), and initially specified through arguments `src_dir`, `out_dir`, and `script` to `create_makefile()`.  If configuration items are missing, they are replaced with defaults by the `Makefile` (#5).
- Dependencies, and rules to process scripts, are stored in `Dependencies`.
