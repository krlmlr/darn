# darn 0.2-3 (2016-07-07)

- Reverse edges in `dep_graph()`.
- Use `rflow`.


Version 0.2-2 (2016-04-11)
===

- New argument `path` to `init()`, if automatic detection of current script doesn't work.
- New `get_target_dir()` returns target directory.
- Support memoizing return values from `init()` and `done()`.
- Properly clear current script name in `run()`.


Version 0.2-1 (2016-02-18)
===

- The `Makefile` now by default calls the new `run()` function which tracks the current file in a robust fashion.
- Use `ezknitr::ezspin()` by default for rendering (#22).
- `init()` and `done()` return path info, invisibly.
- New `file_path()` to simplify working with edge cases such as empty path components and `"."` components.
- If `create_makefile()` doesn't overwrite the config file, a warning is given.
- Use `call. = FALSE` in `warning()` calls to avoid confusing error messages.


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
