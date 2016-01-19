Version 0.1-2 (2016-01-19)
===

- New `dep_graph()` returns dependency graph (#17).
- Satisfy `R CMD check`.


Version 0.1-1 (2016-01-19)
===

- Default values for configuration variables are added to, and queried from, the `Darnfile` (#18).
- Allow passing environment variables to the scripts, which are then loaded into the workspace along with the data (#15).


Version 0.1 (2016-01-18)
===

- `init()` and `done()`, which should be added as first and last statement for each script, respectively
- `create_makefile()` for creation of `Makefile` (#6), and `create_dep_file()` intended to be called by `Makefile`.
- Project configuration is stored in a `Darnfile` (fixed name), and initially specified through arguments `src_dir`, `out_dir`, and `script` to `create_makefile()`.  If configuration items are missing, they are replaced with defaults by the `Makefile` (#5).
- Dependencies, and rules to process scripts, are stored in `Dependencies`.
