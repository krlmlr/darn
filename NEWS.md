Version 0.0-4 (2016-01-18)
===

- New arguments `src_dir`, `out_dir`, and `script` to `create_makefile()`.
- Project configuration is stored in a `Darnfile` (fixed name).  If configuration items are missing, they are replaced with defaults by the `Makefile`.
- Rules to process scripts are now stored in `Dependencies`.
one failing test remains
- New package name: `darn`.


Version 0.0-3 (2016-01-08)
===

- New `create_dep_file()` and `create_makefile()` for creation of `Makefile` to satisfy tests (#6).


Version 0.0-2 (2016-01-07)
===

- Fix documentation.


Version 0.0-1 (2016-01-07)
===

- Implement `init()` and `done()`, which should be added as first and last statement for each script, respectively


