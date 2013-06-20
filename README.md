# b

b is a build system.

## TODO

* Default rules.  If a file doesn't have an associated rule,
  should it behave like Make where the presence of a file
  indicates the file is built?

* Concurrency.  Need a good, flexible model.  MVars or STM
  for management?  Explicit dictionaries for polymorphism?

* Persistant Oracle storage.  SQLite?  Flat file with
  Binary?  Read/Show?  Server/client?

* Make-like top-down no-database incremental building.
  People expect it.

* More examples.  More tests.
