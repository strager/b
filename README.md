# b

b is a build system.

## TODO

* Default rules.  If a file doesn't have an associated rule,
  should it behave like Make where the presence of a file
  indicates the file is built?

* Concurrency.  Need a good, flexible model.  MVars or STM
  for management?  Explicit dictionaries for polymorphism?

* More examples.  More tests.

* Lazy rechecking, like Make and Shake.

* Server.

## Ideas

### Core features.

* Remove files built previously from now-deleted file.  This
  will lead to reproduceable build outputs.

* Express commands run as dependencies.

### Adoption

* Support Shake DSL.
* Support Ninja files.
* Support CMake directly.

### User-land features

* Output capture.
* Filesystem change tracking.
* Execute only rebuilt tests.
* Check files using digests.
* C++ compilation clumping.
* distcc support.
* ccache support.
