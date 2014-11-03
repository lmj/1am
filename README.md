# 1am

A minimal testing framework for Common Lisp.

## Synopsis

The entire API consists of: `test`, `is`, `signals`, `run`, and
`*tests*`.

    (defpackage :example (:use :cl :1am))
    (in-package :example)
    
    (test foo-test
      (is (= 1 1))
      (is (zerop 0)))
    
    (test bar-test
      (signals simple-error
        (error "bar!")))

Running:

    CL-USER> (in-package :example)
    #<PACKAGE "EXAMPLE">
    
    EXAMPLE> (run) ; run all tests
    BAR-TEST.
    FOO-TEST..
    Success: 2 tests, 3 checks.
    
    EXAMPLE> (foo-test) ; tests are just functions
    FOO-TEST..
    Success: 1 test, 2 checks.
    
    EXAMPLE> (run '(bar-test)) ; run particular tests
    BAR-TEST.
    Success: 1 test, 1 check.
    
    EXAMPLE> (setf *tests* '(foo-test)) ; set the default tests to run
    (FOO-TEST)
    EXAMPLE> (run)
    FOO-TEST..
    Success: 1 test, 2 checks.

## Overview

1am is tiny (~60 lines of code), has no dependencies, and will be
stable for the foreseeable future. It was originally written for large
multi-threaded test suites where the complexity of popular testing
frameworks caused problems. The philosophy of 1am is that simple is
better. 1am is especially tailored for fixing bugs that are difficult
to reproduce.

* A test failure always results in a breakpoint with backtrace
  (otherwise a rare opportunity may be missed).

* The order of tests is shuffled on each run so that bugs concealed by
  unintentional dependencies between tests may be exposed.

* If a test run fails to complete then subsequent runs will use the
  same random state that led to the failure, until a successful run.

* Checks may occur inside threads.

* 1am's negligible overhead is unlikely to bury timing bugs and race
  conditions in the code being tested.

* Tests are runnable as ordinary functions of the same name.

* Tests are compiled up front (some testing frameworks, such as 5am,
  defer compilation by default).

* The generated code is small; compiling tests is ~8x faster than 5am
  configured with `:compile-at :definition-time`. Compiling large-ish
  tests with 5am can cause the default heap size to be exceeded on
  some platforms (32-bit SBCL, Allegro Express).

* Type inferencing (if present) works inside the `is` macro.

* Since 1am is small and stable, copying it into the project being
  tested is a viable option (be sure to use a different package name).

## Workflow

Instead of a hierarchy of test suites, 1am has a flat list of tests in
`*tests*`. When a test is compiled, it is added to `*tests*`. A
typical workflow might be:

1. Run all tests to verify they pass.

2. Set `*tests*` to `nil`, then compile specific tests that are
   relevant to some feature, perhaps writing new tests in the process.
   Now `*tests*` contains only those tests.

3. When changes for that feature are complete, recompile all tests to
   confirm those changes, which will add them back to `*tests*`.

4. Run all tests again to check that nothing is messed up.

## API

* [special variable] **`*tests*`** -- A list of tests; the default
  argument to `run`.

* [macro] **`test`** `name` *`&body`* `body` -- Define a test function
  and add it to `*tests*`.

* [macro] **`is`** `form` -- Assert that `form` evaluates to non-nil.

* [macro] **`signals`** `condition` *`&body`* `body` -- Assert that
  `body` signals a condition of type `condition`.

* [function] **`run`** *`&optional`* `tests` -- Run each test in the
  sequence `tests`. Default is `*tests*`.
