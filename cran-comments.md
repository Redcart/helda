## Test environments
 * local macOS 10.12.6
    - release
 * Ubuntu Xenial 16.04 (on travis-ci)
    - oldrel, release, devel
 * win-builder
    - oldrel, release, devel_gcc8

 ## R CMD check results
 There were no ERRORs no WARNINGs no Notes

 ## win-builder (devel only)
 2 errors concerning two unit tests that fail

 ## R CMD check results

    == testthat
    results
    ===========================================================

     [ OK: 4 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 2 ]

    1. Failure: lift curve for titanic data set
    (@test-lift_curve.R#14)
    2. Failure: lift effect
    for titanic data set (@test-lift_effect.R#14)


    Error: testthat unit tests failed
    Execution
    halted

 Basically, in these tests I check that my graphics done with ggplot2 match some graphics that I
 previousy done and stored ind Rda format. This allows me to ensure that my functions that produce        graphics have the same behavior from one version to another version of R.

 The other devel_gcc8 version of R does not raise this error.
