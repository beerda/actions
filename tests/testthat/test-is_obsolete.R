test_that("is_obsolete", {
    dep1 <- tempfile()
    dep2 <- tempfile()
    target1 <- tempfile()
    target2 <- tempfile()

    a1 <- action(targets = c(target1, target2))
    a2 <- action(targets = c(target1, target2),
                 depends = c(dep1, dep2))

    expect_true(is_obsolete(a1))
    expect_true(is_obsolete(a2))

    file.create(target1)
    Sys.setFileTime(target1, "1990-01-01")
    expect_true(is_obsolete(a1))
    expect_true(is_obsolete(a2))

    file.create(target2)
    Sys.setFileTime(target1, "1990-01-02")
    expect_true(!is_obsolete(a1))
    expect_true(is_obsolete(a2))

    file.create(dep1)
    Sys.setFileTime(dep1, "1980-01-01")
    expect_true(!is_obsolete(a1))
    expect_true(is_obsolete(a2))

    file.create(dep2)
    Sys.setFileTime(dep2, "1980-01-02")
    expect_true(!is_obsolete(a1))
    expect_true(!is_obsolete(a2))

    Sys.setFileTime(dep2, "1990-02-01")
    expect_true(!is_obsolete(a1))
    expect_true(is_obsolete(a2))
})
