test_that("missing_deps", {
    a1 <- action(targets = c('a', 'b'))
    a2 <- action(targets = c('c', 'd'), depends = c('x'))
    a3 <- action(targets = c('e', 'f'), depends = c('a'))
    a4 <- action(targets = c('g', 'h'), depends = c('a', 'y', 'z'))
    j <- job(a1, a2, a3, a4)

    expect_equal(missing_deps(j),
                 list(character(0),
                      c('x'),
                      character(0),
                      c('y', 'z')))
})
