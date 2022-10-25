test_that("is_job", {
    a1 <- action(targets = c('foo', 'bar'))
    a2 <- action(targets = c('bar', 'baz'))
    a3 <- list(targets = c('foo', 'bar'))

    expect_true(is_job(list()))
    expect_true(is_job(list(a1)))
    expect_true(is_job(list(a1, a2)))
    expect_true(!is_job(list(a1, a2, a3)))
})
