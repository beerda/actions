test_that("is_action", {
    a1 <- action(targets = c('foo', 'bar'))
    a2 <- list(targets = c('foo', 'bar'))

    expect_true(is_action(a1))
    expect_true(!is_action(a2))
})
