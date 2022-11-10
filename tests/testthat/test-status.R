test_that('status', {
    sd <- function(action) {
        status(action, type = 'depends', newer_text = 'n', nonexistent_text = 'e', older_text = 'o')
    }
    st <- function(action) {
        status(action, type = 'targets', newer_text = 'n', nonexistent_text = 'e', older_text = 'o')
    }

    f1 <- tempfile()
    f2 <- tempfile()

    a1 <- action(f2)
    a2 <- action(f2, f1)

    expect_equal(sd(a1), NULL)
    expect_equal(st(a1), 'e')
    expect_equal(sd(a2), 'e')
    expect_equal(st(a2), 'e')

    file.create(f1)
    Sys.setFileTime(f1, "1990-01-01")

    expect_equal(sd(a1), NULL)
    expect_equal(st(a1), 'e')
    expect_equal(sd(a2), 'n')
    expect_equal(st(a2), 'e')

    file.create(f2)
    Sys.setFileTime(f2, "1990-01-02")

    expect_equal(sd(a1), NULL)
    expect_equal(st(a1), 'n')
    expect_equal(sd(a2), 'o')
    expect_equal(st(a2), 'n')

    Sys.setFileTime(f1, "1990-01-03")

    expect_equal(sd(a1), NULL)
    expect_equal(st(a1), 'n')
    expect_equal(sd(a2), 'n')
    expect_equal(st(a2), 'o')
})
