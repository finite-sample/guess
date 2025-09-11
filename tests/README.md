Tests and Coverage
================
06 November, 2018 09:30:10

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

    ## - Not All Tests Passed
    ##   Coverage statistics are approximations of the non-failing tests.
    ##   Use with caution
    ## 
    ##  For further investigation check in testthat summary tables.

| Object                                       | Coverage (%) |
| :------------------------------------------- | :----------: |
| guess                                        |    21.38     |
| [R/eq1dk.R](../R/eq1dk.R)                    |     0.00     |
| [R/eqn1.R](../R/eqn1.R)                      |     0.00     |
| [R/fit\_dk.R](../R/fit_dk.R)                 |     0.00     |
| [R/fit\_nodk.R](../R/fit_nodk.R)             |     0.00     |
| [R/group\_adj.R](../R/group_adj.R)           |     0.00     |
| [R/guess\_lik.R](../R/guess_lik.R)           |     0.00     |
| [R/guessdk\_lik.R](../R/guessdk_lik.R)       |     0.00     |
| [R/lca\_cor.R](../R/lca_cor.R)               |     0.00     |
| [R/lca\_se.R](../R/lca_se.R)                 |     0.00     |
| [R/lca\_subset.R](../R/lca_subset.R)         |     0.00     |
| [R/interleave.R](../R/interleave.R)          |    60.00     |
| [R/multi\_transmat.R](../R/multi_transmat.R) |    66.67     |
| [R/transmat.R](../R/transmat.R)              |    75.00     |
| [R/stnd\_cor.R](../R/stnd_cor.R)             |    78.57     |
| [R/nona.R](../R/nona.R)                      |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat)
package.

| file                                            | n | time | error | failed | skipped | warning | icon |
| :---------------------------------------------- | -: | ---: | ----: | -----: | ------: | ------: | :--- |
| [test-interleave.R](testthat/test-interleave.R) | 1 | 0.02 |     0 |      0 |       0 |       0 |      |
| [test-nona.R](testthat/test-nona.R)             | 1 | 0.00 |     0 |      0 |       0 |       0 |      |
| [test-pkg-style.R](testthat/test-pkg-style.R)   | 1 | 0.00 |     0 |      0 |       1 |       0 | \+   |
| [test-stnd-adj.R](testthat/test-stnd-adj.R)     | 2 | 0.02 |     0 |      1 |       0 |       0 | \!   |
| [test-stnd-cor.R](testthat/test-stnd-cor.R)     | 0 | 0.00 |     1 |      0 |       0 |       0 |      |
| [test-transmat.R](testthat/test-transmat.R)     | 2 | 0.00 |     0 |      1 |       0 |       0 | \!   |

<details open>

<summary> Show Detailed Test Results
</summary>

| file                                                  | context                           | test                                           | status  | n | time | icon |
| :---------------------------------------------------- | :-------------------------------- | :--------------------------------------------- | :------ | -: | ---: | :--- |
| [test-interleave.R](testthat/test-interleave.R#L7_L9) | Test interleave                   | interleave works correctly                     | PASS    | 1 | 0.02 |      |
| [test-nona.R](testthat/test-nona.R#L6)                | Test nona                         | nona works correctly                           | PASS    | 1 | 0.00 |      |
| [test-pkg-style.R](testthat/test-pkg-style.R#L5)      | lints                             | Package Style                                  | SKIPPED | 1 | 0.00 | \+   |
| [test-stnd-adj.R](testthat/test-stnd-adj.R#L12)       | Test Standard Adjustment          | stnd adj works correctly                       | FAILED  | 2 | 0.02 | \!   |
| [test-stnd-cor.R](testthat/test-stnd-cor.R#L10)       | Test Standard Guessing Correction | Standard Guessing Correction Happens Correctly | ERROR   | 0 | 0.00 |      |
| [test-transmat.R](testthat/test-transmat.R#L12)       | Test transmat                     | transmat and multi\_transmat works correctly   | FAILED  | 2 | 0.00 | \!   |

| Failed | Warning | Skipped |
| :----- | :------ | :------ |
| \!     | \-      | \+      |

</details>

<details>

<summary> Session Info </summary>

| Field    | Value                            |
| :------- | :------------------------------- |
| Version  | R version 3.5.1 (2018-07-02)     |
| Platform | x86\_64-w64-mingw32/x64 (64-bit) |
| Running  | Windows 10 x64 (build 17134)     |
| Language | English\_United States           |
| Timezone | America/Los\_Angeles             |

| Package  | Version |
| :------- | :------ |
| testthat | 2.0.1   |
| covr     | 3.2.1   |
| covrpage | 0.0.62  |

</details>

<!--- Final Status : error/failed --->
