
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DS4P

<!-- badges: start -->

[![status](https://joss.theoj.org/papers/ee3a025be4f61584f977a7657d936187/status.svg)](https://joss.theoj.org/papers/10.21105/joss.06203)
</br> [![Project Status: Active – The project has reached a stable,
usable state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R package
version](https://www.r-pkg.org/badges/version/DS4P)](https://cran.r-project.org/package=DS4P)
[![Package
downloads](https://cranlogs.r-pkg.org/badges/grand-total/DS4P)](https://cran.r-project.org/package=DS4P)</br>
[![R-CMD-check](https://github.com/DataScience4Psych/package/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DataScience4Psych/package/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/DataScience4Psych/package/graph/badge.svg?token=2IARK2XSA6)](https://codecov.io/gh/DataScience4Psych/package)
![License](https://img.shields.io/badge/License-GPL_v3-blue.svg)
<!-- badges: end -->

The DS4P R package offers a comprehensive suite of functions tailored
for extended behavior genetics analysis, including model identification,
calculating relatedness, pedigree conversion, pedigree simulation, and
more.

## Installation

You can install the released version of DS4P from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("DS4P")
```

To install the development version of DS4P from
[GitHub](https://github.com/) use:

``` r
# install.packages("devtools")
devtools::install_github("DataScience4Psych/package")
```

## Citation

If you use DS4P in your research or wish to refer to it, please cite the
following paper:

    citation(package = "DS4P")

    Warning in citation(package = "DS4P"): could not determine year for 'DS4P' from
    package DESCRIPTION file

Garrison S (????). *DS4P: Data Science for Psychologists*. R package
version 0.1.0.

A BibTeX entry for LaTeX users is

    Warning in citation(package = "DS4P"): could not determine year for 'DS4P' from
    package DESCRIPTION file
    @Manual{,
      title = {DS4P: Data Science for Psychologists},
      author = {S. Mason Garrison},
      note = {R package version 0.1.0},
    }

## Contributing

Contributions to the DS4P project are welcome. For guidelines on how to
contribute, please refer to the [Contributing
Guidelines](https://github.com/DataScience4Psych/package/blob/main/CONTRIBUTING.md).
Issues and pull requests should be submitted on the GitHub repository.
For support, please use the GitHub issues page.

### Branching and Versioning System

The development of DS4P follows a [GitFlow branching
strategy](https://tilburgsciencehub.com/topics/automation/version-control/advanced-git/git-branching-strategies/):

- **Feature Branches**: All major changes and new features should be
  developed on separate branches created from the dev_main branch. Name
  these branches according to the feature or change they are meant to
  address.
- **Development Branches**: Our approach includes two development
  branches, each serving distinct roles:
  - **`dev_main`**: This branch is the final integration stage before
    changes are merged into the `main` branch. It is considered stable,
    and only well-tested features and updates that are ready for the
    next release cycle are merged here.
  - **`dev`**: This branch serves as a less stable, active development
    environment. Feature branches are merged here. Changes here are more
    fluid and this branch is at a higher risk of breaking.
- **Main Branch** (`main`): The main branch mirrors the stable state of
  the project as seen on CRAN. Only fully tested and approved changes
  from the dev_main branch are merged into main to prepare for a new
  release.

## License

DS4P is licensed under the GNU General Public License v3.0. For more
details, see the
[LICENSE.md](https://github.com/DataScience4Psych/package/blob/main/LICENSE.md)
file.
