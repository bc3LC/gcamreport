# gcamreport

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![docs](https://github.com/bc3LC/gcamreport/actions/workflows/docs.yaml/badge.svg)](https://github.com/bc3LC/gcamreport/actions/workflows/docs.yaml)
[![pages-build-deployment](https://github.com/bc3LC/gcamreport/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/bc3LC/gcamreport/actions/workflows/pages/pages-build-deployment)
[![test_coverage](https://github.com/bc3LC/gcamreport/actions/workflows/test_coverage.yml/badge.svg)](https://github.com/bc3LC/gcamreport/actions/workflows/test_coverage.yml)
[![codecov](https://codecov.io/gh/bc3LC/gcamreport/branch/gcam-v7.0/graph/badge.svg?token=GHV4F7TGFG)](https://codecov.io/gh/bc3LC/gcamreport)
[![docker](https://github.com/bc3LC/gcamreport/actions/workflows/docker_impl.yaml/badge.svg)](https://github.com/bc3LC/gcamreport/actions/workflows/docker_impl.yaml)
[![build](https://github.com/bc3LC/gcamreport/actions/workflows/build.yaml/badge.svg)](https://github.com/bc3LC/gcamreport/actions/workflows/build.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13711003.svg)](https://doi.org/10.5281/zenodo.13711003)
[![draft-pdf](https://github.com/bc3LC/gcamreport/actions/workflows/draft-pdf.yml/badge.svg)](https://github.com/bc3LC/gcamreport/actions/workflows/draft-pdf.yml)
[![status](https://joss.theoj.org/papers/816fd8765945cd5f6fe6d8d1fefdde19/status.svg)](https://joss.theoj.org/papers/816fd8765945cd5f6fe6d8d1fefdde19)

  

## Contents

- [Contents](#contents)
- [Introduction](#introduction)
- [Installation Guide](#installation-guide)
  - [With R](#with-r)
    - [Light mode installation](#light-mode-installation)
    - [Full mode installation](#full-mode-installation)
  - [With Docker](#with-docker)
- [Getting Started](#getting-started)
- [How to contribute?](#contribute)
- [Top common Warnings and Error
  Messages](#top-common-warnings-and-error-messages)

  

## Introduction

[Back to Contents](#contents)

`gcamreport` is a tool designed to generate consistent datasets from any
scenario run by the Global Change Analysis Model
([GCAM](http://www.globalchange.umd.edu/gcam/)), ensuring they meet the
reporting standards of the Integrated Assessment Modeling Consortium
([IAMC](https://www.iamconsortium.org/)) defined in the [Common
Definitions](https://github.com/IAMconsortium/common-definitions)
repository. In addition, `gcamreport` features an interactive user
interface that allows users to create and download plots in real time
and export reduced, formatted datasets in spreadsheet format. The tool
is currently compatible with GCAM-core versions
[6.0](https://zenodo.org/records/6619287),
[7.0](https://zenodo.org/records/8010145),
[7.1](https://zenodo.org/records/11481167),
[7.2](https://zenodo.org/records/13946379),
[8.2](https://zenodo.org/records/15581174), and the [ScenarioMIP
project](https://wcrp-cmip.org/mips/scenariomip/); and GCAM-Europe
versions [7.2](https://zenodo.org/records/15655568) and
[8.7](https://github.com/bc3LC-GCAMEurope/gcam-core/releases/tag/gcam-europe-v8.7.0).
Moreover, we support the 2015 and 2021 GCAM base years. Check the
[version
guide](https://bc3lc.github.io/gcamreport/articles/Version_Guide.html)
to see how to run your version! 🚀

  

## Installation Guide

[Back to Contents](#contents)

There are multiple equivalent ways to install this package:

### With R

There are two ways to install the `gcamreport` package through R. The
[light mode installation](#with-R-light-mode-installation) is the
quickest method, as it installs the package directly from GitHub and
only requires R. While suitable for general use, it is incompatible with
the graphical user interface. The [full mode
installation](#with-R-full-mode-installation) requires R, Rstudio and
cloning the GitHub repository. This version supports all package
functions, including the UI, and is the recommended option for those
actively developing or customizing the `gcamreport` package. This last
option is essential if you need to modify mappings or core functions to
ensure compatibility with specific versions of your GCAM model.

#### Light mode installation

1.  Requirements

    - R (to download, click [here](https://www.r-project.org/))

2.  Open R and install the `gcamreport` package:

``` r

install.packages('devtools')
devtools::install_github('bc3LC/gcamreport')
```

Now `gcamreport` package is fully loaded. Enjoy! 😄

  

#### Full mode installation

1.  Requirements

    - R (to download, click [here](https://www.r-project.org/))

    - Rstudio (to download, click [here](https://www.rstudio.com/))

    - Git (to download, click [here](https://git-scm.com/downloads/))

2.  Open git bash in the folder where you want to clone the repository
    and clone it:

``` bash
git clone https://github.com/bc3LC/gcamreport.git
```

3.  Load the `gcamreport` package: Open the `gcamreport` folder you just
    cloned and double-click the `gcamreport.Rproj` file. RStudio should
    open the project. Load the library:

``` r

install.packages('devtools')
devtools::load_all(".", reset = TRUE)
```

Now `gcamreport` package is fully loaded. Enjoy! 😄

  

### With Docker

This installation method allows you not to worry about the R libraries
and dependencies. Docker provides you with an already updated
environment suitable for running the `gcamreport` package.

1.  Requirements

    - Docker Desktop (to download, click
      [here](https://docs.docker.com/get-docker/))

    - Git (to download, click [here](https://git-scm.com/downloads))

2.  Open git bash in the folder where you want to clone the repository
    and clone it:

``` bash
git clone https://github.com/bc3LC/gcamreport.git
```

3.  Open Docker Desktop (double-click the icon on your computer) and
    leave it running in the background.

4.  Inside a terminal (bash or cmd) pull the docker image:

``` bash
docker pull claudiarodes/gcamreport_docker:gcam-v7.0-v2
```

**Note**❗: This step requires 13.5GB of free space in your computer.

5.  Run the Docker container using your full path to the `gcamreport`
    folder:

``` bash
docker run -v /path/to/gcamreport:/app -p 4000:3838 -it claudiarodes/gcamreport_docker:gcam-v7.0-v2
```

This should prompt an R console in your terminal.

6.  Install the `gcamreport` package in the new R console:

``` r

remotes::install_github("bc3LC/gcamreport") #you can skip all updates in case you are asked
library(gcamreport)
```

Now `gcamreport` package is fully loaded. Enjoy! 😄

**Note**❗: To access local files, you should place them in the
`gcamreport` folder, which is now considered the root of the R session.
Inside the R session it is referred to as `/app`.

**Note**❗: To reuse the docker image, you can simply perform steps 3,
5, and 6, since the docker image is already on your computer.

**Note**❗: If you followed the [Docker installation](#with-Docker), to
open the user interface (UI) once it has been launched, either go to the
Docker Desktop and type the last port started, or type
<http://localhost:4000> in your browser.

![UI
error](https://raw.githubusercontent.com/bc3LC/gcamreport/gcam-core/vignettes/readme_fig/shiny_error1.png "Click the last started docker port")

  

## Getting Started

[Back to Contents](#contents)

The `gcamreport` package consists of a set of functions divided into two
different blocks:

- *Dataset generation*: It creates or loads a GCAM project and
  automatically saves the generated dataset that meets the reporting
  requirements of the [IAMC](https://www.iamconsortium.org/) —following
  the naming conventions, definitions, and units established by the
  [Common
  Definitions](https://github.com/IAMconsortium/common-definitions)
  repository. The main function is
  [`generate_report()`](https://bc3lc.github.io/gcamreport/reference/generate_report.md).
  For more information, see this
  [tutorial](https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html)
  or type `??generate_report` in your R console. If you get any warning
  or error messages, you might want to look at the [Warnings and Error
  Messages](#bugs) section.

- *Interactive user block*: it launches an interactive widget that
  displays the dataset in tabular form, with the ability to filter,
  reorder and download live. It also displays plots and allows them to
  be downloaded, aggregated by variables, regions and scenarios. The
  main function is
  [`launch_gcamreport_ui()`](https://bc3lc.github.io/gcamreport/reference/launch_gcamreport_ui.md).
  For more information see this
  [tutorial](https://bc3lc.github.io/gcamreport/articles/Interactive_UI_Tutorial.html)
  or type `??launch_gcamreport_ui` in your R console. If you get any
  warning or error messages, it might be useful to have a look at the
  [Top common Warnings and Error Messages](#bugs) section.

The package also includes some default input files (.Rda) that are read
by the different functions. These can be changed by the user as detailed
in [this
tutorial](https://bc3lc.github.io/gcamreport/articles/Modify_Mapping_Template_Tutorial.html).

  

## How to contribute?

[Back to Contents](#contents)

You are welcome to contribute to this project! Follow the steps below to
facilitate the implementation:

1.  Fork this repository.
2.  Commit your modifications.
3.  Open a [Pull Request
    (PR)](https://github.com/bc3LC/gcamreport/pulls) against the
    [gcam-core](https://github.com/bc3LC/gcamreport/tree/gcam-core)
    (main) target branch. Clearly describe the purpose of your
    modifications and outline the specific changes made. Ensure there
    are no merge conflicts and that all automated tests pass
    successfully.
4.  Set [@klau506](https://github.com/klau506) as reviewers (or include
    this mention in the PR requested text).
5.  Once everything is tested, we will merge the PR for you.

**Note**: in case of integrating a new GCAM version into `gcamreport`,
ensure you place a small/dummy project file under
`tests/testthat/testInputs/[GCAM_VERSION]/` folder. This allows to test
and validate the new compatibility.

  

## Top common Warnings and Error Messages

[Back to Contents](#contents)

Some typical and already-known errors that can be easily solved! 💡

💻 Error on “generate_report(prj_name =”path/to/your/data/myData.dat”)”

In your R console, you might see this error:

``` R
  > generate_report("path/to/your/data/myData.dat")
  Loading project...
  Loading data, performing checks, and saving output...
  
  [1] "ag_demand_clean"
  Error in rgcam::getQuery(prj, "demand balances by crop commodity") :
    getQuery: Query demand balances by crop commodity is not in any scenarios in the data set.
```

**Possible solution**

This problem is due to a wrong path specification. Thus, make sure that
you specified correctly the path. In addition:

- In case you are using `gcamreport` package following the [R
  installation](#with-r), try to copy the whole path to your data, for
  instance `C:\Users\username\Documents\path\to\your\data\myData.dat` if
  you are using a Windows distribution.

- In case you are using `gcamreport` package following the [Docker
  installation](#with-docker):

  1.  make sure that your data is inside the `gcamreport` folder.

  2.  make sure that you type correctly the path to your `gcamreport`
      folder when running the docker image (5th step in the [Docker
      section](#with-docker))

  3.  make sure that you are pointing correctly to your data. For
      example, if in the `gcamreport` folder you have a folder called
      `amazingData` with your dataset `myData.dat`, you should refer to
      it as

``` r

  # option 1: full path
  generate_report("/app/amazingData/myData.dat")
  
  # option 2: partial path
  generate_report("amazingData/myData.dat")
```

  

💻 Error on “generate_report(…)”

In your R console, you might see this error:

``` R
  > generate_report(...)
  Loading project...
  Loading data, performing checks, and saving output...
  [1] "ag_demand_clean"

  Error in left_join_strict(., filter_variables(get(paste("ag_demand_map", :
    Error: Some rows in the left dataset do not have matching keys in the right dataset.
```

**Possible solution**

This problem is due to a mismatch in the `ag_demand_map` map. Thus, make
sure that you specified correctly the `GCAM_verions` parameter in the
`generate_report` function. If the error persists, have a look at this
[tutorial](https://bc3lc.github.io/gcamreport/articles/Modify_Mapping_Template_Tutorial.html#example-1-step-by-step-to-adapt-current-mappings-to-your-GCAM-version).

  

💻 Wired message when launching the UI when using the Docker
installation.

After using the functions
[`generate_report()`](https://bc3lc.github.io/gcamreport/reference/generate_report.md)
or
[`launch_gcamreport_ui()`](https://bc3lc.github.io/gcamreport/reference/launch_gcamreport_ui.md)
to launch the UI, you might get this message:

``` R
  Listening on http://0.0.0.0:3838
  /usr/bin/xdg-open: 882: www-browser: not found
  /usr/bin/xdg-open: 882: links2: not found
  /usr/bin/xdg-open: 882: elinks: not found
  /usr/bin/xdg-open: 882: links: not found
  /usr/bin/xdg-open: 882: lynx: not found
  /usr/bin/xdg-open: 882: w3m: not found
  xdg-open: no method available for opening 'http://127.0.0.1:3838' 
```

**Possible solution**

This is not an error! You simply need to either go to your Docker
Desktop program and click the last started port

![UI
error](https://raw.githubusercontent.com/bc3LC/gcamreport/gcam-core/vignettes/readme_fig/shiny_error1.png "Click the last started docker port")

or open this url <http://localhost:4000> in your favourite browser.

  

💻 Error when using the UI through Docker installation.

When oppening your *localhost*, you might see this error:

![UI
error](https://raw.githubusercontent.com/bc3LC/gcamreport/gcam-core/vignettes/readme_fig/shiny_error2.png "UI error")

**Possible solution**

Your UI is not running. Try to either use the
[`generate_report()`](https://bc3lc.github.io/gcamreport/reference/generate_report.md)
function or the
[`launch_gcamreport_ui()`](https://bc3lc.github.io/gcamreport/reference/launch_gcamreport_ui.md).

  

💻 Error related to *system* when using the Docker installation.

Once the R console is opened, you might see this message after
introducing any command:

``` R
  System has not been booted with systemd as init system (PID 1). Can't operate.
  Failed to connect to bus: Host is down
  Warning message:
  In system("timedatectl", intern = TRUE) :
     running command 'timedatectl' had status 1 
```

**Possible solution**

Simply type `Ctrl+C` and run your command again.

  

**Note**:💥 For other errors, please check the troubleshooting sections
of the [`generate_report`
function](https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#troubleshooting-for-the-generate_report-function)
or the [user interface
widget](https://bc3lc.github.io/gcamreport/articles/Interactive_UI_Tutorial.html#troubleshooting-when-launching-the-ui).
If your error is not listed, please open an
[Issue](https://github.com/bc3LC/gcamreport/issues) on the GitHub page
with all the information to reproduce the crash.
