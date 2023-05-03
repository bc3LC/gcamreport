
  

# gcamreport

  

[![docs](https://github.com/bc3LC/gcamreport/actions/workflows/docs.yaml/badge.svg)](https://github.com/bc3LC/gcamreport/actions/workflows/docs.yaml)

[![pages-build-deployment](https://github.com/bc3LC/gcamreport/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/bc3LC/gcamreport/actions/workflows/pages/pages-build-deployment)

[![test_coverage](https://github.com/bc3LC/gcamreport/actions/workflows/test_coverage.yml/badge.svg)](https://github.com/bc3LC/gcamreport/actions/workflows/test_coverage.yml)

[![codecov](https://codecov.io/gh/bc3LC/gcamreport/branch/gcam-v6.0/graph/badge.svg?token=GHV4F7TGFG)](https://codecov.io/gh/bc3LC/gcamreport)

[![docker](https://github.com/bc3LC/gcamreport/actions/workflows/docker_impl.yaml/badge.svg)](https://github.com/bc3LC/gcamreport/actions/workflows/docker_impl.yaml)

[![build](https://github.com/bc3LC/gcamreport/actions/workflows/build.yaml/badge.svg)](https://github.com/bc3LC/gcamreport/actions/workflows/build.yaml)

  

  

<!-- ------------------------>

  

<!-- ------------------------>

  

## <a name="contents"></a>Contents

  

<!-- ------------------------>

  

<!-- ------------------------>

  


- [Introduction](#introduction)

- [Installation Guide](#installation-guide)

	- [With R](#with-r)

	- [With Docker](#with-docker)

- [ Get Started](#-get-started)

  

  

<!-- ------------------------>

  

<!-- ------------------------>

  

## <a name="introduction"></a>Introduction

  

<!-- ------------------------>

  

<!-- ------------------------>

  
  

[Back to Contents](#Contents)

  
  

`gcamreport` is a tool that generates a consistent dataset from any scenario run by the Global Change Analysis Model ([GCAM](http://www.globalchange.umd.edu/gcam/)), which meets the reporting requirements of the Intergovernmental Panel on Climate Change ([IPCC](https://www.ipcc.ch/)). Additionally, `gcamreport` includes an interactive user widget that allows users to create and download plots, as well as download reduced versions of the dataset in table format.

  

  

<!-- ------------------------>

  

<!-- ------------------------>

  

## <a name="installation-guide"></a>Installation Guide

  

<!-- ------------------------>

  

<!-- ------------------------>

  

[Back to Contents](#Contents)

  

There are two equivalent possiblities to make usage of this package:

  

### <a name="with-R"></a>With R

  

1. Requirements

	- [R](https://www.r-project.org/)

	- [Rstudio](https://www.rstudio.com/)

	- [Git](https://git-scm.com/downloads/)

  

2. Clone this repository

  

```bash
git clone  https://github.com/bc3LC/gcamreport.git
```

3. Open the `gcamreport` folder you just cloned and double-click the `gcamreport.Rproj` file. RStudio should open the project.

```r
install.packages('devtools')
devtools::load_all()
```
 Now `gcamreport` package is fully loaded. Enjoy! :smile:

  
<br>

### <a name="with-Docker"></a>With Docker

  

1. Requirements

	- [Docker](https://docs.docker.com/get-docker/)

	- [Git](https://git-scm.com/downloads)

  
2. Clone this repository

```bash
git clone  https://github.com/bc3LC/gcamreport.git
```
  
3. Load the Docker image: open docker manually (double-click Docker Desktop) and go to the `gcamreport` directory you just cloned to build the docker image.

```bash
cd  /path/to/gcamreport/
docker load --input gcamreport.tar
```
  

4. Run the Docker container: still in the `gcamreport` directory.
```bash
docker run  -v  /path/to/gcamreport:/app  -p  4000:3838  -it  gcamreport
```
This should prompt an R terminal in your console.
  

5. Use `gcamreport` package: in the R console, build the package.

```r
remotes::install_github("bc3LC/gcamreport")
library(gcamreport)
```
**Note**:exclamation:: the local files to be accesses must be inside the `gcamreport` folder, which is considered now as the root direcotry of the R session.  

**Note**:exclamation:: to open the shiny app, either go to Docker Desktop and type the last port started, or type 'http://localhost:4000/#' in the browser.

Now `gcamreport` package is fully loaded. Enjoy! :smile:

  

<br>

  

<!-- ------------------------>

  

<!-- ------------------------>

  

## <a name="get-started"></a> Get Started

  

<!-- ------------------------>

  

<!-- ------------------------>

  
  

[Back to Contents](#Contents)

  
  

The package consists of a set of functions divided into two different blocks:

  

- Dataset generation. Project creation/loading and automatically saved generated dataset. Main function: `run`. For more information look at this [tutorial](https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html).

  

- Interactive user block. Interactive widget to display in table format the dataset. Possibility to filter, reorder, and download. Display and download plots by variables, regions, and scenarios. Main function: `launch_app`. For more information look at this [tutorial](https://bc3lc.github.io/gcamreport/articles/Interactive_UI_Tutorial.html)

  

In addition, the package includes some default input files (.Rda), that are read by the different functions. These can be changed by the user. Some of these constants include:

  

- Energy shares  

- Land shares  

- Other