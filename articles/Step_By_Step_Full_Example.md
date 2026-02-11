# Step-by-step full example

In this tutorial, we will 1) generate a dataset from a provided GCAM
database and launch the associated user interface, 2) generate a dataset
from a provided project and launch the corresponding user interface, and
3) launch the user interface from a provided standardized dataset. To
know more about the `gcamreport` package possibilities, look at these
tutorials: [dataset generation
tutorial](https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html)
and [user interface
tutorial](https://bc3lc.github.io/gcamreport/articles/Interactive_UI_Tutorial.html).

newline

## Example 1: step-by-step standardized dataset generation from a provided database

1.  Follow the [installation
    guide](https://bc3lc.github.io/gcamreport/index.html#installation-guide)
    either with R or Docker.

2.  Download the example [GCAM7.1
    database](https://zenodo.org/records/13361605), store it inside
    `gcamreport/examples`, and unpack it. Make sure that the database is
    directly located in `gcamreport/examples/database_basexdb_ref` and
    an intermediate folder has not appeared.

3.  Load the `gcamreport` library. If you are using Rstudio or Docker,
    run

``` r
devtools::load_all(".", reset = TRUE)
```

and if you are using R, run

``` r
library(gcamreport)
```

4.  Generate the standardized dataset:

``` r
## -- store the database path, name, and scenarios in a variable.
dbpath <- "examples"
dbname <- "database_basexdb_ref"
scen <- "Reference"
GCAMv <- "v7.1"

## -- choose a project name
prjname <- "example1_v7.1.dat"

## -- generate the reporting dataset until 2050 for EU-12 and EU-15 for all the 
## -- Agricultural variables, save the output in .RData, .csv and .xlsx format, 
## -- and lunch the user interface
generate_report(db_path = dbpath, db_name = dbname, scenarios = scen, 
                prj_name = prjname, final_year = 2050, GCAM_version = GCAMv,
                desired_regions = c('EU-12', 'EU-15'), 
                desired_variables = c('Agricultural*'), 
                save_output = TRUE, launch_ui = TRUE)
```

In case you experience some trouble, check this [troubleshooting
section](https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#troubleshooting-for-the-generate_report-function).

5.  Once the project has been generated and the standardized dataset
    created, the user interface will prompt. If you are using Rstudio,
    it will automatically open. For a better experience, click the
    `open in browser` button. If you are using Docker, either type
    <http://localhost:4000> in the browser or go to Docker Desktop and
    click the last started port.

![UI
error](https://raw.githubusercontent.com/bc3LC/gcamreport/gcam-core/vignettes/readme_fig/shiny_error1.png "Click the last started docker port")

**Note**: Remember that the user interface can only be launched through
the [full R
installation](https://bc3lc.github.io/gcamreport/index.html#with-r-full-mode-installation)
or the [Docker
installation](https://bc3lc.github.io/gcamreport/index.html#with-docker).

6.  Check the generated dataset: look in the `examples` folder and you
    should see two files named `example1_v7.1_standardized.csv` and
    `example1_v7.1_standardized.xlsx` containing the standardized
    dataset.

7.  Note that a file called `database_basexdb_ref_example1_v7.1.dat` has
    appeared in the `examples` folder. This is the generated project
    file from the provided database. If you want to rebuild the
    standardized dataset or launch the user interface, you can use this
    file directly and avoid rebuilding the project. If you want to
    proceed as mentioned, you can follow [Example 2](#example2).

8.  Note that a file named `example1_v7.1_standardized.RData` has
    appeared in the `examples` folder. This is the report file generated
    from the provided database. If you want to launch the user interface
    in the future, you can use this file directly and avoid creating or
    loading the project again. If you want to proceed as mentioned, you
    can follow [Example 3](#example3).

**Note**: You can do this step-by-step example using the provided
[GCAM7.0 database](https://zenodo.org/records/10258919) or your own
database! Remember to indicate the `GCAM_version` when generating the
report ;)

newline

## Example 2: step-by-step standardized dataset generation from a provided project

1.  Follow the [installation
    guide](https://bc3lc.github.io/gcamreport/index.html#installation-guide)
    either with R or Docker

2.  In this example we will use an example rgcam project stored in
    `examples` called `example2_v7.1.dat`

3.  Load the `gcamreport` library. If you are using Rstudio or Docker,
    run

``` r
devtools::load_all(".", reset = TRUE)
```

and if you are using R, run

``` r
library(gcamreport)
```

4.  Generate the standardized dataset:

``` r
## -- store the project path and name in a variable.
prjname <- "examples/example2_v7.1.dat"

## -- generate the reporting dataset until 2050 for EU-12 and EU-15 for all the 
## -- Agricultural variables, save the output in .RData, .csv and .xlsx format, 
## -- and lunch the user interface
generate_report(prj_name = prjname, final_year = 2050, 
                GCAM_version = 'v7.1',
                desired_regions = c('EU-12', 'EU-15'),
                desired_variables = c('Agricultural*'), 
                save_output = TRUE, launch_ui = TRUE)
```

In case you experience some trouble, check this [troubleshooting
section](https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#troubleshooting-for-the-generate_report-function).

5.  Once the project has been generated and the standardized dataset
    created, the user interface will prompt. If you are using Rstudio,
    it will automatically open. For a better experience, click the
    `open in browser` button. If you are using Docker, either type
    <http://localhost:4000> in the browser or go to Docker Desktop and
    click the last started port.

![UI
error](https://raw.githubusercontent.com/bc3LC/gcamreport/gcam-core/vignettes/readme_fig/shiny_error1.png "Click the last started docker port")

**Note**: Remember that the user interface can only be launched through
the [full R
installation](https://bc3lc.github.io/gcamreport/index.html#with-r-full-mode-installation)
or the [Docker
installation](https://bc3lc.github.io/gcamreport/index.html#with-docker).

6.  Check the generated dataset: look in the `examples` folder and you
    should see two files named `example2_v7.1_standardized.csv` and
    `example2_standardized.xlsx` containing the standardized dataset.

7.  Note that a file called `example2_v7.1_standardized.RData` has
    appeared in the `examples` folder. This is the report file generated
    from the provided database. If you want to launch the user interface
    in the future, you can use this file directly and avoid creating or
    loading the project again. If you want to proceed as mentioned, you
    can follow [Example 3](#example3).

**Note**: You can do this step-by-step example using the provided
`example2_v7.0.dat` project or any project generated through your own
database! Remember to indicate the `GCAM_version` when generating the
report ;)

newline

## Example 3: step-by-step user interface launching from a provided standardized dataset

**Note**: Remember that the user interface can only be launched through
the [full R
installation](https://bc3lc.github.io/gcamreport/index.html#with-r-full-mode-installation)
or the [Docker
installation](https://bc3lc.github.io/gcamreport/index.html#with-docker).

1.  Follow the [installation
    guide](https://bc3lc.github.io/gcamreport/index.html#installation-guide)
    either with the R full installation or the Docker installation.

2.  In this example we will use a standardized example dataset stored in
    `examples` called `example3_v7.1.RData`.

3.  Load the `gcamreport` library:

``` r
devtools::load_all(".", reset = TRUE)
```

4.  Launch the user interface for the standardized dataset:

``` r
## -- load gcamreport library.
devtools::load_all(".", reset = TRUE) # if using Rstudio or Docker
library(gcamreport) # if using R

## -- store the project path and name in a variable.
datapath <- "examples/example3_v7.1.RData"
GCAMv <- "v7.1"

## -- launch the user interface
launch_gcamreport_ui(data_path = datapath, GCAM_version = GCAMv)
```

In case you experience some trouble, check this [troubleshooting
section](https://bc3lc.github.io/gcamreport/articles/Interactive_UI_Tutorial.html#troubleshooting-when-launching-the-ui-with-the-docker-installation).

5.  Once the project has been generated and the standardized dataset
    created, the user interface will prompt. If you are using Rstudio,
    it will automatically open. For a better experience, click the
    `open in browser` button. If you are using Docker, either type
    <http://localhost:4000> in the browser or go to Docker Desktop and
    click the last started port.

![UI
error](https://raw.githubusercontent.com/bc3LC/gcamreport/gcam-core/vignettes/readme_fig/shiny_error1.png "Click the last started docker port")

6.  If you just ran [Example 1](#example1) or [Example 2](#example2),
    you should have a variable called `report` in the environment. You
    can also use it to launch the user interface:

``` r
## -- load gcamreport library.
devtools::load_all(".", reset = TRUE) # if using Rstudio or Docker
library(gcamreport) # if using R

## -- store the database path, name, and scenarios in a variable.
dataname <- "report"
GCAMv <- "v7.1"

## -- launch the user interface
launch_gcamreport_ui(data = dataname, GCAM_version = GCAMv)
```

**Note**: You can do this step-by-step example using the provided
`example3_v7.0.RData` file or any file generated through your own
database! Remember to indicate the `GCAM_version` when generating the
report ;)
