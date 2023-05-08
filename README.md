


# gcamreport
  

[![docs](https://github.com/bc3LC/gcamreport/actions/workflows/docs.yaml/badge.svg?branch=gcam-v6.0)](https://github.com/bc3LC/gcamreport/actions/workflows/docs.yaml)
[![pages-build-deployment](https://github.com/bc3LC/gcamreport/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/bc3LC/gcamreport/actions/workflows/pages/pages-build-deployment)
[![test_coverage](https://github.com/bc3LC/gcamreport/actions/workflows/test_coverage.yml/badge.svg?branch=gcam-v6.0)](https://github.com/bc3LC/gcamreport/actions/workflows/test_coverage.yml)
[![codecov](https://codecov.io/gh/bc3LC/gcamreport/branch/gcam-v6.0/graph/badge.svg?token=GHV4F7TGFG)](https://codecov.io/gh/bc3LC/gcamreport)
[![docker](https://github.com/bc3LC/gcamreport/actions/workflows/docker_impl.yaml/badge.svg?branch=gcam-v6.0)](https://github.com/bc3LC/gcamreport/actions/workflows/docker_impl.yaml)
[![build](https://github.com/bc3LC/gcamreport/actions/workflows/build.yaml/badge.svg?branch=gcam-v6.0)](https://github.com/bc3LC/gcamreport/actions/workflows/build.yaml)
  

<!-- ------------------------>
<!-- ------------------------>
## <a name="contents"></a>Contents
<!-- ------------------------>
<!-- ------------------------>

  
- [Contents](#contents)

- [Introduction](#introduction)

- [Installation Guide](#installation-guide)

	- [With R](#with-r)

	- [With Docker](#with-docker)

- [Getting Started](#get-started)

- [Warning and Error Messages](#bugs)

  

  

<!-- ------------------------>
<!-- ------------------------>
## <a name="introduction"></a>Introduction
<!-- ------------------------>
<!-- ------------------------>  

[Back to Contents](#contents)
   

`gcamreport` is a tool that generates a consistent dataset from any scenario run by the Global Change Analysis Model ([GCAM](http://www.globalchange.umd.edu/gcam/)), which meets the reporting requirements of the Intergovernmental Panel on Climate Change ([IPCC](https://www.ipcc.ch/)). Additionally, `gcamreport` includes an interactive user widget that allows users to create and download plots, as well as download reduced versions of the dataset in table format.

  
<!-- ------------------------>
<!-- ------------------------>  
## <a name="installation-guide"></a>Installation Guide
<!-- ------------------------>
<!-- ------------------------>  

[Back to Contents](#contents)
  

There are two equivalent possibilities to make use of this package:

### <a name="with-R"></a>With R

1. Requirements

	- R (to download clik [here](https://www.r-project.org/))

	- Rstudio (to download clik [here](https://www.rstudio.com/))

	- Git (to download clik [here](https://git-scm.com/downloads/))


2. Clone this repository:  

```bash
git clone  https://github.com/bc3LC/gcamreport.git
```

3. Open the `gcamreport` folder you just cloned and double-click the `gcamreport.Rproj` file. RStudio should open the project. Load the library:

```r
install.packages('devtools')
devtools::load_all()
```
 Now `gcamreport` package is fully loaded. Enjoy! :smile:

  
<br>

### <a name="with-Docker"></a>With Docker

  

1. Requirements

	- Docker (to download click [here](https://docs.docker.com/get-docker/))

	- Git (to download click [here](https://git-scm.com/downloads))

  
2. Open git bash in the folder where you want to clone the repository and clone it:

```bash
git clone https://github.com/bc3LC/gcamreport.git
```

3. Open Docker Desktop (double click the icon on your computer) and leave it running in the background.

4. Inside a terminal (bash or cmd) pull the docker image:

```bash
docker pull claudiarodes/gcamreport_docker:v1
```
**Note**:exclamation:: this step requires 13.5GB of free space in your computer.  

5. Run the Docker container using your path to the `gcamreport` folder: 
```bash
docker run -v /path/to/gcamreport:/app -p 4000:3838 -it claudiarodes/gcamreport_docker:v1
```
This should prompt an R terminal in your console.
  

6. Install the `gcamreport` package in the new R console:

```r
remotes::install_github("bc3LC/gcamreport") #you can skip all updates in case you are asked
library(gcamreport)
```
**Note**:exclamation:: the local files to be accessed must be inside the `gcamreport` folder, which is considered now as the root directory of the R session. Inside the R session, it is referred to as `/app`. 

**Note**:exclamation:: to use again the docker image, you can simply run steps 3, 5 and 6, since the docker image is already on your computer.

Now `gcamreport` package is fully loaded. Enjoy! :smile:

  
<br>

  
<!-- ------------------------>
<!-- ------------------------> 
## <a name="get-started"></a>Getting Started
<!-- ------------------------>
<!-- ------------------------>

[Back to Contents](#contents)
  

The `gcamreport` package consists of a set of functions divided into two different blocks:


- Dataset generation: it creates or loads an existing project and automatically saves the generated dataset that meets the reporting requirements of [IPCC](https://www.ipcc.ch/). Main function: `run()`. For more information look at this [tutorial](https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html) or type `?run` in your R consolse. If you obtain some warning or error messages, it might be useful to look at the [Warning and Error Messages](#bugs) section.


- Interactive user block: it launches an interactive widget to display in table format the dataset, with the possibility to filter, reorder, and download live. Moreover, it displays plots and allows them to be downloaded, aggregated by variables, regions, and scenarios. Main function: `launch_gcamreport_app()`. For more information look at this [tutorial](https://bc3lc.github.io/gcamreport/articles/Interactive_UI_Tutorial.html) or type `?launch_gcamreport_app` in your R console. If you obtain some warning or error messages, it might be useful to look at the [Warning and Error Messages](#bugs) section.

**Note**:exclamation:: if you followed the [Docker installation](#with-Docker), to open the user interface (UI) once it has been lunched, either go to Docker Desktop and type the last started port, or type http://localhost:4000 in the browser.  

<img src="https://github.com/bc3LC/gcamreport/blob/gcam-v6.0/vignettes/readme_fig/shiny_error1.png" alt="UI error" width = "60%" height = "60%" title = "Click the last started docker port">

In addition, the package includes some default input files (.Rda), that are read by the different functions. These can be changed by the user. Some of these constants include energy shares, land shares, and others.


<br>

<!-- ------------------------>
<!-- ------------------------>
## <a name="bugs"></a>Warning and Error Messages
<!-- ------------------------>
<!-- ------------------------>  
  
[Back to Contents](#contents)


Some typical and already-known errors that can be easily solved! :bulb:

<details>
<summary>:computer: Error on "run("path/to/your/data/myData.dat")"

In your R console, you might see this error:

```
> run("path/to/your/data/myData.dat")
[1] "Loading project..."
[1] "Loading data, performing checks, and saving output..."
[1] "ag_demand_clean"
Error in rgcam::getQuery(prj, "demand balances by crop commodity") :
  getQuery: Query demand balances by crop commodity is not in any scenarios in the data set.
```
</summary>

This problem is due to a wrong path specification.

**Possible solution**: make sure that you specified correctly the path. In addition:

 - In case you are using `gcamreport` package following the [R installation](#with-r), try to copy the whole path to your data, for instance `C:\Users\username\Documents\path\to\your\data\myData.dat` if you are using a Windows distribution.
 
 - In case you are using `gcamreport` package following the [Docker installation](#with-docker):
 
 	 a) make sure that your data is inside the `gcamreport` folder.
	 
 	 b) make sure that you type correctly the path to your `gcamreport` folder when running the docker image (5th step in the [Docker section](#with-docker))
	 
 	 c) make sure that you are pointing correctly to your data. For example, if in the `gcamreport` folder you have a folder called `amazingData` with your dataset `myData.dat`, you should refer to it as
	 
```r
# option 1: full path
run("/app/amazingData/myData.dat")

# option 2: partial path
run("amazingData/myData.dat")
```
</details>
<br>

<details>
<summary>:computer: Wired message when launching the UI when using the Docker installation.

After using the functions `run()` or `launch_gcamreport_app()` to launch the UI, you might get this message:

```
Listening on http://0.0.0.0:3838
/usr/bin/xdg-open: 882: www-browser: not found
/usr/bin/xdg-open: 882: links2: not found
/usr/bin/xdg-open: 882: elinks: not found
/usr/bin/xdg-open: 882: links: not found
/usr/bin/xdg-open: 882: lynx: not found
/usr/bin/xdg-open: 882: w3m: not found
xdg-open: no method available for opening 'http://127.0.0.1:3838' 
```
</summary>

This is not an error! You simply need to either go to your Docker Desktop program and click the last started port

<img src="https://github.com/bc3LC/gcamreport/blob/gcam-v6.0/vignettes/readme_fig/shiny_error1.png" alt="UI error" width = "50%" height = "50%" title = "Click the last started docker port">

or open this url http://localhost:4000 in your favourite browser.
</details>
<br>

<details>
<summary> :computer: Error when using the UI through Docker installation.

When oppening your *localhost*, you might see this error:

<img src="https://github.com/bc3LC/gcamreport/blob/gcam-v6.0/vignettes/readme_fig/shiny_error2.png" alt="UI error" width = "25%" height = "25%" title = "UI error">
</summary>

**Possible solution**: your app is not running. Try to either use the `run()` function or the `launch_app_function()`.

</details>

<br>

<details>
<summary>:computer: Error related to *system* when using the Docker installation.

Once the R console is opened, you might see this message after introducing any command:

```
System has not been booted with systemd as init system (PID 1). Can't operate.
Failed to connect to bus: Host is down
Warning message:
In system("timedatectl", intern = TRUE) :
   running command 'timedatectl' had status 1 
```
</summary>

**Possible solution**: simply type `Ctrl+C` and run your command again.

</details>
