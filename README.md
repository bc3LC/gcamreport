
# gcamreport

  

<!-- ------------------------>

<!-- ------------------------>

## <a name="Contents"></a>Contents

<!-- ------------------------>

<!-- ------------------------>

  

- [Introduction](#Introduction)

- [Installation Guide](#InstallGuide)

- [Get Started](#GetStarted)

  

<!-- ------------------------>

<!-- ------------------------>

## <a name="Introduction"></a>Introduction

<!-- ------------------------>

<!-- ------------------------>

  

[Back to Contents](#Contents)

  

`gcamreport` is a tool that generates a consistent dataset from any scenario run by the Global Change Analysis Model ([GCAM](http://www.globalchange.umd.edu/gcam/)), which meets the reporting requirements of the Intergovernmental Panel on Climate Change ([IPCC](https://www.ipcc.ch/)). Additionally, `gcamreport` includes an interactive user widget that allows users to create and download plots, as well as download reduced versions of the dataset in table format.

  

<!-- ------------------------>

<!-- ------------------------>

## <a name="InstallGuide"></a>Installation Guide

<!-- ------------------------>

<!-- ------------------------>

  

[Back to Contents](#Contents)

To be done

  

<!-- ------------------------>

<!-- ------------------------>

## <a name="GetStarted"></a> Get Started

<!-- ------------------------>

<!-- ------------------------>

  

[Back to Contents](#Contents)

  

The package consists of a set of functions divided into two different blocks:

- Dataset generation. Project creation/loading and automatically saved generated dataset. Main function: `run`.

- Interactive user block. Interactive widget to display in table format the dataset. Possibility to filter, reorder, and download. Display and download plots by variables, regions, and scenarios. Main function: `launch_app`.
  

In addition, the package includes some default input files (.Rda), that are read by the different functions. These can be changed by the user. Some of these constants include:

- Energy shares

- Land shares

- Other
