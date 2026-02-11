# Interactive UI Tutorial for gcamreport v6.0\*

**ATTENTION**: this tutorial is compatible with `gcamreport v6.0*`, i.e,
with `gcamreport v6.0.0`, `gcamreport v6.0.1`, and
`gcamreport v6.0.0-gas` releases. For other releases, please check in
the *Tutorials* drop-down menu in the main bar.

newline

## Example 1: launch UI when generating the dataset

When running the main function `run` (see [Dataset Generation
Tutorial](https://bc3lc.github.io/gcamreport//articles//Dataset_Generation_Tutorial.html)
for more information), simply set `launch_ui` to `TRUE` or do not
specify anything.

``` r
run(..., launch_ui = TRUE)    # this is the default option
```

newline

## Example 2: launch UI once the dataset has been generated

After generating the dataset with the `run()` function, simply run in R

``` r
launch_gcamreport_ui()
```

newline

## Troubleshooting when launching the UI with the Docker installation

#### A) Wired message when launching the UI when using the Docker installation.

After using the functions `run()` or
[`launch_gcamreport_ui()`](https://bc3lc.github.io/gcamreport/reference/launch_gcamreport_ui.md)
to launch the UI, you might get this message:

    Listening on http://0.0.0.0:3838
    /usr/bin/xdg-open: 882: www-browser: not found
    /usr/bin/xdg-open: 882: links2: not found
    /usr/bin/xdg-open: 882: elinks: not found
    /usr/bin/xdg-open: 882: links: not found
    /usr/bin/xdg-open: 882: lynx: not found
    /usr/bin/xdg-open: 882: w3m: not found
    xdg-open: no method available for opening 'http://127.0.0.1:3838' 

This is not an error! You simply need to either go to your Docker
Desktop program and click the last started port

![UI
error](https://raw.githubusercontent.com/bc3LC/gcamreport/gcam-v6.0/vignettes/readme_fig/shiny_error1.png "Click the last started docker port")

or open this url <http://localhost:4000> in your favourite browser.

#### B) Error when using the UI through Docker installation.

When opening your *localhost*, you might see this error:

![UI
error](https://raw.githubusercontent.com/bc3LC/gcamreport/gcam-v6.0/vignettes/readme_fig/shiny_error2.png "UI error")

**Possible solution**: your UI is not running. Try to either use the
`run()` function or the
[`launch_gcamreport_ui()`](https://bc3lc.github.io/gcamreport/reference/launch_gcamreport_ui.md).
