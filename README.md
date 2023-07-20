# Analysis of Data from Horiba FluorMax-4 (FM-4)

## Important
Still under development, but feel free to make changes. Excel sheet must be 2 columns, the first titled 'Time' with all time data (preferably in s) and the second titled 'Intensity' with the light intensity data.


## Run in ShinyApps.io
This app may also be run in ShinyApps.io. I will likely not pay for this, so please bear in mind that the app may not be usable at times. For other ways of running the app, see below.

## Run in R/RStudio
Download or copy/paste the code in the repository into your own R/RStudio console. Assuming you have all of the libraries installed (RStudio will prompt you to install the libraries, if not), then the app should run fine.

## Run as Gist

To run as a gist, copy/paste and run the following in R/RStudio:

library(shiny)
`# Gist URL - https://gist.github.com/stdecker/e4db38502091289bf61f837b49b74410`
runGist("e4db38502091289bf61f837b49b74410")
