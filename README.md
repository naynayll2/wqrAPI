
# wqrAPI

<!-- badges: start -->
<!-- badges: end -->

Simple interface for querying robot data from IRVLab servers for Water Quality
Robotics (WQR) project

## Installation

You can install the development version of wqrAPI from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("naynayll2/wqrAPI")
```
## Example

The workflow is as follows: first create a request handler in "test" or "live"
mode (only "test" works right now), then call one of four methods with the handler
as the first argument to get data

``` r
library(wqrAPI)
## basic example code
req <- request_handler("test") # create handler in test mode

names <- get_all_lake_names(req) # you need lake names in order to request info for specific lakes
df <- get_all_lake_data(req)
swan_data <- get_lake_data(req, "swan")
swan_history <- get_lake_history(req, "swan")
```
## Using Data from A Server
Create a variable containing the IP address or http endpoint of the server
```r
url <- "http://12.34.5.6.7"
```
Then supply that to the request handler constructor, along with a `type` of `"live"`
```r
req <- request_handler("live", url)
```
You can then use the request handler as normal!
