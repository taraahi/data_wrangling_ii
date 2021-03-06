Reading from Web
================
Tara
10/25/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)


knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.clour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

sclae_colour_discrete = scale_color_viridis_d()
scale_fill_discrete = scale_fill_viridis_d
```

## NSDUH Data

``` r
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"
drug_use_html = read_html(url)

drug_use_df = 
  drug_use_html %>% 
  html_table() %>% 
  first() %>% 
  slice(-1)
```

This gets the tables from the web page^ can get first table. This table
has a footnote that’s being put into the first row, so we slice out the
first row. We can also make it a dataframe by putting df =

## Star Wars

Getting some star wars data…

``` r
sw_html = 
  read_html("https://www.imdb.com/list/ls070150896/")

sw_titles = 
  sw_html %>% 
  html_elements(".lister-item-header a") %>% 
  html_text()

sw_revenue = 
  sw_html %>% 
  html_elements(".text-small:nth-child(7) span:nth-child(5)") %>% 
  html_text()

sw_df = 
  tibble(
    title = sw_titles,
    revenue = sw_revenue
  )
```

Click SelectorGadget in your bookmarks bar and select the things you
want, click again to deselect things you don’t want. Copy/paste CCS
Selector (lister-item-header a) into code.

\#\#Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

dynamite_review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

dynamite_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

dynamite_df = 
  tibble(
    titles = dynamite_review_titles,
    stars = dynamite_stars
  )
```

**ERROR: getting characters(0) with napoleon dynamite example**

## Try some APIs

We’re just getting data, not HTML stuff.

``` r
water_df = 
  GET("https://data.cityofnewyork.us/resource/ia2d-e54m.csv") %>% 
  content()
```

    ## Rows: 42 Columns: 4

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (4): year, new_york_city_population, nyc_consumption_million_gallons_per...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Let’s see what JSON looks like, gives same thing.

``` r
water_df = 
  GET("https://data.cityofnewyork.us/resource/ia2d-e54m.json") %>% 
  content("text") %>% 
  jsonlite::fromJSON() %>% 
  as_tibble()
```

BRFSS Data via API

``` r
brfss_smart2010 = 
  GET("https://chronicdata.cdc.gov/resource/acme-vg9e.csv",
      query = list("$limit" = 5000)) %>% 
  content("parsed")
```

    ## Rows: 5000 Columns: 23

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (16): locationabbr, locationdesc, class, topic, question, response, data...
    ## dbl  (6): year, sample_size, data_value, confidence_limit_low, confidence_li...
    ## lgl  (1): locationid

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

You add the `query` because there’s a limit of 1000 rows at a time.
`parsed` and `text` give you a certain formatting for `content`.

### Pokemon data

``` r
poke = 
  GET("http://pokeapi.co/api/v2/pokemon/1") %>%
  content()

poke$name
```

    ## [1] "bulbasaur"
