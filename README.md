# shinyhanzi

shinyhanzi is an interactive Shiny application for exploring Chinese characters (汉字). It provides animated stroke order, a type-aware breakdown of each character into its meaningful components (semantic and phonetic parts, with etymology), dictionary lookup with pinyin, English-to-Hanzi search, and frequency analysis. The package also exposes its underlying lookup functions so you can use them in your own R code.

## Installation

``` r
# Install the development version from GitHub:
pak::pak("dylanpieper/shinyhanzi")
```

## Getting started

shinyhanzi requires a database (99 MB) that is downloaded separately. Download it once and it will be cached for all future sessions:

``` r
library(shinyhanzi)

download_hanzi_db()
run_app()
```

## Helper functions

Beyond the app, shinyhanzi exposes several functions for working with Chinese characters in R.

### Look up a character

``` r
result <- hanzi_lookup("好")

result$mmah$definition
#> [1] "good, excellent, fine; proper, suitable; well"

result$cedict
#> # A tibble: 2 × 5
#>   traditional pinyin_toned pinyin_numbered gloss                         is_word
#>   <chr>       <chr>        <chr>           <chr>                         <lgl>
#> 1 好          hǎo          hao3            good / appropriate; proper /… FALSE
#> 2 好          hào          hao4            to be fond of; to have a ten… FALSE
```

### Search by English or pinyin

``` r
hanzi_search("hao3", n = 3)
#> # A tibble: 3 × 6
#>   simplified traditional pinyin_toned gloss                      score freq_rank
#>   <chr>      <chr>       <chr>        <chr>                      <dbl>     <int>
#> 1 好         好          hǎo          good / appropriate; prope…    NA        82
#> 2 少         少          shǎo         few / less / to lack / to…    NA       233
#> 3 找         找          zhǎo         to try to find / to look …    NA       466

hanzi_search("good", n = 3)
#> # A tibble: 3 × 6
#>   simplified traditional pinyin_toned gloss                      score freq_rank
#>   <chr>      <chr>       <chr>        <chr>                      <dbl>     <int>
#> 1 良         良          liáng        good / very / very much     3.23       835
#> 2 还         還          hái          still / still in progress…  1.96        80
#> 3 美         美          měi          beautiful / very satisfac…  1.96       151
```

### Break a character into its meaningful components

Returns the parts that carry meaning or sound — the 形符 (semantic) and 聲符
(phonetic) of a phono-semantic character, or the components named in its
etymology — never the raw stroke split.

``` r
hanzi_decompose("清")
#> # A tibble: 2 × 5
#>   component role     definition                        radical_name pinyin_toned
#>   <chr>     <chr>    <chr>                             <chr>        <chr>
#> 1 氵        semantic water                             water        shuǐ
#> 2 青        phonetic nature's color; blue, green, bla… green / blue qīng
```

### Find where a component appears

``` r
hanzi_components_of("女")
#> # A tibble: 249 × 2
#>    char   rank
#>    <chr> <int>
#>  1 要       26
#>  2 如       67
#>  3 好       82
#>  4 她       91
#>  5 数      231
#>  6 安      232
#>  7 接      247
#>  8 始      381
#>  9 委      457
#> 10 案      518
#> # ℹ 239 more rows
```

### Get pinyin for a character

``` r
hanzi_pinyin("好")
#> [1] "hǎo"

hanzi_pinyin("好", toned = FALSE)
#> [1] "hao3"
```

## Data sources

The database is built from open data:

-   [makemeahanzi](https://github.com/skishore/makemeahanzi) — character definitions, radicals, and etymology
-   [CC-CEDICT](https://www.mdbg.net/chinese/dictionary?page=cedict) — CC-BY-SA dictionary with pinyin and English glosses
-   [cjk-decomp](https://github.com/amake/cjk-decomp) — character decomposition tree
-   [Jun Da frequency list](https://lingua.mtsu.edu/chinese-computing/) — character frequency from a 9M-word corpus
-   [Leiden word frequency](https://github.com/hermitdave/FrequencyWords) — word-level frequency for 50k Chinese words
-   [Kangxi radicals](https://en.wikipedia.org/wiki/Kangxi_radical) — the 214 traditional radicals with meanings

## Getting help

If you encounter a bug or have a feature request, please [file an issue](https://github.com/dylanpieper/shinyhanzi/issues).