# shinyhanzi

shinyhanzi is an interactive Shiny application for exploring Chinese characters (汉字). It provides animated stroke order, character decomposition, dictionary lookup with pinyin, English-to-Hanzi search, and frequency analysis. The package also exposes its underlying lookup functions so you can use them in your own R code.

## Installation

``` r
# Install the development version from GitHub:
pak::pak("dylanpieper/shinyhanzi")
```

## Getting started

shinyhanzi requires a database (114 MB) that is downloaded separately. Download it once and it will be cached for all future sessions:

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
#>   simplified traditional pinyin_toned                             gloss freq_rank
#> 1         好          好          hǎo  good / appropriate; proper / …        82
#> 2         少          少         shǎo  few / less / to lack / to be …       233
#> 3         找          找         zhǎo  to try to find / to look for …       466

hanzi_search("good", n = 3)  # English search also works
#>   simplified traditional pinyin_toned                            gloss    score freq_rank
#> 1         良          良        liáng              good / very / very much 3.234724       835
#> 2         还          還          hái  still / still in progress / …       1.959329        80
#> 3         美          美          měi  beautiful / very satisfactory…       1.959329       151
```

### Decompose a character

``` r
hanzi_decompose("好")
#> # A tibble: 2 × 5
#>   component is_intermediate definition                 radical_name pinyin_toned
#>   <chr>     <lgl>           <chr>                      <chr>        <chr>
#> 1 女        FALSE           woman, girl; female        woman        nǚ
#> 2 子        FALSE           son, child; seed, egg; fr… child        zi
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