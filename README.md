# rainbowrite

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R build status](https://github.com/richfitz/rainbowrite/workflows/R-CMD-check/badge.svg)](https://github.com/richfitz/rainbowrite/actions)
[![codecov.io](https://codecov.io/github/richfitz/rainbowrite/coverage.svg?branch=master)](https://codecov.io/github/richfitz/rainbowrite?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/richfitz/rainbowrite/badge)](https://www.codefactor.io/repository/github/richfitz/rainbowrite)
![works?](https://img.shields.io/badge/works-on%20my%20machine-pink)
<!-- badges: end -->

This package contains a drop-in replacement for R's `cat`, `message` and `print`, inspired by [lolcat](https://github.com/busyloop/lolcat).

## Usage

Use it just like `cat`


```r
for (i in 1:10) {
  rainbowrite::lolcat("hello world\n")
}
```

![lolcat](man/figures/lolcat.png)

Or like `message`


```r
rainbowrite::lolmessage("wow\n\t\tsuch colours\n   much rainbow")
```

![lolmessage](man/figures/lolmessage.png)


Modify functions that already use `cat` or `message` using `lolify`:


```r
lolsay <- rainbowrite::lolify(cowsay::say)
lolsay("I ate a rainbow", "longcat")
```

![lolify](man/figures/lolify.png)

# Installation

Install from github using [remotes](https://github.com/r-lib/remotes):

```r
remotes::install_github("richfitz/rainbowrite", upgrade = FALSE)
```

## License

MIT + file LICENSE © [Rich FitzJohn](https://github.com/richfitz)
