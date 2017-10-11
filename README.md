# rainbowrite

[![Build Status](https://travis-ci.org/richfitz/rainbowrite.png?branch=master)](https://travis-ci.org/richfitz/rainbowrite)

This package contains a drop-in replacement for R's `cat` and `message`, based on the [lolcat](https://github.com/busyloop/lolcat).

The terminal painting code takes inspiration from the the Ruby gem [paint](https://github.com/janlelis/paint) and from the R package [xtermStyle](http://cran.r-project.org/web/packages/xtermStyle).

# Installation instructions

Install using [devtools](https://github.com/hadley/devtools):

```r
devtools::install_github("richfitz/rainbowrite")
```

If you don't have devtools installed you will see an error "there is no package called 'devtools'"; if that happens install devtools with `install.packages("devtools")`.

# Usage

Use it just like `cat`

```
library(rainbowrite)
for (i in 1:10) {
  lolcat("hello world\n")
}
file <- system.file("DESCRIPTION", package="rainbowrite")
rainbowrite:::lolcat(readLines(file), sep="\n")
```

Or like `message`

```
lolmessage("wow\n\t\tsuch colours\n   much rainbow")
```

![basic screenshot](https://github.com/richfitz/rainbowrite/raw/master/extra/screenshot1.png)

Modify functions that already use `cat` or `message`:

```
say <- lolify(cowsay::say)
say("I ate a rainbow", "longcat")
```

![longcat screenshot](https://github.com/richfitz/rainbowrite/raw/master/extra/screenshot2.png)

There was a testthat reporter

```
test_dir("tests/testthat", reporter=lolreporter())
```

![testthat screenshot](https://github.com/richfitz/rainbowrite/raw/master/extra/screenshot3.png)

But this needs rewriting to support the new testthat classes.

# Issues

THere are two options for mimicking the behaviour of `cat`: accurate or fast -- I've chosen to implement this accurately, which involves passing all arguments through `base::cat` and then again through `lolcat`.  This uses temporary files, which are apparently faster than `textConnection`, but are still going to be slow.  However, some care is needed to get newlines to behave properly and I think that this is totally worth it.  But don't use this anywhere where speed is likely to be an issue.

It is (un?)surprisingly hard to test code that embeds control sequences into text strings, so the package is poorly tested.  Bug reports welcome on the [issues page](issues).

There is some support for `stop()` and `warning()` but I need to work out how to rewrite the triggering call or they are just confusing.
