## Resubmission

This is a resubmission of the eyeris package following feedback from CRAN.

### **CRAN Comments:**

##### Responses to comments in **bold** below:

> 1) It seems like you have too many spaces in your description field. 
Probably because linebreaks count as spaces too. 
Please remove unecassary ones
>
>>> **Thanks for pointing this out. This was a result of hard-wrapping the text by manually line breaking. This has now been resolved.**
(https://github.com/shawntz/eyeris/issues/126?issue=shawntz%7Ceyeris%7C127)


> 2) If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) doi:...
authors (year, ISBN:...)
or if those are not available: https:...
with no space after 'doi:', 'https:' and angle brackets for
auto-linking. (If you want to add a title as well please put it in
quotes: "Title")
For more details:
https://contributor.r-project.org/cran-cookbook/description_issues.html#references
>
>>> **I have now added a handful of relevant references for the method in the DESCRIPTION file using the syntax provided.**
(https://github.com/shawntz/eyeris/issues/126?issue=shawntz%7Ceyeris%7C128)


> 3) Some code lines in examples are commented out. Please never do that.
Ideally find toy examples that can be regularly executed and checked.
Lengthy examples (> 5 sec), can be wrapped in \donttest{}. -> Examples
in comments in:
glassbox.Rd
>
>>> **Thank you for this comment. I have wrapped the previously commented out line of code you were referring to within `glassbox.Rd` with `\donttest{}` as it is a command that runs an interactive workflow and would hang without user input.**
(https://github.com/shawntz/eyeris/issues/126?issue=shawntz%7Ceyeris%7C129)


> 4) \dontrun{} should only be used if the example really cannot be executed
    (e.g. because of missing additional software, missing API keys, ...) by
    the user. That's why wrapping examples in \dontrun{} adds the comment
    ("# Not run:") as a warning for the user. Does not seem necessary.
    Please replace \dontrun with \donttest.
    Please unwrap the examples if they are executable in < 5 sec, or replace
    dontrun{} with \donttest{}.
    For more details:
    https://contributor.r-project.org/cran-cookbook/general_issues.html#structuring-of-examples
>
>>> **This has been resolved accordingly.**
(https://github.com/shawntz/eyeris/issues/126?issue=shawntz%7Ceyeris%7C130)


> 5) You write information messages to the console that cannot be easily
suppressed.
It is more R like to generate objects that can be used to extract the
information a user is interested in, and then print() that object.
Instead of cat() rather use message()/warning() or if(verbose)cat(..)
(or maybe stop()) if you really have to write text to the console.
(except for print, summary, interactive functions) -> R/pipeline-glassbox.R
For more details:
https://contributor.r-project.org/cran-cookbook/code_issues.html#using-printcat
>
>>> **Thank you for the helpful recommendations regarding console output messages. I've gone ahead and added better control over verbosity for all info related messages that a user may be interested in suppressing. Moreover, I've also changed some key messages into warnings that can be suppressed (but shouldn't be as the warnings indicate serious issues with the user's dataset that shouldn't be ignored) -- and as such, the default is to keep these critical messages shown by default (and suppressed by the user at their own risk).**
(https://github.com/shawntz/eyeris/issues/126?issue=shawntz%7Ceyeris%7C131)


> 6) Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your
examples/vignettes/tests you can write to tempdir().
For more details:
https://contributor.r-project.org/cran-cookbook/code_issues.html#writing-files-and-directories-to-the-home-filespace
>
>>> **Understood, thank you for clarifying the CRAN policy on this. I have resolved all of these issues accordingly per your specific recommendations.**
(https://github.com/shawntz/eyeris/issues/126?issue=shawntz%7Ceyeris%7C132)


> 7) Please make sure that you do not change the user's options, par or
working directory. If you really have to do so within functions, please
ensure with an *immediate* call of on.exit() that the settings are reset
when the function is exited.
e.g.:
>
```r
oldpar <- par(no.readonly = TRUE) # code line i
on.exit(par(oldpar)) # code line i + 1
...
par(mfrow=c(2,2)) # somewhere after
```
>
> e.g.: -> R/pipeline-lpfilt.R, R/plot.eyeris.
If you're not familiar with the function, please check ?on.exit. This
function makes it possible to restore options before exiting a function
even if the function breaks. Therefore it needs to be called immediately
after the option change within a function.
For more details:
<https://contributor.r-project.org/cran-cookbook/code_issues.html#change-of-options-graphical-parameters-and-working-directory>
>
>>> **Thank you for this recommendation. I have gone ahead and updated the plotting functions you noted here to verify that all instances of `par()` are preceded by the `oldpar`/`on.exit` safety mechanism you described. I have done so because these `par()` modifications are required to achieve the various layouts needed for diagnostic plotting so that the user can effectively utilize the package to evaluate their pupillometry datasets.**
(https://github.com/shawntz/eyeris/issues/126?issue=shawntz%7Ceyeris%7C133)


> 8) Please do not modifiy the .GlobalEnv. This is not allowed by the CRAN
policies. -> R/plot.eyeris.R
For more details:
https://contributor.r-project.org/cran-cookbook/code_issues.html#writing-to-the-.globalenv
>
>>> **Apologies for not conforming to CRAN's policy here. I have resolved these issues accordingly by (1) ensuring that `.Random.seed` is not modified within `R/plot.eyeris.R`, and (2) adding in a safety check within `R/glassbox.R`'s call to `plot()` to prevent any accidental issues related to this within that function.**
(https://github.com/shawntz/eyeris/issues/126?issue=shawntz%7Ceyeris%7C134)

Please fix and resubmit.

---

**Thank you for the helpful feedback.**

**-shawn**

---

## R CMD check results
Duration: 54.6s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Downstream dependencies

No strong reverse dependencies to be checked.
