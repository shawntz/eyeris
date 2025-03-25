## Resubmission

This is a resubmission of the eyeris package following feedback from CRAN.

### **CRAN Comments:**

1. Found the following (possibly) invalid file URI:
  * URI: LICENSE.md (From: README.md)

2. Size of tarball: 29588409 bytes
  * Pls reduce to less than 5 MB.

3. Is there some reference about the method you can add in the Description field in the form Authors (year) <doi:10.....>?

Please fix and resubmit.

### **Changes made:**

1. **Fixed invalid URI in README.md**: Removed the local link to `LICENSE.md` to comply with CRAN policies; replaced it with a full external GitHub URL.

2. **Tarball size reduced** to below 5 MB (*now: 3.8 MB*) by removing unnecessary files and directories from the build using `.Rbuildignore`.

3. **'Description' field references request**: There are no references for the package to add to the Description field.

Thank you for the helpful feedback.

-shawn

---

## R CMD check results

Duration: 38.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Downstream dependencies

No strong reverse dependencies to be checked.
