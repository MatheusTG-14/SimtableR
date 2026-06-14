# SimtablR 2.0.0

## Major Changes 

### tb() Function
*Overhauled tb() to return a structured list. The object inherits the S3 class vector c("tb", "simtab"). Matrices with attributes are no longer returned directly from the primary function loop.
*Ratio Schema Standardization: Renamed fields within the internal ratios data frame to lower_ci and upper_ci to establish strict compatibility with multivariable regression tables (regtab()).
*Simplified Continuous Syntax: Enhanced var.type parsing to accept an unnamed scalar character string shorthand (e.g., var.type = "continuous") and map it automatically to the main row variable.
#### New Features
*Table Stacking (rbind.tb): Implemented the rbind.tb() S3 method to support the vertical stacking of discrete tb objects sharing the same column variables.
*Dual Export Modes: Expanded as.data.frame.tb() and as.data.frame.rbind_tb() to support a tidy toggle. tidy = FALSE (default) provides display-ready character strings for manuscripts, while tidy = TRUE returns unformatted numeric data frames optimized for ggplot2 workflows.
*RStudio Autocomplete Replacement: Integrated an unexported interactive completion replacement hook inside zzz.R using .rs.registerAutocompleteReplacement() to dynamically expose dataset column names inside RStudio console environments.
*Wald-Aligned Ratio Statistics: Upgraded unadjusted Prevalence Ratio (PR) and Odds Ratio (OR) calculations to compute Wald z-score p-values aligned directly alongside confidence intervals.
*Added new runtime educational message() notifications that fire automatically under specific conditions
*Added explicit registerS3method() entries for rbind, print, and as.data.frame generics within .onLoad() to guarantee stable dispatch across development environments, source routines, and unattached package builds.
#### Other changes
*Fixed a vulnerability where common column names (like p or col) matching formatting flags were silently intercepted by the NSE symbol parser.
*Extracted all text formatting, cell stitching matrices, margin additions, and string template processing out of core workflows and isolated them within a unified internal builder called .build_display_matrix().
