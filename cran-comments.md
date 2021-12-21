Dear Mr. Ligges, dear CRAN maintainers.

> R Under development (unstable) (2021-12-19 r81394 ucrt)
>   Pretests results:
> Windows: <https://win-builder.r-project.org/incoming_pretest/MALDIquant_1.21_20211221_120139/Windows/00check.log>
> Status: 1 NOTE
> Debian: <https://win-builder.r-project.org/incoming_pretest/MALDIquant_1.21_20211221_120139/Debian/00check.log>
> Status: 1 NOTE
>   Last released version's CRAN status: OK: 13
> See: <https://CRAN.R-project.org/web/checks/check_results_MALDIquant.html>
>   CRAN Web: <https://cran.r-project.org/package=MALDIquant>
>   *** Strong rev. depends ***: MALDIquantForeign MALDIrppa MGMS2
>   Best regards,
> CRAN teams' auto-check service
> ...
> Flavor: r-devel-linux-x86_64-debian-gcc
> Check: CRAN incoming feasibility, Result: NOTE
>    Maintainer: 'Sebastian Gibb <mail@sebastiangibb.de>'
>       Found the following (possibly) invalid URLs:
>      URL: https://www.strimmerlab.github.io/software/maldiquant/
>        From: DESCRIPTION
>              man/AbstractMassObject-class.Rd
>              ...
>              man/warp-functions.Rd
>        Status: Error
>        Message: SSL: certificate subject name (www.github.com) does not match target host name 'www.strimmerlab.github.io'
>
> Indeed, that URL is also not accepted by my browser.
>
> Please fix and resubmit.
>
> Best,
> Uwe Ligges

Sorry for the misformed URL. I removed the superfluous www.

Best wishes,

Sebastian

