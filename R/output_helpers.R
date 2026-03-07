out_ci_section <- function(estimates, lower, upper,
                           label = "Mean",
                           ci_width = 0.95,
                           bootstrap = FALSE,
                           by_factor = FALSE,
                           factor_names = NULL,
                           plural = NULL,
                           digits = 4L) {
    if (is.null(plural)) plural <- by_factor

    mat <- cbind(estimates, lower, upper)

    bs_label <- if (bootstrap) " Percentile Bootstrap" else ""
    header_text <- paste0(
        label,
        " with ", ci_width * 100, "%", bs_label,
        " Confidence Interval", if (plural) "s" else ""
    )

    out_group(
        out_text(header_text),
        out_blank(),
        out_table(mat,
            col_headers = c("Estimate", "Lower", "Upper"),
            row_headers = factor_names,
            digits = digits
        )
    )
}

out_privacy_section <- function(privacy_controls, width = 100L) {
    if (is.null(privacy_controls)) return(NULL)

    items <- character()

    if (privacy_controls$has("rounding")) {
        items <- c(items, sprintf("counts are rounded using %s",
            switch(privacy_controls$get("rounding"),
                "RR3" = "RR3 (random rounding to base 3)",
                paste0("other (", privacy_controls$get("rounding"), ")")
            )
        ))
    }

    if (privacy_controls$has("suppression")) {
        items <- c(items, sprintf(
            "suppression of counts smaller than %d, indicated by %s%s",
            privacy_controls$get("suppression"),
            privacy_controls$get("symbol"),
            ifelse(privacy_controls$get("secondary_suppression"),
                ", with secondary suppression where necessary", ""
            )
        ))
    }

    if (privacy_controls$has("suppression_raw_counts")) {
        items <- c(items, sprintf(
            "suppression of weighted counts with corresponding unweighted counts < %s",
            privacy_controls$get("suppression_raw_counts")
        ))
    }

    if (privacy_controls$has("suppression_magnitude")) {
        items <- c(items, sprintf(
            "suppression of totals and means where underlying unrounded count < %s",
            privacy_controls$get("suppression_magnitude")
        ))
    }

    if (privacy_controls$has("suppression_quantiles")) {
        q_values <- do.call(cbind, privacy_controls$get("suppression_quantiles"))
        q_items <- apply(q_values, 1L, function(qv) {
            sprintf("  - %s%s if underlying unrounded count < %s",
                qv[1] * 100, "%", qv[2])
        })
        items <- c(items, "suppression of quantiles", q_items)
    }

    if (privacy_controls$has("check_rse")) {
        rse_values <- do.call(cbind, privacy_controls$get("check_rse"))
        rse_items <- apply(rse_values, 1L, function(rv) {
            if (rv[2] == "suppress") {
                sprintf("  - estimates with RSE >= %s%s suppressed", rv[1], "%")
            } else {
                sprintf("  - estimates with RSE >= %s%s marked with %s",
                    rv[1], "%", rv[2])
            }
        })
        items <- c(items,
            "for estimates with large relative sampling error (RSE),",
            rse_items
        )
    }

    if (privacy_controls$has("seed")) {
        items <- c(items, sprintf("using RNG seed %d",
            privacy_controls$get("seed")))
    }

    out_group(
        out_h2("Privacy and confidentialisation information"),
        out_blank(),
        out_bullet(items),
        out_blank(),
        out_text(paste0(
            "NOTE: this feature is still experimental, ",
            "and all output should be manually\n",
            "checked before being made public. ",
            "This is simply to aid that process.\n"
        )),
        out_rule("=", width),
        out_blank()
    )
}
