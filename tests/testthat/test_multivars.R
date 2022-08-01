# testing of multiple variables
skip("Tests need to be completed")

cas5k <- iNZightMR::census.at.school.5000

inzplot(~techtv, data = cas5k)

inzplot(~ techtv + techmp3 + techinternet + techmobinternet + techfacebook +
    techtwitter + techbebo + techmyspace + techskype + technone, data = cas5k
)
inzplot(~ techtv + techmp3 + techinternet + techmobinternet + techfacebook +
    techtwitter + techbebo + techmyspace + techskype + technone, data = cas5k,
    keep_missing = TRUE
)

inzplot(~ techtv + techmp3 + techinternet + techmobinternet + techfacebook +
    techtwitter + techbebo + techmyspace + techskype + technone, data = cas5k,
    g1 = gender
)


inzplot(~ techtv + techmp3 + techinternet + techmobinternet + techfacebook +
    techtwitter + techbebo + techmyspace + techskype + technone, data = cas5k,
    g1 = gender, g1.level = "female"
)

inzsummary(~ techtv + techmp3 + techinternet, data = cas5k)

dd <- data.frame(
    q1 = sample(c("never", "sometimes", "often", "always", "don't know"), 100, replace = TRUE),
    q2 = sample(c("never", "sometimes", "often", "always", "don't know"), 100, replace = TRUE),
    stringsAsFactors = TRUE
)

inzplot(~q1 + q2, data = dd)
inzplot(~q1 + q2, data = dd, plottype = "vertical-stack")


cas_raw <- iNZightTools::smart_read('cas500_coded.csv')
cas_dict <- iNZightTools::read_dictionary('casdict.csv',
    name = "variable",
    title = "friendly_name"
)
cas <- iNZightTools::apply_dictionary(cas_raw, cas_dict)

levels(cas$getlunch) <- levels(cas$travel)

for (c in c("bike", "bus", "motor", "other", "train", "walk")) {
    cas[[c]] <- expss::set_var_lab(
        ifelse(cas$travel == c, "yes", "no"),
        sprintf("What ways do you travel to school? %s%s",
            toupper(substr(c, 1, 1)),
            substr(c, 2, 100)
        )
    )
}

inzsummary(~getlunch+travel, data = cas)

inzsummary(~getlunch+travel, data = cas, g1 = gender)

inzsummary(~bike + bus + motor + train + walk + other, data = cas)

inzplot(~bus + train + bike, data = cas, g1 = gender)
inzsummary(~bus + train + bike, data = cas, g1 = gender)

inzsummary(~bike + bus + motor + train + walk + other, data = cas, g1 = gender)


inzplot(~eth5_e_y8c+eth5_m_y8c+eth5_p_y8c, data = d)
inzplot(~tu82_0_y8c+tu82_1_y8c+tu82_2_y8c+tu82_3_y8c+tu82_4_y8c+tu82_5_y8c+tu82_6_y8c+tu82_7_y8c+tu82_8_y8c, data = d)


## linked multi-vars
dummy_df <- data.frame(
    d1_varx_1 = structure(factor(sample(c("yes", "no"), 100, TRUE, c(0.2, 0.8))), table = "d1"),
    d1_varx_2 = structure(factor(sample(c("yes", "no"), 100, TRUE, c(0.4, 0.6))), table = "d1"),
    d1_varx_3 = structure(factor(sample(c("yes", "no"), 100, TRUE, c(0.9, 0.1))), table = "d1"),
    d2_varx_1 = structure(factor(sample(c("yes", "no"), 100, TRUE, c(0.5, 0.5))), table = "d2"),
    d2_varx_2 = structure(factor(sample(c("yes", "no"), 100, TRUE, c(0.3, 0.7))), table = "d2"),
    d2_varx_3 = structure(factor(sample(c("yes", "no"), 100, TRUE, c(0.8, 0.2))), table = "d2")
)
dummy_df2 <- data.frame(
    varx_1 = factor(sample(c("yes", "no"), 100, TRUE, c(0.2, 0.8))),
    varx_2 = factor(sample(c("yes", "no"), 100, TRUE, c(0.4, 0.6))),
    varx_3 = factor(sample(c("yes", "no"), 100, TRUE, c(0.9, 0.1)))
)

inzplot(~ d1_varx_1 + d1_varx_2 + d1_varx_3 + d2_varx_1 + d2_varx_2 + d2_varx_3, data = dummy_df)
inzplot(~ varx_1 + varx_2 + varx_3, data = dummy_df2)


# guinz
guinz <- iNZightTools::load_linked('~/terourou/guinz/data/guinz_full.inzlnk')

inzplot(~ eth5_e_am + eth5_m_am + eth5_p_am + eth5_a_am, data = guinz, outcome_value = "Yes")

inzplot(~ eth5_e_am + eth5_m_am + eth5_p_am + eth5_a_am + eth5_mela_am +
    eth5_o_am + eth5_nzder_am + eth5_e_m9m + eth5_m_m9m + eth5_p_m9m + eth5_a_m9m +
    eth5_mela_m9m + eth5_o_m9m + eth5_nzder_m9m + eth5_e_y8c + eth5_m_y8c +
    eth5_p_y8c + eth5_a_y8c + eth5_mela_y8c + eth5_o_y8c + eth5_nzder_y8c,
    outcome_value = "Yes",
    rotation = TRUE,
    x_groups = list(
        European = c("European", "New Zealand European"),
        Pacific = c("Pacific", "Pacific people", "pacific")
    ),
    data = guinz)

inzplot(~ eth5_e_am + eth5_m_am + eth5_p_am + eth5_a_am + eth5_mela_am +
    eth5_o_am + eth5_nzder_am + eth5_e_m9m + eth5_m_m9m + eth5_p_m9m +
    eth5_a_m9m + eth5_mela_m9m + eth5_o_m9m + eth5_nzder_m9m + eth5_e_m54cm +
    eth5_m_m54cm + eth5_p_m54cm + eth5_a_m54cm + eth5_mela_m54cm +
    eth5_o_m54cm + eth5_nzder_m54cm + eth5_e_y8c + eth5_m_y8c +
    eth5_p_y8c + eth5_a_y8c + eth5_mela_y8c + eth5_o_y8c + eth5_nzder_y8c,
    data = guinz,
    x_groups = list(
        European = c("European", "New Zealand European", "European?"),
        Pacific = c("Pacific", "Pacific people", "pacific", "Pacific?"),
        Maori = c("Maori", "Maori?"),
        Asian = c("Asian", "Asian?"),
        MELAA = c("MELAA", "MELAA?"),
        Other = c("Other", "Other?"),
        "New Zealander" = c("New Zealander", "New Zealander?")
    ),
    outcome_value = "Yes",
    rotation = TRUE,
    full_cases = TRUE
)


inzsummary(~ eth5_e_am + eth5_m_am + eth5_p_am + eth5_a_am + eth5_mela_am +
    eth5_o_am + eth5_nzder_am + eth5_e_m9m + eth5_m_m9m + eth5_p_m9m +
    eth5_a_m9m + eth5_mela_m9m + eth5_o_m9m + eth5_nzder_m9m + eth5_e_m54cm +
    eth5_m_m54cm + eth5_p_m54cm + eth5_a_m54cm + eth5_mela_m54cm +
    eth5_o_m54cm + eth5_nzder_m54cm + eth5_e_y8c + eth5_m_y8c +
    eth5_p_y8c + eth5_a_y8c + eth5_mela_y8c + eth5_o_y8c + eth5_nzder_y8c,
    data = guinz,
    x_groups = list(
        European = c("European", "New Zealand European", "European?"),
        Pacific = c("Pacific", "Pacific people", "pacific", "Pacific?"),
        Maori = c("Maori", "Maori?"),
        Asian = c("Asian", "Asian?"),
        MELAA = c("MELAA", "MELAA?"),
        Other = c("Other", "Other?"),
        "New Zealander" = c("New Zealander", "New Zealander?")
    ),
    outcome_value = "Yes",
    rotation = TRUE,
    full_cases = TRUE
)



inzplot(~ eth5_e_am + eth5_m_am + eth5_p_am + eth5_a_am + eth5_mela_am +
    eth5_o_am + eth5_nzder_am + eth5_e_m9m + eth5_m_m9m + eth5_p_m9m +
    eth5_a_m9m + eth5_mela_m9m + eth5_o_m9m + eth5_nzder_m9m + eth5_e_m54cm +
    eth5_m_m54cm + eth5_p_m54cm + eth5_a_m54cm + eth5_mela_m54cm +
    eth5_o_m54cm + eth5_nzder_m54cm + eth5_e_y8c + eth5_m_y8c +
    eth5_p_y8c + eth5_a_y8c + eth5_mela_y8c + eth5_o_y8c + eth5_nzder_y8c,
    data = guinz,
    x_groups = list(
        European = c("European", "New Zealand European", "European?"),
        Pacific = c("Pacific", "Pacific people", "pacific", "Pacific?"),
        Maori = c("Maori", "Maori?"),
        Asian = c("Asian", "Asian?"),
        MELAA = c("MELAA", "MELAA?"),
        Other = c("Other", "Other?"),
        "New Zealander" = c("New Zealander", "New Zealander?")
    ),
    # outcome_value = "Yes",
    # rotation = TRUE,
    full_cases = TRUE
)

inzplot(~ eth5_e_am + eth5_m_am + eth5_p_am + eth5_a_am,
    g1 = cself_proeth_y8c,
    data = guinz,
    outcome_value = "Yes"
)
