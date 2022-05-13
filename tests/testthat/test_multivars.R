# testing of multiple variables
cas5k <- iNZightMR::census.at.school.5000

inzplot(~techtv, data = cas5k)

inzplot(~ techtv + techmp3 + techinternet + techmobinternet + techfacebook +
    techtwitter + techbebo + techmyspace + techskype + technone, data = cas5k
)
inzplot(~ techtv + techmp3 + techinternet + techmobinternet + techfacebook +
    techtwitter + techbebo + techmyspace + techskype + technone, data = cas5k,
    keep_missing = TRUE
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

inzplot(~travel+getlunch, data = cas)

inzplot(~eth5_e_y8c+eth5_m_y8c+eth5_p_y8c, data = d)
inzplot(~tu82_0_y8c+tu82_1_y8c+tu82_2_y8c+tu82_3_y8c+tu82_4_y8c+tu82_5_y8c+tu82_6_y8c+tu82_7_y8c+tu82_8_y8c, data = d)
