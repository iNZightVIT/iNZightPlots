# testing of multiple variables
cas5k <- iNZightMR::census.at.school.5000

inzplot(~techtv, data = cas5k)
inzplot(~ techtv + techfacebook, data = cas5k)
