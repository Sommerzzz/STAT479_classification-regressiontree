library(data.table)
library(dplyr)
########## data2014-2016 ##########

# read data
dat <- fread("natl2016.csv")

# select variables
natl2016 <- dat %>% 
  filter(dbwt != 9999) %>%
  mutate(lowbwt = ifelse(dbwt < 2500, 1, 0)) %>%
  select(lowbwt, attend, bfacil, bfacil3, bmi, bmi_r, cig_0, cig_1, cig_2, cig_3, cig_rec, 
         combgest, dlmp_mm, dmar, dmeth_rec, dob_mm, dob_wk, dplural, dwgt_r, 
         f_cigs_0, f_cigs_1, f_cigs_2, f_cigs_3, f_dwgt, f_facility, f_ip_chlam, 
         f_ip_gonor, f_ip_hepatb, f_ip_hepatc, f_ip_syph, f_m_ht, f_mar_p, f_meduc, 
         f_mpcb, f_ob_fail, f_ob_succ, f_pay, f_pay_rec, f_pwgt, f_rf_cesar, 
         f_rf_eclamp, f_rf_gdiab, f_rf_ghyper, f_rf_inf_art, f_rf_ncesar, 
         f_rf_pdiab, f_rf_phyper, f_rf_ppb, f_tobaco, f_tpcv, f_wic, f_wtgain, 
         fagecomb, fagerec11, fbrace, feduc, fhisp_r, gestrec10, gestrec3, illb_r, 
         illb_r11, ilop_r, ilop_r11, ilp_r11, ip_chlam, ip_gon, ip_hepatb, 
         ip_hepatc, ip_syph, lbo_rec, ld_anes, ld_antb, ld_augm, ld_chor, ld_indl, 
         ld_ster, m_ht_in, mager, mager14, mager9, mar_p, mbrace, me_pres, me_rout, 
         me_trial, meduc, mhisp_r, mm_aicu, mm_mtr, mm_plac, mm_rupt, mm_uhyst, 
         mtran, pay, pay_rec, precare, previs_rec, priorterm, pwgt_r, rdmeth_rec, 
         restatus, rf_artec, rf_cesar, rf_cesarn, rf_fedrg, rf_inftr, rf_ppterm, 
         setorder_r, sex, tbo_rec, wic, wtgain, wtgain_rec)

# categorical variables
c_var <- c("attend", "bfacil", "bfacil3", "bmi_r", "cig_rec", 
           "dlmp_mm", "dmar", "dmeth_rec", "dob_mm", "dob_wk", "dplural", 
           "f_cigs_0", "f_cigs_1", "f_cigs_2", "f_cigs_3", "f_dwgt", "f_facility", "f_ip_chlam", 
           "f_ip_gonor", "f_ip_hepatb", "f_ip_hepatc", "f_ip_syph", "f_m_ht", "f_mar_p", "f_meduc", 
           "f_mpcb", "f_ob_fail", "f_ob_succ", "f_pay", "f_pay_rec", "f_pwgt", "f_rf_cesar", 
           "f_rf_eclamp", "f_rf_gdiab", "f_rf_ghyper", "f_rf_inf_art", "f_rf_ncesar", 
           "f_rf_pdiab", "f_rf_phyper", "f_rf_ppb", "f_tobaco", "f_tpcv", "f_wic", "f_wtgain", 
           "fagerec11", "fbrace", "feduc", "fhisp_r", "gestrec10", "gestrec3", 
           "illb_r11", "ilop_r11", "ilp_r11", "ip_chlam", "ip_gon", "ip_hepatb", 
           "ip_hepatc", "ip_syph", "lbo_rec", "ld_anes", "ld_antb", "ld_augm", "ld_chor", "ld_indl", 
           "ld_ster", "mager14", "mager9", "mar_p", "mbrace", "me_pres", "me_rout", 
           "me_trial", "meduc", "mhisp_r", "mm_aicu", "mm_mtr", "mm_plac", "mm_rupt", "mm_uhyst", 
           "mtran", "pay", "pay_rec", "precare", "previs_rec", "rdmeth_rec", 
           "restatus", "rf_artec", "rf_cesar", "rf_fedrg", "rf_inftr", "rf_ppterm", 
           "setorder_r", "sex", "tbo_rec", "wic", "wtgain_rec")
# numeric variables
n_var <- c("bmi", "cig_0", "cig_1", "cig_2", "cig_3", "combgest", "dwgt_r", "fagecomb", "illb_r", "ilop_r",
           "m_ht_in", "mager", "priorterm", "pwgt_r", "rf_cesarn", "wtgain")


# deal with NA in numerical variables
natl2016 <- natl2016 %>%
    mutate(bmi = replace(bmi, bmi == 99.9, NA)) %>%
    mutate(cig_0 = replace(cig_0, cig_0 == 99, NA)) %>%
    mutate(cig_1 = replace(cig_1, cig_1 == 99, NA)) %>%
    mutate(cig_2 = replace(cig_2, cig_2 == 99, NA)) %>%
    mutate(cig_3 = replace(cig_3, cig_3 == 99, NA)) %>%
    mutate(combgest = replace(combgest, combgest == 99, NA)) %>%
    mutate(dwgt_r = replace(dwgt_r, dwgt_r == 999, NA)) %>%
    mutate(fagecomb = replace(fagecomb, fagecomb == 99, NA)) %>%
    mutate(m_ht_in = replace(m_ht_in, m_ht_in == 99, NA)) %>%
    mutate(priorterm = replace(priorterm, priorterm == 99, NA)) %>%
    mutate(pwgt_r = replace(pwgt_r, pwgt_r == 999, NA)) %>%
    mutate(rf_cesarn = replace(rf_cesarn, rf_cesarn == 99, NA)) %>%
    mutate(wtgain = replace(wtgain, wtgain == 99, NA)) %>%
    mutate(illb_r = replace(illb_r, illb_r == 888, NA)) %>%
    mutate(illb_r = replace(illb_r, illb_r == 999, NA)) %>%
    mutate(ilop_r = replace(ilop_r, ilop_r == 888, NA)) %>%
    mutate(ilop_r = replace(ilop_r, ilop_r == 999, NA))


# deal with NA in categorical variables
natl2016 <- natl2016 %>%
    mutate(cig_rec = replace(cig_rec, cig_rec == "", "U")) %>%
    mutate(ip_chlam = replace(ip_chlam, ip_chlam == "", "U")) %>%
    mutate(ip_gon = replace(ip_gon, ip_gon == "", "U")) %>%
    mutate(ip_hepatb = replace(ip_hepatb, ip_hepatb == "", "U")) %>%
    mutate(ip_hepatc = replace(ip_hepatc, ip_hepatc == "", "U")) %>%
    mutate(ip_syph = replace(ip_syph, ip_syph == "", "U")) %>%
    mutate(ld_anes = replace(ld_anes, ld_anes == "", "U")) %>%
    mutate(ld_antb = replace(ld_antb, ld_antb == "", "U")) %>%
    mutate(ld_augm = replace(ld_augm, ld_augm == "", "U")) %>%
    mutate(ld_chor = replace(ld_chor, ld_chor == "", "U")) %>%
    mutate(ld_indl = replace(ld_indl, ld_indl == "", "U")) %>%
    mutate(ld_ster = replace(ld_ster, ld_ster == "", "U")) %>%
    mutate(mar_p = replace(mar_p, mar_p == "", "U")) %>%
    mutate(me_pres = replace(me_pres, me_pres == "", "9")) %>%
    mutate(me_rout = replace(me_rout, me_rout == "", "9")) %>%
    mutate(me_trial = replace(me_trial, me_trial == "", "U")) %>%
    mutate(mm_aicu = replace(mm_aicu, mm_aicu == "", "U")) %>%
    mutate(mm_mtr = replace(mm_mtr, mm_mtr == "", "U")) %>%
    mutate(mm_plac = replace(mm_plac, mm_plac == "", "U")) %>%
    mutate(mm_rupt = replace(mm_rupt, mm_rupt == "", "U")) %>%
    mutate(mm_uhyst = replace(mm_uhyst, mm_uhyst == "", "U")) %>%
    mutate(mtran = replace(mtran, mtran == "", "U")) %>%
    mutate(rf_artec = replace(rf_artec, rf_artec == "", "U")) %>%
    mutate(rf_cesar = replace(rf_cesar, rf_cesar == "", "U")) %>%
    mutate(rf_fedrg = replace(rf_fedrg, rf_fedrg == "", "U")) %>%
    mutate(rf_inftr = replace(rf_inftr, rf_inftr == "", "U")) %>%
    mutate(rf_ppterm = replace(rf_ppterm, rf_ppterm == "", "U")) %>%
    mutate(wic = replace(wic, wic == "", "U"))
    
# convert data type
natl2016 <- natl2016 %>% 
  mutate_at(c_var, funs(factor)) %>% 
  mutate_at(n_var, funs(as.numeric))

# NAs
natl2016 <- natl2016 %>%
    mutate(attend = replace(attend, is.na(attend), "9")) %>%
    mutate(bfacil = replace(bfacil, is.na(bfacil), "9")) %>%
    mutate(bfacil3 = replace(bfacil3, is.na(bfacil3), "3")) %>%
    mutate(bmi_r = replace(bmi_r, is.na(bmi_r), "9")) %>%
    mutate(dlmp_mm = replace(dlmp_mm, is.na(dlmp_mm), "99")) %>%
    mutate(dmeth_rec = replace(dmeth_rec, is.na(dmeth_rec), "9")) %>%
    mutate(fbrace = replace(fbrace, is.na(fbrace), "9")) %>%
    mutate(feduc = replace(feduc, is.na(feduc), "9")) %>%
    mutate(fhisp_r = replace(fhisp_r, is.na(fhisp_r), "9")) %>%
    mutate(gestrec10 = replace(gestrec10, is.na(gestrec10), "99")) %>%
    mutate(gestrec3 = replace(gestrec3, is.na(gestrec3), "3")) %>%
    mutate(illb_r11 = replace(illb_r11, is.na(illb_r11), "99")) %>%
    mutate(ilop_r11 = replace(ilop_r11, is.na(ilop_r11), "99")) %>%
    mutate(ilp_r11 = replace(ilp_r11, is.na(ilp_r11), "99")) %>%
    mutate(lbo_rec = replace(lbo_rec, is.na(lbo_rec), "9")) %>%
    mutate(meduc = replace(meduc, is.na(meduc), "9")) %>%
    mutate(mhisp_r = replace(mhisp_r, is.na(mhisp_r), "9")) %>%
    mutate(pay = replace(pay, is.na(pay), "9")) %>%
    mutate(pay_rec = replace(pay_rec, is.na(pay_rec), "9")) %>%
    mutate(precare = replace(precare, is.na(precare), "99")) %>%
    mutate(previs_rec = replace(previs_rec, is.na(previs_rec), "12")) %>%
    mutate(rdmeth_rec = replace(rdmeth_rec, is.na(rdmeth_rec), "9")) %>%
    mutate(setorder_r = replace(setorder_r, is.na(setorder_r), "9")) %>%
    mutate(tbo_rec = replace(tbo_rec, is.na(tbo_rec), "9")) %>%
    mutate(wtgain_rec = replace(wtgain_rec, is.na(wtgain_rec), "9"))
    
# response
natl2016 <- natl2016 %>%
    mutate_at("lowbwt", funs(factor))

# save rdata
save(natl2016, file = "natl2016.RData")



########## data2011-2013 ##########

# read data
dat <- fread("natl2011.csv")

# some changes
natl2011 <- dat %>%
    mutate(fracehisp = replace(fracehisp, fracehisp == 6, 0)) %>%
    mutate(fracehisp = replace(fracehisp, fracehisp == 7, 0)) %>%
    mutate(fracehisp = replace(fracehisp, fracehisp == 8, 0)) %>%
    mutate(mracehisp = replace(mracehisp, mracehisp == 6, 0)) %>%
    mutate(mracehisp = replace(mracehisp, mracehisp == 7, 0)) %>%
    mutate(mracehisp = replace(mracehisp, mracehisp == 8, 0)) %>%
    rename(fhisp_r = fracehisp) %>%
    rename(mhisp_r = mracehisp) %>%
    rename(ip_gon = ip_gono) %>%
    rename(ld_antb = ld_anti) %>%
    rename(dmar = mar) %>%
    rename(mm_aicu = mm_icu) %>%
    select(-fbrace) %>%
    select(-mbrace) %>%
    rename(fbrace = fracerec) %>%
    rename(mbrace = mracerec)

# select variables
natl2011 <- natl2011 %>% 
  filter(dbwt != 9999) %>%
  mutate(lowbwt = ifelse(dbwt < 2500, 1, 0)) %>%
  select(lowbwt, attend, bfacil, bfacil3, bmi, bmi_r, cig_0, cig_1, cig_2, cig_3, cig_rec, 
         combgest, dlmp_mm, dmar, dmeth_rec, dob_mm, dob_wk, dplural, dwgt_r, 
         f_cigs_0, f_cigs_1, f_cigs_2, f_cigs_3, f_dwgt, f_facility, f_ip_chlam, 
         f_ip_gonor, f_ip_hepatb, f_ip_hepatc, f_ip_syph, f_m_ht, f_mar_p, f_meduc, 
         f_mpcb, f_ob_fail, f_ob_succ, f_pay, f_pay_rec, f_pwgt, f_rf_cesar, 
         f_rf_eclamp, f_rf_gdiab, f_rf_ghyper, f_rf_inf_art, f_rf_ncesar, 
         f_rf_pdiab, f_rf_phyper, f_rf_ppb, f_tobaco, f_tpcv, f_wic, f_wtgain, 
         fagecomb, fagerec11, fbrace, feduc, fhisp_r, gestrec10, gestrec3, illb_r, 
         illb_r11, ilop_r, ilop_r11, ilp_r11, ip_chlam, ip_gon, ip_hepatb, 
         ip_hepatc, ip_syph, lbo_rec, ld_anes, ld_antb, ld_augm, ld_chor, ld_indl, 
         ld_ster, m_ht_in, mager, mager14, mager9, mar_p, mbrace, me_pres, me_rout, 
         me_trial, meduc, mhisp_r, mm_aicu, mm_mtr, mm_plac, mm_rupt, mm_uhyst, 
         mtran, pay, pay_rec, precare, previs_rec, priorterm, pwgt_r, rdmeth_rec, 
         restatus, rf_artec, rf_cesar, rf_cesarn, rf_fedrg, rf_inftr, rf_ppterm, 
         setorder_r, sex, tbo_rec, wic, wtgain, wtgain_rec)

# categorical variables
c_var <- c("attend", "bfacil", "bfacil3", "bmi_r", "cig_rec", 
           "dlmp_mm", "dmar", "dmeth_rec", "dob_mm", "dob_wk", "dplural", 
           "f_cigs_0", "f_cigs_1", "f_cigs_2", "f_cigs_3", "f_dwgt", "f_facility", "f_ip_chlam", 
           "f_ip_gonor", "f_ip_hepatb", "f_ip_hepatc", "f_ip_syph", "f_m_ht", "f_mar_p", "f_meduc", 
           "f_mpcb", "f_ob_fail", "f_ob_succ", "f_pay", "f_pay_rec", "f_pwgt", "f_rf_cesar", 
           "f_rf_eclamp", "f_rf_gdiab", "f_rf_ghyper", "f_rf_inf_art", "f_rf_ncesar", 
           "f_rf_pdiab", "f_rf_phyper", "f_rf_ppb", "f_tobaco", "f_tpcv", "f_wic", "f_wtgain", 
           "fagerec11", "fbrace", "feduc", "fhisp_r", "gestrec10", "gestrec3", 
           "illb_r11", "ilop_r11", "ilp_r11", "ip_chlam", "ip_gon", "ip_hepatb", 
           "ip_hepatc", "ip_syph", "lbo_rec", "ld_anes", "ld_antb", "ld_augm", "ld_chor", "ld_indl", 
           "ld_ster", "mager14", "mager9", "mar_p", "mbrace", "me_pres", "me_rout", 
           "me_trial", "meduc", "mhisp_r", "mm_aicu", "mm_mtr", "mm_plac", "mm_rupt", "mm_uhyst", 
           "mtran", "pay", "pay_rec", "precare", "previs_rec", "rdmeth_rec", 
           "restatus", "rf_artec", "rf_cesar", "rf_fedrg", "rf_inftr", "rf_ppterm", 
           "setorder_r", "sex", "tbo_rec", "wic", "wtgain_rec")
# numeric variables
n_var <- c("bmi", "cig_0", "cig_1", "cig_2", "cig_3", "combgest", "dwgt_r", "fagecomb", "illb_r", "ilop_r",
           "m_ht_in", "mager", "priorterm", "pwgt_r", "rf_cesarn", "wtgain")


# deal with NA in numerical variables
natl2011 <- natl2011 %>%
    mutate(bmi = replace(bmi, bmi == 99.9, NA)) %>%
    mutate(cig_0 = replace(cig_0, cig_0 == 99, NA)) %>%
    mutate(cig_1 = replace(cig_1, cig_1 == 99, NA)) %>%
    mutate(cig_2 = replace(cig_2, cig_2 == 99, NA)) %>%
    mutate(cig_3 = replace(cig_3, cig_3 == 99, NA)) %>%
    mutate(combgest = replace(combgest, combgest == 99, NA)) %>%
    mutate(dwgt_r = replace(dwgt_r, dwgt_r == 999, NA)) %>%
    mutate(fagecomb = replace(fagecomb, fagecomb == 99, NA)) %>%
    mutate(m_ht_in = replace(m_ht_in, m_ht_in == 99, NA)) %>%
    mutate(priorterm = replace(priorterm, priorterm == 99, NA)) %>%
    mutate(pwgt_r = replace(pwgt_r, pwgt_r == 999, NA)) %>%
    mutate(rf_cesarn = replace(rf_cesarn, rf_cesarn == 99, NA)) %>%
    mutate(wtgain = replace(wtgain, wtgain == 99, NA)) %>%
    mutate(illb_r = replace(illb_r, illb_r == 888, NA)) %>%
    mutate(illb_r = replace(illb_r, illb_r == 999, NA)) %>%
    mutate(ilop_r = replace(ilop_r, ilop_r == 888, NA)) %>%
    mutate(ilop_r = replace(ilop_r, ilop_r == 999, NA))


# deal with NA in categorical variables
natl2011 <- natl2011 %>%
    mutate(cig_rec = replace(cig_rec, cig_rec == "", "U")) %>%
    mutate(ip_chlam = replace(ip_chlam, ip_chlam == "", "U")) %>%
    mutate(ip_gon = replace(ip_gon, ip_gon == "", "U")) %>%
    mutate(ip_hepatb = replace(ip_hepatb, ip_hepatb == "", "U")) %>%
    mutate(ip_hepatc = replace(ip_hepatc, ip_hepatc == "", "U")) %>%
    mutate(ip_syph = replace(ip_syph, ip_syph == "", "U")) %>%
    mutate(ld_anes = replace(ld_anes, ld_anes == "", "U")) %>%
    mutate(ld_antb = replace(ld_antb, ld_antb == "", "U")) %>%
    mutate(ld_augm = replace(ld_augm, ld_augm == "", "U")) %>%
    mutate(ld_chor = replace(ld_chor, ld_chor == "", "U")) %>%
    mutate(ld_indl = replace(ld_indl, ld_indl == "", "U")) %>%
    mutate(ld_ster = replace(ld_ster, ld_ster == "", "U")) %>%
    mutate(mar_p = replace(mar_p, mar_p == "", "U")) %>%
    mutate(me_pres = replace(me_pres, me_pres == "", "9")) %>%
    mutate(me_rout = replace(me_rout, me_rout == "", "9")) %>%
    mutate(me_trial = replace(me_trial, me_trial == "", "U")) %>%
    mutate(mm_aicu = replace(mm_aicu, mm_aicu == "", "U")) %>%
    mutate(mm_mtr = replace(mm_mtr, mm_mtr == "", "U")) %>%
    mutate(mm_plac = replace(mm_plac, mm_plac == "", "U")) %>%
    mutate(mm_rupt = replace(mm_rupt, mm_rupt == "", "U")) %>%
    mutate(mm_uhyst = replace(mm_uhyst, mm_uhyst == "", "U")) %>%
    mutate(mtran = replace(mtran, mtran == "", "U")) %>%
    mutate(rf_artec = replace(rf_artec, rf_artec == "", "U")) %>%
    mutate(rf_cesar = replace(rf_cesar, rf_cesar == "", "U")) %>%
    mutate(rf_fedrg = replace(rf_fedrg, rf_fedrg == "", "U")) %>%
    mutate(rf_inftr = replace(rf_inftr, rf_inftr == "", "U")) %>%
    mutate(rf_ppterm = replace(rf_ppterm, rf_ppterm == "", "U")) %>%
    mutate(wic = replace(wic, wic == "", "U"))

# convert data type
natl2011 <- natl2011 %>% 
  mutate_at(c_var, funs(factor)) %>% 
  mutate_at(n_var, funs(as.numeric))

# NAs
natl2011 <- natl2011 %>%
    mutate(attend = replace(attend, is.na(attend), "9")) %>%
    mutate(bfacil = replace(bfacil, is.na(bfacil), "9")) %>%
    mutate(bfacil3 = replace(bfacil3, is.na(bfacil3), "3")) %>%
    mutate(bmi_r = replace(bmi_r, is.na(bmi_r), "9")) %>%
    mutate(dlmp_mm = replace(dlmp_mm, is.na(dlmp_mm), "99")) %>%
    mutate(dmeth_rec = replace(dmeth_rec, is.na(dmeth_rec), "9")) %>%
    mutate(fbrace = replace(fbrace, is.na(fbrace), "9")) %>%
    mutate(feduc = replace(feduc, is.na(feduc), "9")) %>%
    mutate(fhisp_r = replace(fhisp_r, is.na(fhisp_r), "9")) %>%
    mutate(gestrec10 = replace(gestrec10, is.na(gestrec10), "99")) %>%
    mutate(gestrec3 = replace(gestrec3, is.na(gestrec3), "3")) %>%
    mutate(illb_r11 = replace(illb_r11, is.na(illb_r11), "99")) %>%
    mutate(ilop_r11 = replace(ilop_r11, is.na(ilop_r11), "99")) %>%
    mutate(ilp_r11 = replace(ilp_r11, is.na(ilp_r11), "99")) %>%
    mutate(lbo_rec = replace(lbo_rec, is.na(lbo_rec), "9")) %>%
    mutate(meduc = replace(meduc, is.na(meduc), "9")) %>%
    mutate(mhisp_r = replace(mhisp_r, is.na(mhisp_r), "9")) %>%
    mutate(pay = replace(pay, is.na(pay), "9")) %>%
    mutate(pay_rec = replace(pay_rec, is.na(pay_rec), "9")) %>%
    mutate(precare = replace(precare, is.na(precare), "99")) %>%
    mutate(previs_rec = replace(previs_rec, is.na(previs_rec), "12")) %>%
    mutate(rdmeth_rec = replace(rdmeth_rec, is.na(rdmeth_rec), "9")) %>%
    mutate(setorder_r = replace(setorder_r, is.na(setorder_r), "9")) %>%
    mutate(tbo_rec = replace(tbo_rec, is.na(tbo_rec), "9")) %>%
    mutate(wtgain_rec = replace(wtgain_rec, is.na(wtgain_rec), "9"))

# response
natl2011 <- natl2011 %>%
    mutate_at("lowbwt", funs(factor))

# save rdata
save(natl2011, file = "natl2011.RData")




########## test sample ##########
# read data
testsample <- fread("testsample.csv")

# categorical variables
c_var <- c("attend", "bfacil", "bfacil3", "bmi_r", "cig_rec", 
           "dlmp_mm", "dmar", "dmeth_rec", "dob_mm", "dob_wk", "dplural", 
           "f_cigs_0", "f_cigs_1", "f_cigs_2", "f_cigs_3", "f_dwgt", "f_facility", "f_ip_chlam", 
           "f_ip_gonor", "f_ip_hepatb", "f_ip_hepatc", "f_ip_syph", "f_m_ht", "f_mar_p", "f_meduc", 
           "f_mpcb", "f_ob_fail", "f_ob_succ", "f_pay", "f_pay_rec", "f_pwgt", "f_rf_cesar", 
           "f_rf_eclamp", "f_rf_gdiab", "f_rf_ghyper", "f_rf_inf_art", "f_rf_ncesar", 
           "f_rf_pdiab", "f_rf_phyper", "f_rf_ppb", "f_tobaco", "f_tpcv", "f_wic", "f_wtgain", 
           "fagerec11", "fbrace", "feduc", "fhisp_r", "gestrec10", "gestrec3", 
           "illb_r11", "ilop_r11", "ilp_r11", "ip_chlam", "ip_gon", "ip_hepatb", 
           "ip_hepatc", "ip_syph", "lbo_rec", "ld_anes", "ld_antb", "ld_augm", "ld_chor", "ld_indl", 
           "ld_ster", "mager14", "mager9", "mar_p", "mbrace", "me_pres", "me_rout", 
           "me_trial", "meduc", "mhisp_r", "mm_aicu", "mm_mtr", "mm_plac", "mm_rupt", "mm_uhyst", 
           "mtran", "pay", "pay_rec", "precare", "previs_rec", "rdmeth_rec", 
           "restatus", "rf_artec", "rf_cesar", "rf_fedrg", "rf_inftr", "rf_ppterm", 
           "setorder_r", "sex", "tbo_rec", "wic", "wtgain_rec")
# numeric variables
n_var <- c("bmi", "cig_0", "cig_1", "cig_2", "cig_3", "combgest", "dwgt_r", "fagecomb", "illb_r", "ilop_r",
           "m_ht_in", "mager", "priorterm", "pwgt_r", "rf_cesarn", "wtgain")


# deal with NA in numerical variables
testsample <- testsample %>%
    mutate(bmi = replace(bmi, bmi == 99.9, NA)) %>%
    mutate(cig_0 = replace(cig_0, cig_0 == 99, NA)) %>%
    mutate(cig_1 = replace(cig_1, cig_1 == 99, NA)) %>%
    mutate(cig_2 = replace(cig_2, cig_2 == 99, NA)) %>%
    mutate(cig_3 = replace(cig_3, cig_3 == 99, NA)) %>%
    mutate(combgest = replace(combgest, combgest == 99, NA)) %>%
    mutate(dwgt_r = replace(dwgt_r, dwgt_r == 999, NA)) %>%
    mutate(fagecomb = replace(fagecomb, fagecomb == 99, NA)) %>%
    mutate(m_ht_in = replace(m_ht_in, m_ht_in == 99, NA)) %>%
    mutate(priorterm = replace(priorterm, priorterm == 99, NA)) %>%
    mutate(pwgt_r = replace(pwgt_r, pwgt_r == 999, NA)) %>%
    mutate(rf_cesarn = replace(rf_cesarn, rf_cesarn == 99, NA)) %>%
    mutate(wtgain = replace(wtgain, wtgain == 99, NA)) %>%
    mutate(illb_r = replace(illb_r, illb_r == 888, NA)) %>%
    mutate(illb_r = replace(illb_r, illb_r == 999, NA)) %>%
    mutate(ilop_r = replace(ilop_r, ilop_r == 888, NA)) %>%
    mutate(ilop_r = replace(ilop_r, ilop_r == 999, NA))



# convert data type
testsample <- testsample %>% 
  mutate_at(c_var, funs(factor)) %>% 
  mutate_at(n_var, funs(as.numeric))


# save rdata
save(testsample, file = "testsample.RData")






