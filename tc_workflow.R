rm(list = ls())

#load("C:/Users/mfarr/Documents/R_files/Spotfire.data/typecurves.RData")
resource <- na.omit(read.csv("etc_strip_8_7.csv", stringsAsFactors = FALSE))
tc_br <- na.omit(read.csv("briscoe_tc.csv", stringsAsFactors = FALSE))
tc_gr <- na.omit(read.csv("galvan_tc.csv", stringsAsFactors = FALSE))


##  load packages=============================================================================
suppressWarnings(library(dplyr,warn.conflicts=FALSE))
#suppressWarnings(library(broom, warn.conflicts = FALSE))
#suppressWarnings(library(tidyr, warn.conflicts = FALSE))
suppressWarnings(library(RODBC, warn.conflicts = FALSE))
suppressWarnings(library(stringr, warn.conflicts = FALSE))
#suppressWarnings(library(purrr,warn.conflicts=FALSE))
#suppressWarnings(library(tidyr,warn.conflicts=FALSE))

##  define functions=========================================================================

##  function to id date time columes in data tables
is.POSIXt <- function(x) inherits(x, "POSIXt")

##  function to calc the end of the month
month_end <- function(dt) {
  dt <- as.Date(dt)
  d <- seq(dt, dt+31, by="days")
  max(d[format(d,"%m")==format(dt,"%m")])
}

clean_dt <- function(x) {
  x <- data.frame(sapply(x, function(x) gsub("\\s+|-|/", " ", x)))
  mutate_if(x, is.factor, as.character) -> x
}

  
##  data cleanup=============================================================================

## get a list of data.frames
file.names<-ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']

## remove unwanted symbols in colnames
for(i in 1:length(file.names)){
  
  tmp<- get(file.names[i])
  names(tmp)<-gsub("-","", names(tmp))
  names(tmp)<-gsub(" ", "_", names(tmp))
  names(tmp)<-gsub("[()]", "", names(tmp))
  names(tmp)<-gsub("/","_",  names(tmp))
  names(tmp)<-gsub("\\%", "", names(tmp))
  assign(file.names[i], tmp)
  rm(tmp)
  
}

##  for loop to remove all attribute (except for datetime columes)
for(i in seq_along(file.names)){
  tmp <- get(file.names[i])
  
  if(length(which(sapply(tmp, is.POSIXt)))!=0){
    tmp_dates <- which(sapply(tmp, is.POSIXt));
    tmp2 <- tmp[tmp_dates]
    tmp <- tmp[-tmp_dates]
    
    for(j in seq_along(colnames(tmp))){
      attributes(tmp[[j]]) <- NULL
    }
    tmp <- bind_cols(tmp2,tmp)
    assign(file.names[i], tmp)
    rm(tmp, tmp2)
  }else{
    for(j in seq_along(colnames(tmp))){
      attributes(tmp[[j]]) <- NULL
    }
    assign(file.names[i], tmp)
    rm(tmp)
  }
}

##  build resource model table===========================================

##  lateral length bins for tbl cleanup
lat_lngth <- c("7000", "10000", "13000", "16000")
##  all the unique scenario names
scenario_names <- unique(sort(resource$Scenario))

#i<-1
##  for loop to transpose data tbl
dt_res <- data.frame()
for(i in seq_along(lat_lngth)){
  
  indx <- grepl(lat_lngth[i], colnames(resource));
  tmp_res <- resource[1:3]
  tmp_res <- cbind(tmp_res, resource[indx]);
  tmp_res$eff_lat <- lat_lngth[i];
  names(tmp_res) <- c("scenario", "tc_sub_region", "corpcall_dec", 
                      "well_count", "spacing", "eff_lat");
  dt_res <- rbind(dt_res, tmp_res);
  
}

## clean data in friendly format
dt_res <- clean_dt(dt_res)

res_spacing_names <- unique(dt_res$spacing)
res_sub_names <- unique(dt_res$tc_sub_region)
res_zone_lower <- res_spacing_names[grepl("LOWER", res_spacing_names)]
res_zone_int <- res_spacing_names[grepl("INT", res_spacing_names)]
res_h2s <- res_sub_names[grepl("H2S", res_sub_names)]
res_spacing_cat <- c("625", "900", "1250")
res_h_tc_region <- c("H_EAST", "H_WEST")
res_mod_awest <- c("A2W", "A4W", "A6W")
res_mod_aeast <- c("A4E", "A6E", "A10E")
res_cc_dec_no_zone <- c("AREAE", "AREAF", "AREAG", "AREAK", "AREAHJ")
  
  
##  build resource model table
res_mod <- dt_res %>% ##  fix h2s column
  filter(tc_sub_region %in% res_h2s &
           !spacing %in% res_zone_int) %>%
  mutate(h2s = "H2S") %>%
  bind_rows(dt_res %>%##  fix noh2s column
              filter(!tc_sub_region %in% res_h2s) %>%
              mutate(h2s = "NOH2S")) %>%
  bind_rows(dt_res %>%
              filter(spacing %in% res_zone_int) %>%
              mutate(h2s = "NOH2S")) %>%
  mutate(reservoir = if_else(word(spacing, 2) %in% res_spacing_cat, "UEF", 
                        word(spacing, 2)), 
         spacing = word(spacing, 1),
         zone = case_when(reservoir == "LOWER" ~ "LEF", 
                          reservoir == "INT" ~ "IEF", 
                          TRUE ~ "UEF"),
         tc_sub =  case_when(tc_sub_region %in% "SA_A" ~ "H_WEST",
                             tc_sub_region %in% "D" ~ "D0",
                             TRUE ~ word(tc_sub_region, 1)), 
         ## fixed tc region to ariesMaster
         tc_region = case_when(tc_sub %in% res_mod_aeast ~ "A_EAST",
                               tc_sub %in% res_mod_awest ~ "A_WEST",
                               tc_sub %in% res_h_tc_region & 
                                 !tc_sub_region %in% "SA_A" ~ tc_sub_region,
                               tc_sub_region %in% "SA_A" ~ "SAA",
                               TRUE ~ substr(word(tc_sub_region, 1), start = 1, stop = 1)), 
         cc_dec = case_when(corpcall_dec %in% res_cc_dec_no_zone ~ corpcall_dec,
                            zone %in% "IEF" ~ paste0(corpcall_dec, "_", "LEF"),
                            TRUE ~ paste0(corpcall_dec, "_", zone)), 
         well_count = as.numeric(well_count)) %>% 
  filter(well_count > 0) %>%
  ##  duplicate rows based on well_count
  slice(rep(seq_len(n()), well_count)) %>%
  mutate(well_count = 1, 
         tc_name = paste0(tc_region, "_", tc_sub, "_", zone, "_", h2s, "_", 
                          cc_dec, "_",eff_lat, "_", spacing)) %>%
  select(scenario, tc_name, reservoir, zone, tc_region, tc_sub, h2s, 
         cc_dec, eff_lat, spacing, well_count) %>%
  mutate(tc_lookup = case_when(tc_region == "A_WEST" | tc_region == "A_EAST" ~ "A", 
                               tc_region == "SAA" | tc_region == "H_EAST" | tc_region == "H_WEST" |
                                 tc_region == "F" | tc_region == "K" ~ "KFH", 
                               tc_region == "D" | tc_region == "B" | tc_region == "E" ~ "DBE", 
                               TRUE ~ tc_region), 
         tc_zone = case_when(zone == "LEF" | zone == "IEF" ~ paste0(tc_lookup,"-","LOWER"), 
                             TRUE ~ paste0(tc_lookup, "-", "UPPER")), 
         team = case_when(tc_lookup == "A" | tc_lookup == "C" ~ "GALVEF", 
                          TRUE ~ "BRISEF"), 
         frac_design = case_when(tc_region == "A_WEST" | tc_region == "A_EAST" ~ "SW_420_FULL_PRIME", 
                                 tc_region == "C" ~ "SW_340_FULL_PRIME", 
                                 tc_region == "BRISA" ~ "SSW_525_FULL_PRIME", 
                                 TRUE ~ "SW_540_FULL_PRIME")) %>%
  arrange(scenario, tc_region)

res_mod$tc_sub <- str_replace(res_mod$tc_sub, "A6W", "A4W")
res_mod$tc_name <- str_replace(res_mod$tc_name, "A6W", "A4W")
res_mod$tc_sub <- str_replace(res_mod$tc_sub, "A2W", "A6W")
res_mod$tc_name <- str_replace(res_mod$tc_name, "A2W", "A6W")
res_mod$tc_sub <- str_replace(res_mod$tc_sub, "A6E", "A10E")
res_mod$tc_name <- str_replace(res_mod$tc_name, "A6E", "A10E")
res_mod$tc_sub <- str_replace(res_mod$tc_sub, "A4E", "A6E")
res_mod$tc_name <- str_replace(res_mod$tc_name, "A4E", "A6E")



#write.csv(res_mod, file = "res_mod.csv")


##  briscoe lookup tbl===================================================

br_lease_names <- unique(tc_br$LEASE)
br_saa <- br_lease_names[grepl("SA A", br_lease_names)]
br_h_east <- br_lease_names[grepl("H-EAST", br_lease_names)& !grepl("SA A", br_lease_names)]
br_f_west <- br_lease_names[grepl("F-WEST", br_lease_names)]
br_h_west <- br_lease_names[grepl("H-WEST", br_lease_names) & !grepl("SA A", br_lease_names)]
br_cc_3.7 <- br_lease_names[grepl("AREAD_BR_3.7", br_lease_names)]
br_tc_match <- c("B3", "B10", "B20", "B40", "D0")
br_bd <- br_lease_names[grepl(paste(br_tc_match, collapse = "|"), br_lease_names)]
br_areab_6 <- br_lease_names[grepl("AREAB_6.0", br_lease_names)]
br_d <- br_lease_names[grepl("D0", br_lease_names)]


briscoe_lease_names <- tc_br %>%
  filter(LEASE %in% br_areab_6) %>%
  distinct(LEASE) %>%
  mutate(L1 = gsub("-", " ", LEASE), 
         L2 = gsub("/", " ", L1),
         tc_region = substr(word(L2, 2), start = 1, stop = 1),
         tc_sub = word(L2, 2),
         eff_lat = word(L2, -1), 
         zone = word(L2, 3), 
         spacing = word(L2,4), 
         h2s = if_else(grepl("H2S", L2), "H2S", "NOH2S"),
         cc_dec = paste0("AREAB_6","_",word(L2, 3))) %>%
  bind_rows(tc_br %>%
              filter(LEASE %in% br_cc_3.7) %>%
              distinct(LEASE) %>%
              mutate(L1 = gsub("-", " ", LEASE), 
                     L2 = gsub("/", " ", L1),
                     tc_region = substr(word(L2, 2), start = 1, stop = 1),
                     tc_sub = word(L2, 2),
                     eff_lat = word(L2, -1), 
                     zone = word(L2, 3), 
                     spacing = word(L2,4), 
                     h2s = if_else(grepl("H2S", L2), "H2S", "NOH2S"),
                     cc_dec = paste0(word(L2, -2),"_",word(L2, 3)))) %>%
  bind_rows(tc_br %>% 
              filter(LEASE %in% br_d & !LEASE %in% br_cc_3.7 & !LEASE %in% br_areab_6) %>%
              distinct(LEASE) %>%
              mutate(L1 = gsub("-", " ", LEASE), 
                     L2 = gsub("/", " ", L1),
                     tc_region = substr(word(L2, 2), start = 1, stop = 1),
                     tc_sub = word(L2, 2),
                     eff_lat = word(L2, -1), 
                     zone = word(L2, 3), 
                     spacing = word(L2,4), 
                     h2s = if_else(grepl("H2S", L2), "H2S", "NOH2S"),
                     cc_dec = paste0(word(L2, -2),"_",word(L2, 3)))) %>%
  bind_rows(tc_br %>% 
              filter(LEASE %in% br_saa) %>%
              distinct(LEASE) %>%
              mutate(L1 = gsub("-", " ", LEASE), 
                     L2 = gsub("/", " ", L1),
                     tc_region = "SAA",
                     tc_sub = paste0(word(L2,4),"_",word(L2,5)),
                     eff_lat = word(L2, -1), 
                     zone = word(L2, 6), 
                     spacing = word(L2,7), 
                     h2s = if_else(grepl("H2S", L2), "H2S", "NOH2S"),
                     cc_dec = word(L2, -2))) %>%
  bind_rows(tc_br %>% 
              filter(LEASE %in% br_h_east) %>%
              distinct(LEASE) %>%
              mutate(L1 = gsub("-", " ", LEASE), 
                     L2 = gsub("/", " ", L1),
                     tc_region = paste0(word(L2, 2),"_",word(L2,3)),
                     tc_sub = tc_region,
                     eff_lat = word(L2, -1), 
                     zone = word(L2, 4), 
                     spacing = word(L2,5), 
                     h2s = if_else(grepl("H2S", L2), "H2S", "NOH2S"),
                     cc_dec = word(L2, -2))) %>%
  bind_rows(tc_br %>% 
              filter(LEASE %in% br_f_west) %>%
              distinct(LEASE) %>%
              mutate(L1 = gsub("-", " ", LEASE), 
                     L2 = gsub("/", " ", L1),
                     tc_region = substr(word(L2, 2), start = 1, stop = 1),
                     tc_sub = word(L2, 2),
                     eff_lat = word(L2, -1), 
                     zone = word(L2, 3), 
                     spacing = word(L2,4), 
                     h2s = if_else(grepl("H2S", L2), "H2S", "NOH2S"),
                     cc_dec = word(L2, -3))) %>%
  bind_rows(tc_br %>% 
              filter(LEASE %in% br_h_west) %>%
              distinct(LEASE) %>%
              mutate(L1 = gsub("-", " ", LEASE), 
                     L2 = gsub("/", " ", L1),
                     tc_region = paste0(word(L2, 2),"_",word(L2,3)),
                     tc_sub = tc_region,
                     eff_lat = word(L2, -1), 
                     zone = word(L2, 4), 
                     spacing = word(L2,5), 
                     h2s = if_else(grepl("H2S", L2), "H2S", "NOH2S"),
                     cc_dec = paste0(word(L2, -2)))) %>%
  bind_rows(tc_br %>% 
            filter(!LEASE %in% br_f_west & !LEASE %in% br_h_east & 
                     !LEASE %in% br_saa & !LEASE %in% br_h_west & 
                     !LEASE %in% br_bd & !LEASE %in% br_cc_3.7 & !LEASE %in% br_areab_6) %>%
            distinct(LEASE) %>%
            mutate(L1 = gsub("-", " ", LEASE), 
                   L2 = gsub("/", " ", L1),
                   tc_region = substr(word(L2, 2), start = 1, stop = 1),
                   tc_sub = word(L2, 2),
                   eff_lat = word(L2, -1), 
                   zone = word(L2, 3), 
                   spacing = word(L2,4), 
                   h2s = if_else(grepl("H2S", L2), "H2S", "NOH2S"),
                   cc_dec = word(L2, -2))) %>% 
  select(LEASE, tc_region, tc_sub, eff_lat, zone, spacing, h2s, cc_dec) %>%
  mutate(TEAM = "BRISCOE")

##  galvan lookup tbl===================================================

lease_gr <- unique(tc_gr$LEASE)
a_west <- lease_gr[grepl("W", lease_gr)]
tc_match <- c("A12E", "A10E", "A8E", "A6E", "A4E")
a_east <- lease_gr[grepl(paste(tc_match, collapse = "|"), lease_gr)]

galvan_tc_name <- tc_gr %>% 
  filter(LEASE %in% a_west) %>%
  distinct(LEASE) %>%
  mutate(L1 = gsub("-", " ", LEASE), 
         L2 = gsub("/", " ", L1),
         tc_region = "A_WEST",
         tc_sub = word(L2, 2), 
         zone = word(L2, 3),
         eff_lat = word(L2, -1),
         spacing = word(L2, 4),
         h2s = if_else(grepl("H2S", L2), "H2S", "NOH2S"),
         cc_dec = if_else(word(L2, 3) == "IEF", word(L2, -2),
                          paste0(word(L2, -2), "_", word(L2, 3)))) %>%
  bind_rows(tc_gr %>% 
              filter(LEASE %in% a_east) %>%
              distinct(LEASE) %>%
              mutate(L1 = gsub("-", " ", LEASE), 
                     L2 = gsub("/", " ", L1),
                     tc_region = "A_EAST",
                     tc_sub = word(L2, 2), 
                     zone = word(L2, 3),
                     eff_lat = word(L2, -1),
                     spacing = word(L2, 4),
                     h2s = if_else(grepl("H2S", L2), "H2S", "NOH2S"),
                     cc_dec = if_else(word(L2, 3) == "IEF", word(L2, -2),
                                      paste0(word(L2, -2), "_", word(L2, 3))))) %>%
  bind_rows(tc_gr %>%
              filter(!LEASE %in% a_east & !LEASE %in% a_west) %>%
              distinct(LEASE) %>%
              mutate(L1 = gsub("-", " ", LEASE), 
                     L2 = gsub("/", " ", L1),
                     tc_region = substr(word(L2, 2), start = 1, stop = 1),
                     tc_sub = word(L2, 2), 
                     zone = word(L2, 4),
                     eff_lat = word(L2, -1),
                     spacing = word(L2, 5),
                     h2s = if_else(grepl("H2S", L2), "H2S", "NOH2S"),
                     cc_dec = if_else(word(L2, 3) == "IEF", word(L2, -2),
                                      paste0(word(L2, -2), "_", word(L2, 4))))) %>%
  select(LEASE, tc_region, tc_sub, eff_lat, zone, spacing, h2s, cc_dec) %>%
  mutate(TEAM = "GALVAN")


comb_tc_names <- bind_rows(briscoe_lease_names, galvan_tc_name) %>%
  mutate(tc_name = paste0(tc_region, "_", tc_sub, "_", zone, "_", 
                          h2s, "_", cc_dec, "_",eff_lat, "_", spacing),
         id = row_number())

##  join comb tc and res_mod to get id=============================

res_mod$id <- comb_tc_names[match(res_mod$tc_name, comb_tc_names$tc_name),11]

#na_count <- sapply(res_mod, function(y) sum(is.na(y)))
##  create tc lookup tbl============================================

tc_comb <- bind_rows(tc_br, tc_gr)

prop <- unique(tc_comb$PROPNUM)

#i<-1
tc_lookup <- data.frame()
for(i in seq_along(prop)){
  tc_tmp <- subset(tc_comb, PROPNUM == prop[i]);
  LEASE <- tc_tmp[1,16];
  BU_Q1 <- tc_tmp[2,2]
  BU_T1 <- tc_tmp[2,5];
  BU_D1 <- tc_tmp[2,8];
  EXP_Q1 <- tc_tmp[3,2];
  EXP_T1 <- tc_tmp[3,5];
  EXP_D1 <- tc_tmp[3,8];
  HYP_T1 <- tc_tmp[4,5];
  HYP_B1 <- tc_tmp[4,7];
  HYP_D1 <- tc_tmp[4,8];
  HYP_Q4 <- tc_tmp[5,3];
  HYP_DMIN <- tc_tmp[5,8];
  if(tc_tmp[6,1]=="OIL/GAS"){
    YIELD_Q1 <- tc_tmp[6,2];
    YIELD_Q2 <- tc_tmp[6,3];
    YIELD_T1 <- tc_tmp[6,5];
    YIELD_Q3 <- tc_tmp[7,2];
    YIELD_Q4 <- tc_tmp[7,3];
  }else{
    YIELD_Q1 <- 0;
    YIELD_Q2 <- 0;
    YIELD_T1 <- 0;
    YIELD_Q3 <- 0;
    YIELD_Q4 <- 0;
    
  }


  tc_tmp2 <- data.frame(LEASE, BU_Q1, BU_T1, BU_D1, EXP_Q1, EXP_T1, EXP_D1, 
                        HYP_T1, HYP_B1, HYP_D1, HYP_Q4, HYP_DMIN, 
                        YIELD_Q1, YIELD_Q2, YIELD_T1, YIELD_Q3, YIELD_Q4)
  
  tc_lookup <- rbind(tc_lookup, tc_tmp2);
  
  mutate_if(tc_lookup, is.factor, as.character) -> tc_lookup
  
}

tc_lookup <- left_join(tc_lookup, 
                       comb_tc_names %>%
                         select(LEASE, id, tc_name), by = "LEASE") %>%
  na.omit() %>%
  select(tc_name, id, BU_Q1, BU_T1, BU_D1, EXP_Q1, EXP_T1, EXP_D1,
         HYP_T1, HYP_B1, HYP_D1, HYP_Q4, HYP_DMIN, YIELD_Q1, YIELD_Q2,
         YIELD_T1, YIELD_Q3, YIELD_Q4)
  

#na_count <- sapply(tc_lookup, function(y) sum(is.na(y)))
##  open connect to access==========================================

##doc property
#AccessFilePath <- "C:/Users/mfarr/Documents/Aries db/etc_resource_model.accdb"
AccessFilePath <- "N:/Dept/Prod/Aries/Db/Houston/2019 Projects/ETC Renegotiation WIG/etc_resource_model.accdb"
driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
dLocation <- AccessFilePath
ch <- odbcDriverConnect(paste(driver,';DBQ=',dLocation))


##  check if table already exist====================================
#check_tbl <- c("TcCum", "TcForecast", "TcParameters", "TcWellList")
access_tbl <- sqlTables(ch)[3]


##  load table from access==========================================
AC_PROPERTY <- sqlQuery(ch, paste("select * from AC_PROPERTY"))
#AC_ECONOMIC <- sqlQuery(ch, paste("select * from AC_ECONOMIC"))
#ARLOOKUP <- sqlQuery(ch, paste("select * from ARLOOKUP"))

close(ch)

i <- sapply(AC_PROPERTY, is.factor)
AC_PROPERTY[i] <- lapply(AC_PROPERTY[i], as.character)

ac_p <- AC_PROPERTY %>% 
  filter(CORP10 == "UPSIDE") %>%
  slice(1:nrow(res_mod))

res_mod$PROPNUM <- ac_p$PROPNUM
res_mod$id <- as.character(res_mod$id)

##  ac_property key
##  corp13 = scenario | corp14 = tc_name | corp15 = tc_zone

ac_p$CORP12 <- res_mod[match(ac_p$PROPNUM, res_mod$PROPNUM),16]
ac_p$CORP13 <- res_mod[match(ac_p$PROPNUM, res_mod$PROPNUM),1]
ac_p$CORP14 <- res_mod[match(ac_p$PROPNUM, res_mod$PROPNUM),2]
ac_p$CORP15 <- res_mod[match(ac_p$PROPNUM, res_mod$PROPNUM),13]
ac_p$TEAM <- res_mod[match(ac_p$PROPNUM, res_mod$PROPNUM), 14]
ac_p$EFF_LAT <- as.numeric(res_mod[match(ac_p$PROPNUM, res_mod$PROPNUM), 9])
ac_p$ZONE <- res_mod[match(ac_p$PROPNUM, res_mod$PROPNUM), 4]
ac_p$WELL_SPACING_PLAN_VIEW <- as.numeric(res_mod[match(ac_p$PROPNUM, res_mod$PROPNUM),10])
ac_p$TYPE_CURVE_REGION <- res_mod[match(ac_p$PROPNUM, res_mod$PROPNUM), 5]
ac_p$TYPE_CURVE_SUBREGION <- res_mod[match(ac_p$PROPNUM, res_mod$PROPNUM), 6]
ac_p$CORPCALL_DEC <- res_mod[match(ac_p$PROPNUM, res_mod$PROPNUM),8]
ac_p$FRAC_DESIGN <- res_mod[match(ac_p$PROPNUM, res_mod$PROPNUM),15]

AC_PROPERTY <- bind_rows(AC_PROPERTY %>%
                           filter(CORP10 != "UPSIDE"), 
                         ac_p)

columnTypes <- list(OIL_DIF_DATE="datetime", DATE_COMP="datetime", FIRST_PROD="datetime", INC_END_DATE="datetime", PROP_SPUD="datetime", 
                    PROP_CMPL="datetime", PROP_SALES="datetime", PROP_TBG="datetime", PROP_AL="datetime", LAST_UPDATE_DATE="datetime")
     


ch <- odbcDriverConnect(paste(driver,';DBQ=',dLocation))

sqlDrop(ch, "AC_PROPERTY")
sqlSave(ch, AC_PROPERTY, tablename = "AC_PROPERTY", rownames = FALSE,
        varTypes = columnTypes, append = FALSE)
close(ch)

##  end of script===================================================

#write.csv(dt_res, file = "dt_res.csv")
#write.csv(dtx, file = "dtx.csv")
#write.csv(briscoe_lease_names, file = "br_tc.csv")
write.csv(tc_lookup, file = "tc_lookup.csv")
write.csv(comb_tc_names, file = "tc_names.csv")
write.csv(res_mod, file = "res_mod.csv")


res_mod %>%
  group_by(scenario) %>%
  summarise(sum(well_count))


