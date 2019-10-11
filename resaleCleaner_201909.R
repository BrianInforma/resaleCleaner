# ===============================================================================================================================
# ===============================================================================================================================
# Resale Files Formatting
# ===============================================================================================================================
# ===============================================================================================================================

# ===============================================================================================================================
# Read in data
# ===============================================================================================================================

library(plyr)
library(data.table)
library(stringr)
library(zoo)
library(lubridate)
library(stringi)
library(tools)
#install.packages("countrycode")
library(countrycode)
library(readr)

dirIn  <-("C:\\Users\\McVayN\\OneDrive - Informa plc\\Documents\\Resale\\2019\\2019_9\\Raw_09232019\\")
dirOut <-("C:\\Users\\McVayN\\OneDrive - Informa plc\\Documents\\Resale\\2019\\2019_9\\Clean_09232019\\")

file_list <- paste0(dirIn, list.files(path = dirIn))
file_list2<-file_list[file_list %like% 'EquipmentTrader']
file_list3<-file_list[file_list %like% 'CommercialTruckTrader']
file_list<-file_list[!file_list %like% 'EquipmentTrader'& !file_list %like% 'CommercialTruckTrader']
ds = rbindlist(lapply(file_list, fread, header=TRUE, sep =","),fill=TRUE)
et = rbindlist(lapply(file_list2, fread, header=TRUE, sep =","),fill=TRUE)
ct = rbindlist(lapply(file_list3, fread, header=TRUE, sep =","),fill=TRUE)
# ===============================================================================================================================
# Column Name Correction (LocationUrl and ASSETURL)
# ===============================================================================================================================
if("LocationUrl" %in% names(ds) & unique(!is.na(ds$LocationUrl))==TRUE){
  ds$ASSETURL<-NULL
  names(ds)[names(ds)=="LocationUrl"]<-"ASSETURL"
}

# ===============================================================================================================================
# Set Column Name
# ===============================================================================================================================
if(length(ds)>=26){
ds<-ds[,c(
  "Created",
  "SURVEYTYPE",
  "PARTICIPANTCODE",
  "SELLERNAME",
  "SELLERADDRESS",
  "SELLERCITY",
  "SELLERSTATE",
  "SELLERZIP",
  "SELLERCOUNTRY",
  "CATEGORY",
  "AUCTIONSTARTDATE",
  "AUCTIONENDDATE",
  "BIDS",
  "PRICE",
  "CURRENCY",
  "LOTNUMBER",
  "MANUFACTURER",
  "MODEL",
  "DESCRIPTION",
  "YEAR",
  "SERIALNUMBER",
  "METERREADS",
  "CONDITION",
  "ASSETURL",
  "STOCKNUMBER",
  "SOURCE")]}
if(length(et)>=26){
et<-et[,c(
  "Created",
  "SURVEYTYPE",
  "PARTICIPANTCODE",
  "SELLERNAME",
  "SELLERADDRESS",
  "SELLERCITY",
  "SELLERSTATE",
  "SELLERZIP",
  "SELLERCOUNTRY",
  "CATEGORY",
  "AUCTIONSTARTDATE",
  "AUCTIONENDDATE",
  "BIDS",
  "PRICE",
  "CURRENCY",
  "LOTNUMBER",
  "MANUFACTURER",
  "MODEL",
  "DESCRIPTION",
  "YEAR",
  "SERIALNUMBER",
  "METERREADS",
  "CONDITION",
  "ASSETURL",
  "STOCKNUMBER",
  "SOURCE")]}
if(length(ct)>=26){
ct<-ct[,c(
  "Created",
  "SURVEYTYPE",
  "PARTICIPANTCODE",
  "SELLERNAME",
  "SELLERADDRESS",
  "SELLERCITY",
  "SELLERSTATE",
  "SELLERZIP",
  "SELLERCOUNTRY",
  "CATEGORY",
  "AUCTIONSTARTDATE",
  "AUCTIONENDDATE",
  "BIDS",
  "PRICE",
  "CURRENCY",
  "LOTNUMBER",
  "MANUFACTURER",
  "MODEL",
  "DESCRIPTION",
  "YEAR",
  "SERIALNUMBER",
  "METERREADS",
  "CONDITION",
  "ASSETURL",
  "STOCKNUMBER",
  "SOURCE")]}

#===============================================================================================================================
# Manufacturer/Model Parsing for EquipmentTrader
# ==============================================================================================================================
if(length(et)>=26){
et$MODEL<-gsub("John Deere Construction", "DEERE",et$MODEL)
et$MODEL<-gsub("GENIE INDUSTRIES", "GENIE",et$MODEL)
et$MODEL<-gsub("H and H Trailers", "H & H",et$MODEL)
et$MODEL<-gsub("H and S", "H & S",et$MODEL)
et$MODEL<-gsub("J and J", "J & J",et$MODEL)
et$MODEL<-gsub("J and L", "J&J TRAILERS",et$MODEL)
et$MODEL<-gsub("J and M", "J&M",et$MODEL)
et$MODEL<-gsub("R and R", "R&R",et$MODEL)
et$MODEL<-gsub("Hitachi Construction", "HITACHI",et$MODEL)
et$MODEL<-gsub("Homesteader Trailers", "HOMESTEADER",et$MODEL)
et$MODEL<-gsub("Honda Power Equipment", "HONDA POWER EQUIP",et$MODEL)
et$MODEL<-gsub("Husqvarna Construction", "HUSQVARNA",et$MODEL)
et$MODEL<-gsub("ROBEX ", "R",et$MODEL)
et$MODEL<-gsub("Hyundai Construction", "HYUNDAI",et$MODEL)
et$MODEL<-gsub("KUHN KNIGHT", "KUHN",et$MODEL)
et$MODEL<-gsub("KUHN KRAUSE", "KUHN",et$MODEL)
et$MODEL<-gsub("Lamar Trailers", "LAMAR",et$MODEL)
et$MODEL<-gsub("LIFT-ALL|Lift-All", "LIFTALL",et$MODEL)
et$MODEL<-gsub("Vantage ", "Vantage",et$MODEL)
et$MODEL<-gsub("Mirage Trailers", "MIRAGE",et$MODEL)
et$MODEL<-gsub("Moritz International", "MORITZ",et$MODEL)
et$MODEL<-gsub("New Holland Agriculture", "NEW HOLLAND",et$MODEL)
et$MODEL<-gsub("HDT ", "HDT-",et$MODEL)
et$MODEL<-gsub("MDT ", "MDT-",et$MODEL)
et$MODEL<-gsub("NOVACAT ", "NOVACAT-",et$MODEL)
et$MODEL<-gsub("WARRIOR ", "WARRIOR-",et$MODEL)
et$MODEL<-gsub("CHEIFTAIN ", "CHEIFTAIN-",et$MODEL)
et$MODEL<-gsub("Quality Trailers", "QUALITY",et$MODEL)
et$MODEL<-gsub("SJIII ", "SJIII",et$MODEL)
et$MODEL<-gsub("Stealth Trailers", "STEALTH ENTERPRISES",et$MODEL)
et$MODEL<-gsub("Take 3", "TAKE3",et$MODEL)
et$MANUFACTURER<-gsub("TSE INTERNATIONAL", "TSE",et$MANUFACTURER)
et$MODEL<-gsub("Bc 1000xl", "BC1000XL",et$MODEL)
et$MODEL<-gsub("NAVIGATOR ", "NAVIGATOR-",et$MODEL)
et$MODEL<-gsub("HYDRAULIC EXCAVATOR ", "",et$MODEL)
et$MODEL<-gsub("SKY TRAK", "SKYTRAK",et$MODEL)
et$MODEL<-gsub("Sundowner Trailers", "SUNDOWNER TRAILER",et$MODEL)
et$MODEL<-toupper(et$MODEL)
et$MODEL<-gsub(" XL SERIES III", "-XL-SERIES-III",et$MODEL)
et$MODEL<-gsub(" XL SERIES II", "-XL-SERIES-II",et$MODEL)
et$MODEL<-gsub(" SERIES III", "-SERIES-III",et$MODEL)
et$MODEL<-gsub(" SERIES II", "-SERIES-II",et$MODEL)
et$MODEL<-gsub(" DS XL III", "-DS-XL-III",et$MODEL)
et$MODEL<-gsub(" DS XL II", "-DS-XL-II",et$MODEL)
et$MODEL<-gsub(" DS XL", "-DS-XL",et$MODEL)
et$MODEL<-gsub(" HST III", "-HST-III",et$MODEL)
et$MODEL<-gsub(" HST II", "-HST-II",et$MODEL)
et$MODEL<-gsub(" HST", "-HST",et$MODEL)
et$MODEL<-gsub(" LGP III", "-LGP-III",et$MODEL)
et$MODEL<-gsub(" LGP II", "-LGP-II",et$MODEL)
et$MODEL<-gsub(" LGP", "-LGP",et$MODEL)
et$MODEL<-gsub(" LCD-9A", "-LCD-9A",et$MODEL)
et$MODEL<-gsub(" LCR-9A", "-LCR-9A",et$MODEL)
et$MODEL<-gsub(" LGP WT", "-LGP-WT",et$MODEL)
et$MODEL<-gsub(" XLT III", "-XLT-III",et$MODEL)
et$MODEL<-gsub(" XLT II", "-XLT-II",et$MODEL)
et$MODEL<-gsub(" XLT", "-XLT",et$MODEL)
et$MODEL<-gsub(" XL III", "-XL-III",et$MODEL)
et$MODEL<-gsub(" XL II", "-XL-II",et$MODEL)
et$MODEL<-gsub(" XL", "-XL",et$MODEL)
et$MODEL<-gsub(" CR SB", "-CR-SB",et$MODEL)
et$MODEL<-gsub(" LC-7A", "-LC-7A",et$MODEL)
et$MODEL<-gsub(" LC-9A", "-LC-9A",et$MODEL)
et$MODEL<-gsub(" LCR-9", "-LCR-9",et$MODEL)
et$MODEL<-gsub(" LC-7", "-LC-7",et$MODEL)
et$MODEL<-gsub(" LC-9", "-LC-9",et$MODEL)
et$MODEL<-gsub(" E CR", "-E-CR",et$MODEL)
et$MODEL<-gsub(" LC-3", "-LC-3",et$MODEL)
et$MODEL<-gsub(" III", "-III",et$MODEL)
et$MODEL<-gsub(" LCR", "-LCR",et$MODEL)
et$MODEL<-gsub(" VHP", "-VHP",et$MODEL)
et$MODEL<-gsub(" XPS", "-XPS",et$MODEL)
et$MODEL<-gsub(" CR", "-CR",et$MODEL)
et$MODEL<-gsub(" kW", "-kW",et$MODEL)
et$MODEL<-gsub(" KW", "-KW",et$MODEL)
et$MODEL<-gsub(" Kw", "-Kw",et$MODEL)
et$MODEL<-gsub(" LC", "-LC",et$MODEL)
et$MODEL<-gsub(" LR", "-LR",et$MODEL)
et$MODEL<-gsub(" LT", "-LT",et$MODEL)
et$MODEL<-gsub(" TC", "-TC",et$MODEL)
et$MODEL<-gsub(" WT", "-WT",et$MODEL)
et$MODEL<-gsub(" XW", "-XW",et$MODEL)

#gsub("(.+?)\\s.+","\\1","RTV-X900 General Purpose")
et$modextract<-str_sub(et$MODEL,start=6+nchar(et$MANUFACTURER),end=-1)
et$modextract<-gsub("(.+?)\\s.+","\\1",et$modextract)
#et$modextract<-gsub("(.+?(XL SERIES III|XL SERIES II|SERIES III|SERIES II|DS XL III|DS XL II|HST III|HST II|HST|LGP III|LGP II|LGP|LCD-9A|LCR-9A|LGP WT|XLT III|XLT II|XLT|XL III|XL II|XL|CR SB|LC-7A|LC-9A|LCR-9|LC-7|LC-9|E CR|LC-3|III|LCR|VHP|XPS|CR|II|IT|kW|KW|LC|LR|LT|TC|WT|XW)?)\\s.+","\\1",et$modextract)
et$MODEL<-et$modextract
et$modextract<-NULL
ds<-rbind(ds,et)
}
#===============================================================================================================================
# Manufacturer/Model Parsing for CommercialTruckTrader
# ==============================================================================================================================
if(length(ct)>=26){
ct$MODEL<-toupper(ct$MODEL)
ct$MODEL<-gsub("BUSINESS CLASS M2 106", "M2-106",ct$MODEL)
ct$MODEL<-gsub("BUSINESS CLASS M2", "M2",ct$MODEL)
ct$MODEL<-gsub("SILVERADO ", "SILVERADO-",ct$MODEL)
ct$MODEL<-gsub("CHALLENGER ", "CHALLENGER-",ct$MODEL)
ct$MODEL<-gsub("CASCADIA 125", "CASCADIA-125",ct$MODEL)
ct$MODEL<-gsub("CASCADIA 126", "CASCADIA-126",ct$MODEL)
ct$MODEL<-gsub("CASCADIA 113", "CASCADIA-113",ct$MODEL)
ct$MODEL<-gsub("SIERRA 3500 HD", "SIERRA-3500HD",ct$MODEL)
ct$MODEL<-gsub("SIERRA 3500", "SIERRA-3500",ct$MODEL)
ct$MODEL<-gsub("3500 HD", "3500HD",ct$MODEL)
ct$MODEL<-gsub("SIERRA 2500 HD", "SIERRA-2500HD",ct$MODEL)
ct$MODEL<-gsub("SIERRA 2500", "SIERRA-2500",ct$MODEL)
ct$MODEL<-gsub("2500 HD", "2500HD",ct$MODEL)
ct$MODEL<-gsub("SIERRA 1500 HD", "SIERRA-1500HD",ct$MODEL)
ct$MODEL<-gsub("SIERRA 1500", "SIERRA-1500",ct$MODEL)
ct$MODEL<-gsub("1500 HD", "1500HD",ct$MODEL)
ct$MODEL<-gsub("SAVANA G2500", "SAVANA-G2500",ct$MODEL)
ct$MODEL<-gsub("SAVANA G3500", "SAVANA-G3500",ct$MODEL)
ct$MODEL<-gsub("TOPKICK C7500", "TOPKICK-C7500",ct$MODEL)
ct$MODEL<-gsub("CORONADO 122 SD", "122SD",ct$MODEL)
ct$MODEL<-gsub("DURASTAR 4300", "DURASTAR-4300",ct$MODEL)
ct$MODEL<-gsub("DURASTAR 4300", "DURASTAR-4400",ct$MODEL)
ct$MODEL<-gsub("TRANSTAR 8600", "TRANSTAR-8600",ct$MODEL)
ct$MODEL<-gsub("WORKSTAR 7600", "WORKSTAR-7600",ct$MODEL)
ct$MODEL<-gsub("PRO LF687", "LF687",ct$MODEL)
ct$MODEL<-gsub("NPR XD", "NPR-XD",ct$MODEL)
ct$MODEL<-gsub("NPR HD", "NPR-HD",ct$MODEL)
ct$MODEL<-gsub("122 SD", "122-SD",ct$MODEL)
ct$MODEL<-gsub("114 SD", "114-SD",ct$MODEL)
ct$MODEL<-gsub("SPRINTER ", "SPRINTER-",ct$MODEL)
ct$MODEL<-gsub("COLUMBIA ", "COLUMBIA-",ct$MODEL)
ct$MODEL<-gsub("EXPRESS 3500", "EXPRESS-3500",ct$MODEL)
ct$MODEL<-gsub("EXPRESS 2500", "EXPRESS-2500",ct$MODEL)
ct$MODEL<-gsub("LCF 4500", "LCF-4500",ct$MODEL)
ct$MODEL<-gsub("CENTRY ", "CENTRY-",ct$MODEL)
ct$MODEL<-gsub("KODIAK ", "KODIAK-",ct$MODEL)
ct$MODEL<-gsub("RAM ", "RAM-",ct$MODEL)
ct$MODEL<-gsub("M2 ", "M2-",ct$MODEL)
ct$MODEL<-gsub("BAR-BELL TRAILER", "BAR-BEL TRAILERS",ct$MODEL)
ct$MODEL<-gsub("BENSON TRAILERS|BENSON TRAILER", "BENSON",ct$MODEL)
ct$MODEL<-gsub("BIG TEX TRAILERS|BIG TEX TRAILER|BIG TEX", "BIG TEX TRAILERS",ct$MODEL)
ct$MODEL<-gsub("BENSON TRAILER", "BENSON",ct$MODEL)
ct$MODEL<-gsub("RAM 1500", "RAM-1500",ct$MODEL)
ct$MODEL<-gsub("RAM 2500", "RAM-2500",ct$MODEL)
ct$MODEL<-gsub("RAM 3500", "RAM-3500",ct$MODEL)
ct$MODEL<-gsub("RAM 4500", "RAM-4500",ct$MODEL)
ct$MODEL<-gsub("RAM 5500", "RAM-5500",ct$MODEL)
ct$MODEL<-gsub(" XL", "-XL",ct$MODEL)

ct$modextract<-str_sub(ct$MODEL,start=6+nchar(ct$MANUFACTURER),end=-1)
ct$modextract<-gsub("(.+?)\\s.+","\\1",ct$modextract)
ct$MODEL<-ct$modextract
ct$modextract<-NULL
ds<-rbind(ds,ct)
}


#===============================================================================================================================
# Format and Clean Fields
# ===============================================================================================================================
ds$MANUFACTURER<-iconv(ds$MANUFACTURER,"UTF-8","ASCII",sub="")
ds$MODEL<-iconv(ds$MODEL,"UTF-8","ASCII",sub="")
ds$CATEGORY<-iconv(ds$CATEGORY,"UTF-8","ASCII",sub="")
ds$CURRENCY<-iconv(ds$CURRENCY,"UTF-8","ASCII",sub="")
ds$SERIALNUMBER<-iconv(ds$SERIALNUMBER,"UTF-8","ASCII",sub="")
ds$METERREADS<-iconv(ds$METERREADS,"UTF-8","ASCII",sub="")
ds$DESCRIPTION<-iconv(ds$DESCRIPTION,"UTF-8","ASCII",sub="")
ds$SURVEYTYPE<-iconv(ds$SURVEYTYPE,"UTF-8","ASCII",sub="")
ds$PARTICIPANTCODE<-iconv(ds$PARTICIPANTCODE,"UTF-8","ASCII",sub="")
ds$SELLERADDRESS<-iconv(ds$SELLERADDRESS,"UTF-8","ASCII",sub="")
ds$SELLERCITY<-iconv(ds$SELLERCITY,"UTF-8","ASCII",sub="")
ds$SELLERSTATE<-iconv(ds$SELLERSTATE,"UTF-8","ASCII",sub="")
ds$SELLERZIP<-iconv(ds$SELLERZIP,"UTF-8","ASCII",sub="")
ds$SELLERCOUNTRY<-iconv(ds$SELLERCOUNTRY,"UTF-8","ASCII",sub="")
ds$SELLERNAME<-iconv(ds$SELLERNAME,"UTF-8","ASCII",sub="")
ds$CONDITION<-iconv(ds$CONDITION,"UTF-8","ASCII",sub="")
ds$ASSETURL<-iconv(ds$ASSETURL,"UTF-8","ASCII",sub="")
ds$SOURCE<-iconv(ds$SOURCE,"UTF-8","ASCII",sub="")

ds$MANUFACTURER<-gsub("\r\n|\r|\n", " ", ds$MANUFACTURER)
ds$MODEL<-gsub("\r\n|\r|\n", " ", ds$MODEL)
ds$CATEGORY<-gsub("\r\n|\r|\n", " ", ds$CATEGORY)
ds$CURRENCY<-gsub("\r\n|\r|\n", " ", ds$CURRENCY)
ds$SERIALNUMBER<-gsub("\r\n|\r|\n", " ", ds$SERIALNUMBER)
ds$METERREADS<-gsub("\r\n|\r|\n", " ", ds$METERREADS)
ds$DESCRIPTION<-gsub("\r\n|\r|\n", " ", ds$DESCRIPTION)
ds$SURVEYTYPE<-gsub("\r\n|\r|\n", " ", ds$SURVEYTYPE)
ds$PARTICIPANTCODE<-gsub("\r\n|\r|\n", " ", ds$PARTICIPANTCODE)
ds$SELLERADDRESS<-gsub("\r\n|\r|\n", " ", ds$SELLERADDRESS)
ds$SELLERCITY<-gsub("\r\n|\r|\n", " ", ds$SELLERCITY)
ds$SELLERSTATE<-gsub("\r\n|\r|\n", " ", ds$SELLERSTATE)
ds$SELLERZIP<-gsub("\r\n|\r|\n", " ", ds$SELLERZIP)
ds$SELLERCOUNTRY<-gsub("\r\n|\r|\n", " ", ds$SELLERCOUNTRY)
ds$SELLERNAME<-gsub("\r\n|\r|\n", " ", ds$SELLERNAME)
ds$CONDITION<-gsub("\r\n|\r|\n", " ", ds$CONDITION)
ds$ASSETURL<-gsub("\r\n|\r|\n", " ", ds$ASSETURL)
ds$SOURCE<-gsub("\r\n|\r|\n", " ", ds$SOURCE)

grep("\r\n|\r|\n",ds$MANUFACTURER)
grep("\r\n|\r|\n",ds$MODEL)
grep("\r\n|\r|\n",ds$CATEGORY)
grep("\r\n|\r|\n",ds$CURRENCY)
grep("\r\n|\r|\n",ds$SERIALNUMBER)
grep("\r\n|\r|\n",ds$METERREADS)
grep("\r\n|\r|\n",ds$DESCRIPTION)
grep("\r\n|\r|\n",ds$SURVEYTYPE)
grep("\r\n|\r|\n",ds$PARTICIPANTCODE)
grep("\r\n|\r|\n",ds$SELLERADDRESS)
grep("\r\n|\r|\n",ds$SELLERCITY)
grep("\r\n|\r|\n",ds$SELLERSTATE)
grep("\r\n|\r|\n",ds$SELLERZIP)
grep("\r\n|\r|\n",ds$SELLERCOUNTRY)
grep("\r\n|\r|\n",ds$SELLERNAME)
grep("\r\n|\r|\n",ds$CONDITION)
grep("\r\n|\r|\n",ds$ASSETURL)
grep("\r\n|\r|\n",ds$SOURCE)

ds$MANUFACTURER <- toupper(ds$MANUFACTURER)
ds$MANUFACTURER <- gsub(",","",ds$MANUFACTURER)
ds$MANUFACTURER <- gsub("JOHN DEERE", "DEERE", ds$MANUFACTURER)
ds$MANUFACTURER <- gsub("\\?","",ds$MANUFACTURER)
ds$MANUFACTURER <- gsub("\\:","",ds$MANUFACTURER)
ds$MANUFACTURER <- gsub("(.+)\\(.+","\\1",ds$MANUFACTURER)
ds$MANUFACTURER[!grepl("[A-Za-z]",ds$MANUFACTURER)]<-NA
ds$MANUFACTURER <- gsub("(.+)\\(.+","\\1",ds$MANUFACTURER)
ds$MODEL <- gsub("\\?","",ds$MODEL)
ds$MODEL <- gsub("\\:","",ds$MODEL)
ds$MODEL <- gsub(",","",ds$MODEL)
ds$MODEL <- gsub('"',"",ds$MODEL)
ds$MODEL <- gsub("(.+)\\(.+","\\1",ds$MODEL)
ds$MANUFACTURER[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("deere",ds$ASSETURL)]<-"DEERE"
ds$MODEL[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("deere",ds$ASSETURL)]<-gsub("Deere ","",ds$MODEL)
ds$MANUFACTURER[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("allis-chalmers",ds$ASSETURL)]<-"ALLIS CHALMERS"
ds$MODEL[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("allis-chalmers",ds$ASSETURL)]<-gsub("Chalmers ","",ds$MODEL)
ds$MANUFACTURER[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("atlas-copco",ds$ASSETURL)]<-"ATLAS COPCO"
ds$MODEL[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("atlas-copco",ds$ASSETURL)]<-gsub("Copco ","",ds$MODEL)
ds$MANUFACTURER[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("blaw-knox",ds$ASSETURL)]<-"BLAW KNOX"
ds$MODEL[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("blaw-knox",ds$ASSETURL)]<-gsub("Knox ","",ds$MODEL)
ds$MANUFACTURER[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("ingersoll-rand",ds$ASSETURL)]<-"INGERSOLL RAND"
ds$MODEL[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("ingersoll-rand",ds$ASSETURL)]<-gsub("Rand ","",ds$MODEL)
ds$MANUFACTURER[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("massey-ferguson",ds$ASSETURL)]<-"MASSEY FERGUSON"
ds$MODEL[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("massey-ferguson",ds$ASSETURL)]<-gsub("Ferguson ","",ds$MODEL)
ds$MANUFACTURER[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("new-holland",ds$ASSETURL)]<-"NEW HOLLAND"
ds$MODEL[grepl("CONEQUIPGD",ds$PARTICIPANTCODE)&grepl("new-holland",ds$ASSETURL)]<-gsub("Holland ","",ds$MODEL)

# TractorHouse
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("agco-allis",ds$ASSETURL)]<-"AGCO ALLIS"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("agco-allis",ds$ASSETURL)]<-gsub("ALLIS ","",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("agco-allis",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("case-ih",ds$ASSETURL)]<-"CASE IH"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("case-ih",ds$ASSETURL)]<-gsub("E IH PAOT","PATRIOT",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("case-ih",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("case-ih",ds$ASSETURL)]<-"CASE IH"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("case-ih",ds$ASSETURL)]<-gsub("E IH ALL","FARMALL",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("case-ih",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("case-ih",ds$ASSETURL)]<-"CASE IH"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("case-ih",ds$ASSETURL)]<-gsub("E IH ","",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("case-ih",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("deutz-fahr",ds$ASSETURL)]<-"DEUTZ FAHR"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("deutz-fahr",ds$ASSETURL)]<-gsub("FAHR ","",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("deutz-fahr",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("john-deere",ds$ASSETURL)]<-"DEERE"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("john-deere",ds$ASSETURL)]<-gsub("DEERE ","",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("john-deere",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("buhler-versatile",ds$ASSETURL)]<-"VERSATILE"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("buhler-versatile",ds$ASSETURL)]<-gsub("VERSATILE ","",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("buhler-versatile",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("kuhn-knight",ds$ASSETURL)]<-"KUHN KNIGHT"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("kuhn-knight",ds$ASSETURL)]<-gsub("KNIGHT ","",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("kuhn-knight",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("great-plains",ds$ASSETURL)]<-"GREAT PLAINS"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("great-plains",ds$ASSETURL)]<-gsub("PLAINS ","",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("great-plains",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("land-pride",ds$ASSETURL)]<-"LAND PRIDE"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("land-pride",ds$ASSETURL)]<-gsub("PRIDE ","",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("land-pride",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("duo-lift",ds$ASSETURL)]<-"DUO LIFT"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("duo-lift",ds$ASSETURL)]<-gsub("LIFT ","",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("duo-lift",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("honey-bee",ds$ASSETURL)]<-"HONEY BEE"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("honey-bee",ds$ASSETURL)]<-gsub("BEE ","",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("honey-bee",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("mac-don",ds$ASSETURL)]<-"MACDON"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("mac-don",ds$ASSETURL)]<-gsub("DON ","",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("mac-don",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("geringhoff",ds$ASSETURL)]<-"GERINGHOFF"
ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("geringhoff",ds$ASSETURL)]<-gsub("RINGHOFF ","",ds$MODEL[grepl("TRCTRHOUSE",ds$PARTICIPANTCODE)&grepl("geringhoff",ds$ASSETURL)])

# TruckPaper
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("john-deere",ds$ASSETURL)]<-"DEERE"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("john-deere",ds$ASSETURL)]<-gsub("DEERE ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("john-deere",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("am-general",ds$ASSETURL)]<-"AM GENERAL"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("am-general",ds$ASSETURL)]<-gsub("GENERAL ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("am-general",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("american-hauler",ds$ASSETURL)]<-"AMERICAN HAULER"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("american-hauler",ds$ASSETURL)]<-gsub("HAULER ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("american-hauler",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("arctic-cat",ds$ASSETURL)]<-"ARCTIC CAT"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("arctic-cat",ds$ASSETURL)]<-gsub("CAT ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("arctic-cat",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("arising-industries",ds$ASSETURL)]<-"ARISING INDUSTRIES"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("arising-industries",ds$ASSETURL)]<-gsub("INDUSTRIES ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("arising-industries",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("b-and-b",ds$ASSETURL)]<-"B & B"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("b-and-b",ds$ASSETURL)]<-gsub("& B ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("b-and-b",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("better-built",ds$ASSETURL)]<-"BETTER BUILT"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("better-built",ds$ASSETURL)]<-gsub("BUILT ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("better-built",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("big-john",ds$ASSETURL)]<-"BIG JOHN"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("big-john",ds$ASSETURL)]<-gsub("JOHN ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("big-john",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("big-tex",ds$ASSETURL)]<-"BIG TEX"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("big-tex",ds$ASSETURL)]<-gsub("TEX ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("big-tex",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("bradford-built",ds$ASSETURL)]<-"BRADFORD BUILT"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("bradford-built",ds$ASSETURL)]<-gsub("BUILT ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("bradford-built",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("c-and-b",ds$ASSETURL)]<-"C & B"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("c-and-b",ds$ASSETURL)]<-gsub("& B ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("c-and-b",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("cargo-express",ds$ASSETURL)]<-"CARGO EXPRESS"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("cargo-express",ds$ASSETURL)]<-gsub("EXPRESS ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("cargo-express",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("canada-trailers-mfg",ds$ASSETURL)]<-"CANADA TRAILERS MFG"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("canada-trailers-mfg",ds$ASSETURL)]<-gsub("TRAILERS MFG ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("canada-trailers-mfg",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("continental-cargo",ds$ASSETURL)]<-"CONTINENTAL CARGO"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("continental-cargo",ds$ASSETURL)]<-gsub("CARGO ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("continental-cargo",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("canada-trailers-mfg",ds$ASSETURL)]<-"CANADA TRAILERS MFG"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("canada-trailers-mfg",ds$ASSETURL)]<-gsub("TRAILERS MFG ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("canada-trailers-mfg",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("cross-trailers",ds$ASSETURL)]<-"CROSS TRAILERS"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("cross-trailers",ds$ASSETURL)]<-gsub("TRAILERS ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("cross-trailers",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("custom-trailer",ds$ASSETURL)]<-"CUSTOM TRAILER"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("custom-trailer",ds$ASSETURL)]<-gsub("TRAILER ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("custom-trailer",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("custom-built",ds$ASSETURL)]<-"CUSTOM BUILT"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("custom-built",ds$ASSETURL)]<-gsub("BUILT ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("custom-built",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("diamond-c",ds$ASSETURL)]<-"DIAMOND C"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("diamond-c",ds$ASSETURL)]<-gsub("C ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("diamond-c",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("great-dane",ds$ASSETURL)]<-"GREAT DANE"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("great-dane",ds$ASSETURL)]<-gsub("DANE ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("great-dane",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("h-and-h",ds$ASSETURL)]<-"H & H"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("h-and-h",ds$ASSETURL)]<-gsub("& H ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("h-and-h",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("load-king",ds$ASSETURL)]<-"LOAD KING"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("load-king",ds$ASSETURL)]<-gsub("KING ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("load-king",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("load-trail",ds$ASSETURL)]<-"LOAD TRAIL"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("load-trail",ds$ASSETURL)]<-gsub("TRAIL ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("load-trail",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("lode-king",ds$ASSETURL)]<-"LODE KING"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("lode-king",ds$ASSETURL)]<-gsub("KING ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("lode-king",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("mac-trailer-mfg",ds$ASSETURL)]<-"MAC TRAILER MFG"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("mac-trailer-mfg",ds$ASSETURL)]<-gsub("TRAILER MFG ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("mac-trailer-mfg",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("marksman-mfg",ds$ASSETURL)]<-"MARKSMAN MFG"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("marksman-mfg",ds$ASSETURL)]<-gsub("MFG ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("marksman-mfg",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("mitsubishi-fuso",ds$ASSETURL)]<-"MITSUBISHI FUSO"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("mitsubishi-fuso",ds$ASSETURL)]<-gsub("FUSO ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("mitsubishi-fuso",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("redi-haul",ds$ASSETURL)]<-"REDI HAUL"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("redi-haul",ds$ASSETURL)]<-gsub("HAUL ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("redi-haul",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("trail-king",ds$ASSETURL)]<-"TRAIL KING"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("trail-king",ds$ASSETURL)]<-gsub("KING ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("trail-king",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("wabash-national",ds$ASSETURL)]<-"WABASH NATIONAL"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("wabash-national",ds$ASSETURL)]<-gsub("NATIONAL ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("wabash-national",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("wells-cargo",ds$ASSETURL)]<-"WELLS CARGO"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("wells-cargo",ds$ASSETURL)]<-gsub("CARGO ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("wells-cargo",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("western-star",ds$ASSETURL)]<-"WESTERN STAR"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("western-star",ds$ASSETURL)]<-gsub("STAR ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("western-star",ds$ASSETURL)])
ds$MANUFACTURER[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("xl-specialized",ds$ASSETURL)]<-"XL SPECIALIZED"
ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("xl-specialized",ds$ASSETURL)]<-gsub("SPECIALIZED ","",ds$MODEL[grepl("TRUCKPAPER",ds$PARTICIPANTCODE)&grepl("xl-specialized",ds$ASSETURL)])

# MachinaryTrader
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("air-burners",ds$ASSETURL)]<-"AIR BURNERS"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("air-burners",ds$ASSETURL)]<-gsub("BURNERS ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("air-burners",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("all-power",ds$ASSETURL)]<-"ALL POWER"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("all-power",ds$ASSETURL)]<-gsub("POWER ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("all-power",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("allen-concrete-pavers",ds$ASSETURL)]<-"ALLEN CONCRETE PAVERS"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("allen-concrete-pavers",ds$ASSETURL)]<-gsub("CONCRETE PAVERS ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("allen-concrete-pavers",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("allen-eng",ds$ASSETURL)]<-"ALLEN ENG"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("allen-eng",ds$ASSETURL)]<-gsub("ENG ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("allen-eng",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("allmand-bros",ds$ASSETURL)]<-"ALLMAND BROS"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("allmand-bros",ds$ASSETURL)]<-gsub("BROS ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("allmand-bros",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("american-augers",ds$ASSETURL)]<-"AMERICAN AUGERS"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("american-augers",ds$ASSETURL)]<-gsub("AUGERS ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("american-augers",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("american-eagle",ds$ASSETURL)]<-"AMERICAN EAGLE"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("american-eagle",ds$ASSETURL)]<-gsub("EAGLE ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("american-eagle",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("american-lincoln",ds$ASSETURL)]<-"AMERICAN LINCOLN"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("american-lincoln",ds$ASSETURL)]<-gsub("LINCOLN ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("american-lincoln",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("american-steel-works",ds$ASSETURL)]<-"AMERICAN STEEL WORKS"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("american-steel-works",ds$ASSETURL)]<-gsub("STEEL WORKS ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("american-steel-works",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("arrow-master",ds$ASSETURL)]<-"ARROW MASTER"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("arrow-master",ds$ASSETURL)]<-gsub("MASTER ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("arrow-master",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("asphalt-zipper",ds$ASSETURL)]<-"ASPHALT ZIPPER"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("asphalt-zipper",ds$ASSETURL)]<-gsub("ZIPPER ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("asphalt-zipper",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("atlas-copco",ds$ASSETURL)]<-"ATLAS COPCO"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("atlas-copco",ds$ASSETURL)]<-gsub("COPCO ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("atlas-copco",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("c-w-machine-worx",ds$ASSETURL)]<-"C W MACHINE WORX"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("c-w-machine-worx",ds$ASSETURL)]<-gsub("W MACHINE WORX ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("c-w-machine-worx",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("custom-built",ds$ASSETURL)]<-"CUSTOM BUILT"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("custom-built",ds$ASSETURL)]<-gsub("BUILT ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("custom-built",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("ditch-witch",ds$ASSETURL)]<-"DITCH WITCH"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("ditch-witch",ds$ASSETURL)]<-gsub("WITCH ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("ditch-witch",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("eagle-crusher",ds$ASSETURL)]<-"EAGLE CRUSHER"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("eagle-crusher",ds$ASSETURL)]<-gsub("CRUSHER ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("eagle-crusher",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("ez-lift",ds$ASSETURL)]<-"EZ LIFT"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("ez-lift",ds$ASSETURL)]<-gsub("LIFT ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("ez-lift",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("ez-screen",ds$ASSETURL)]<-"EZ SCREEN"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("ez-screen",ds$ASSETURL)]<-gsub("SCREEN ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("ez-screen",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("e-z-drill",ds$ASSETURL)]<-"E-Z DRILL"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("e-z-drill",ds$ASSETURL)]<-gsub("DRILL ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("e-z-drill",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("fab-tec-inc",ds$ASSETURL)]<-"FABTEC"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("fab-tec-inc",ds$ASSETURL)]<-gsub("TEC INC ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("fab-tec-inc",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("fiat-hitachi",ds$ASSETURL)]<-"FIAT HITACHI"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("fiat-hitachi",ds$ASSETURL)]<-gsub("HITACHI ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("fiat-hitachi",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("fiat-kobelco",ds$ASSETURL)]<-"FIAT KOBELCO"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("fiat-kobelco",ds$ASSETURL)]<-gsub("KOBELCO ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("fiat-kobelco",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("hd-engineering",ds$ASSETURL)]<-"HD ENGINEERING"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("hd-engineering",ds$ASSETURL)]<-gsub("ENGINEERING ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("hd-engineering",ds$ASSETURL)])
#ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("john-deere",ds$ASSETURL)]<-"DEERE"
#ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("john-deere",ds$ASSETURL)]<-gsub("DEERE ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("john-deere",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("new-holland",ds$ASSETURL)]<-"NEW HOLLAND"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("new-holland",ds$ASSETURL)]<-gsub("HOLLAND ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("new-holland",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("o-and-k",ds$ASSETURL)]<-"O & K"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("o-and-k",ds$ASSETURL)]<-gsub("& K ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("o-and-k",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("sullivan-palatek",ds$ASSETURL)]<-"SULLIVAN PALATEK"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("sullivan-palatek",ds$ASSETURL)]<-gsub("PALATEK ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("sullivan-palatek",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("superior-broom",ds$ASSETURL)]<-"SUPERIOR BROOM"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("superior-broom",ds$ASSETURL)]<-gsub("BROOM ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("superior-broom",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("terex-cmi",ds$ASSETURL)]<-"TEREX CMI"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("terex-cmi",ds$ASSETURL)]<-gsub("CMI ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("terex-cmi",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("terex-finlay",ds$ASSETURL)]<-"TEREX FINLAY"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("terex-finlay",ds$ASSETURL)]<-gsub("FINLAY ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("terex-finlay",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("terex-pegson",ds$ASSETURL)]<-"TEREX PEGSON"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("terex-pegson",ds$ASSETURL)]<-gsub("PEGSON ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("terex-pegson",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("vince-hagan",ds$ASSETURL)]<-"VINCE HAGAN"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("vince-hagan",ds$ASSETURL)]<-gsub("HAGAN ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("vince-hagan",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("wacker-neuson",ds$ASSETURL)]<-"WACKER NEUSON"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("wacker-neuson",ds$ASSETURL)]<-gsub("NEUSON ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("wacker-neuson",ds$ASSETURL)])


#Liftstoday
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("bt-lifts",ds$ASSETURL)]<-"BT LIFTS"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("bt-lifts",ds$ASSETURL)]<-gsub("LIFTS ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("bt-lifts",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("lift-king",ds$ASSETURL)]<-"LIFT KING"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("lift-king",ds$ASSETURL)]<-gsub("KING ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("lift-king",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("load-lifter",ds$ASSETURL)]<-"LOAD LIFTER"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("load-lifter",ds$ASSETURL)]<-gsub("LIFTER ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("load-lifter",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("master-craft",ds$ASSETURL)]<-"MASTER CRAFT"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("master-craft",ds$ASSETURL)]<-gsub("CRAFT ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("master-craft",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("nifty-lift",ds$ASSETURL)]<-"NIFTY LIFT"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("nifty-lift",ds$ASSETURL)]<-gsub("LIFT ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("nifty-lift",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("sky-trak",ds$ASSETURL)]<-"SKY TRAK"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("sky-trak",ds$ASSETURL)]<-gsub("TRAK ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("sky-trak",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("traverse-lift",ds$ASSETURL)]<-"TRAVERSE LIFT"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("traverse-lift",ds$ASSETURL)]<-gsub("LIFT ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("traverse-lift",ds$ASSETURL)])
ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("xtreme-mfg",ds$ASSETURL)]<-"XTREME MFG"
ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("xtreme-mfg",ds$ASSETURL)]<-gsub("MFG ","",ds$MODEL[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("xtreme-mfg",ds$ASSETURL)])
#ds$MANUFACTURER[grepl("MACHTRADER",ds$PARTICIPANTCODE)&grepl("agco-allis",ds$ASSETURL)]<-"AGCO ALLIS"
#ds$MODEL<-gsub("ALLIS ","",ds$MODEL)


ds$DESCRIPTION  <- ifelse(nchar(ds$DESCRIPTION) > 249, substring(ds$DESCRIPTION, 1, 249),ds$DESCRIPTION)
ds$DESCRIPTION[is.na(ds$DESCRIPTION)]<-NA
ds$DESCRIPTION[grepl("^None$",ds$DESCRIPTION)]<-NA
ds$DESCRIPTION[grepl("ul class",ds$DESCRIPTION)]<-NA
ds$DESCRIPTION[!grepl("[A-Za-z]",ds$DESCRIPTION)]<-NA
ds$DESCRIPTION <- gsub("^\\[.]$","",ds$DESCRIPTION)
#ds$DESCRIPTION <- gsub("\\â","",ds$DESCRIPTION)
#ds$DESCRIPTION<-gsub("w/ ...more.","",ds$DESCRIPTION)
ds$SELLERCITY<-gsub("(.+)\\s?\\(\\w+\\)","\\1",ds$SELLERCITY)
ds$SELLERSTATE[!grepl("[A-Z].*",ds$SELLERSTATE)]<-NA
ds$SELLERSTATE[grepl("HYSTER",ds$SELLERSTATE)]<-NA
ds$SELLERADDRESS[grepl("Description",ds$SELLERADDRESS)]<-NA
ds$SELLERADDRESS[!grepl("[A-Za-z]",ds$SELLERADDRESS)]<-NA
ds$SELLERADDRESS[grepl("Sign",ds$SELLERADDRESS)]<-NA
ds$SELLERCITY[!grepl("[A-Za-z]",ds$SELLERCITY)]<-NA
ds$SELLERCITY<-gsub("\\,","",ds$SELLERCITY)
ds$SELLERSTATE<-gsub("\\,","",ds$SELLERSTATE)
ds$SELLERZIP<-gsub("\\,+","",ds$SELLERZIP)
ds$SELLERZIP[grepl("News",ds$SELLERZIP)]<-NA
ds$SELLERZIP[grepl("[+]",ds$SELLERZIP)]<-NA
ds$SELLERZIP[ds$SELLERCOUNTRY=="US"&nchar(ds$SELLERZIP)<3]<-NA
ds$SELLERCOUNTRY[grepl("Fax",ds$SELLERCOUNTRY)]<-NA
ds$SELLERCOUNTRY[grepl("OFFICE",ds$SELLERCOUNTRY)]<-NA
ds$SELLERCOUNTRY[grepl("S/F",ds$SELLERCOUNTRY)]<-NA
ds$SELLERCOUNTRY<-gsub("\\?","",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub("\\/","",ds$SELLERCOUNTRY)
ds$CATEGORY[!grepl("[A-Za-z]",ds$CATEGORY)]<-NA
ds$PRICE[grepl("[A-Za-z]",ds$PRICE)]<-NA
ds$CURRENCY[grepl("[0-9]",ds$CURRENCY)]<-NA
ds$CURRENCY[grepl("HRS",ds$CURRENCY)]<-NA
ds$SELLERSTATE[grepl("[0-9]",ds$SELLERSTATE)]<-NA
#ds$SELLERSTATE[grepl("TRUCKS",ds$SELLERSTATE)]<-NA
ds$SELLERCITY[grepl("[0-9]",ds$SELLERCITY)]<-NA
ds$CONDITION<-gsub("None|N/A|Asis|Dismantled|Remanufactured","",ds$CONDITION)
ds$SELLERZIP[nchar(ds$SELLERZIP)<4]<-NA
ds$SELLERCOUNTRY[grepl("SAN LEANDRO",ds$SELLERCOUNTRY)]<-NA
ds$SELLERCOUNTRY[grepl("San Diego",ds$SELLERCOUNTRY)]<-"US"
ds$SELLERCOUNTRY[grepl("Finedon Industrial Estate",ds$SELLERCOUNTRY)]<-NA
ds$SELLERADDRESS[grepl("Phone",ds$SELLERADDRESS)]<-NA
ds$SELLERADDRESS[grepl("Equipment",ds$SELLERADDRESS)]<-NA
ds$SELLERADDRESS[grepl("Company",ds$SELLERADDRESS)]<-NA
ds$SELLERCITY[grepl('""r""',ds$SELLERCITY)]<-NA
ds$SELLERCITY[grepl("Volvo",ds$SELLERCITY)]<-NA
ds$SELLERCITY[grepl("Call for",ds$SELLERCITY)]<-NA
ds$SELLERCITY[grepl("Virtual Location",ds$SELLERCITY)]<-NA
ds$SELLERCITY[grepl("Valero Refinery",ds$SELLERCITY)]<-NA
ds$SELLERSTATE<-gsub("\\*","",ds$SELLERSTATE)
ds$CATEGORY[grepl("Yes",ds$CATEGORY)]<-NA
ds$MANUFACTURER<-gsub("\\!","",ds$MANUFACTURER)
ds$MODEL[grepl("\\#",ds$MODEL)]<-NA
ds$CONDITION[grepl("Location",ds$CONDITION)]<-NA
ds$CONDITION[!grepl("Excellent|Very Good|Good|Fair|Poor|New|Unknown|Used",ds$CONDITION)]<-NA
ds$CONDITION[!grepl("EXCELLENT|VERY GOOD|GOOD|FAIR|POOR|NEW|UNKNOWN|USED",ds$CONDITION)]<-NA
ds$ASSETURL[ds$ASSETURL=="Caterpillar"]<-NA
ds$ASSETURL[grepl("jpg",ds$ASSETURL)]<-NA
ds$SERIALNUMBER<-gsub("More Detail","",ds$SERIALNUMBER)
ds$SERIALNUMBER<-gsub("\\:","",ds$SERIALNUMBER)
ds$SERIALNUMBER<-gsub("^0$","",ds$SERIALNUMBER)
ds$DESCRIPTION<-gsub("For more Info, Please Call 1-800-635-9801 to Speak with a Truck Sales Rep. Manufacturer","",ds$DESCRIPTION)
ds$SELLERCOUNTRY[ds$PARTICIPANTCODE=="WIESEUSA" & is.na(ds$SELLERCOUNTRY)]<-"US"
ds$SELLERCOUNTRY[ds$PARTICIPANTCODE=="WIESEUSA" & ds$SELLERCOUNTRY==""]<-"US"
ds$DESCRIPTION<-gsub("^\\-(.+)?$","\\1",ds$DESCRIPTION)
ds$DESCRIPTION<-gsub("^\\=(.+)?$","\\1",ds$DESCRIPTION)
ds$DESCRIPTION<-gsub("Description:","",ds$DESCRIPTION)
ds$DESCRIPTION<-gsub("Stock :","",ds$DESCRIPTION)
ds$DESCRIPTION<-gsub("View Larger","",ds$DESCRIPTION)
ds$DESCRIPTION<-gsub("PENDING","",ds$DESCRIPTION)
ds$DESCRIPTION<-gsub("DISPONIBLE","",ds$DESCRIPTION)
ds$CONDITION[grepl("HOUR",ds$CONDITION)]<-NA
ds$CONDITION[grepl("Location",ds$CONDITION)]<-NA
ds$CONDITION[ds$CONDITION=="Rental"]<-NA
ds$CONDITION<-gsub("Used/Rental","Used",ds$CONDITION)
ds$SERIALNUMBER[grepl("CONDITION",ds$SERIALNUMBER)]<-NA
ds$SERIALNUMBER[grepl("SHIFTER",ds$SERIALNUMBER)]<-NA
ds$SERIALNUMBER[grepl("SPECIAL",ds$SERIALNUMBER)]<-NA

ds$STOCKNUMBER[grepl("eLiftTruck",ds$PARTICIPANTCODE)]<-NA
ds$STOCKNUMBER[grepl("`",ds$PARTICIPANTCODE)]<-NA
ds$CATEGORY[grepl("FarmerHtln",ds$PARTICIPANTCODE)]<-NA
ds$METERREADS[grepl("CONDITION",ds$METERREADS)]<-NA
ds$METERREADS[grepl("\\-",ds$METERREADS)]<-NA
ds$METERREADS<-gsub("(\\d+\\s[H|M|K])\\s.*","\\1",ds$METERREADS)
ds$STOCKNUMBER[ds$STOCKNUMBER=="Willers"|ds$STOCKNUMBER=="WEGS"|ds$STOCKNUMBER=="weg"]<-NA
ds$SELLERCITY<-gsub("\\d+","",ds$SELLERCITY) #Remove numeric
ds$SELLERCITY<-gsub("\\-","",ds$SELLERCITY)
ds$SELLERCITY<-gsub("\\.","",ds$SELLERCITY)
ds$SELLERCITY<-gsub("\\?","",ds$SELLERCITY)
ds$SELLERCITY<-gsub("\\(","",ds$SELLERCITY)
ds$SELLERCITY<-gsub("\\)","",ds$SELLERCITY)
ds$SELLERCITY<-gsub("'","",ds$SELLERCITY)
ds$SELLERCITY<-gsub('"',"",ds$SELLERCITY)
ds$SELLERCITY<-gsub("Usa","",ds$SELLERCITY)
ds$SELLERCITY<-gsub("//","",ds$SELLERCITY)
ds$SELLERCITY<-gsub("\\/","",ds$SELLERCITY)
ds$SELLERSTATE<-gsub("\\d+","",ds$SELLERSTATE) #Remove numeric
ds$SELLERSTATE<-gsub("\\-","",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("\\.","",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("\\?","",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("\\(","",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("\\)","",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("\\/","",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("TRINIDAD","",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("\\w+\\s(AL|AK|AZ|AR|CA|CO|CT|DE|FL|GA|HI|ID|IL|IN|IA|KS|KY|
                      LA|ME|MD|MA|MI|MN|MS|MO|MT|NE|NV|NH|NJ|NM|NY|NC|ND|
                      OH|OK|OR|PA|RI|SC|SD|TN|TX|UT|VT|VA|WA|WV|WI|WY|AB|BC|MB|NB|NL|NS|NT|NU|ON|PE|QC|SK|YT)","\\1",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("\\w+\\s(ALABAMA|ALASKA|ARIZONA|ARKANSAS|CALIFORNIA|COLORADO|CONNECTICUT|DELAWARE|FLORIDA|GEORGIA|HAWAII|IDAHO|ILLINOIS|INDIANA|IOWA|KANSAS|KENTUCKY|
                     LOUISIANA|MAINE|MARYLAND|MASSACHUSETTS|MICHIGAN|MINNESOTA|MISSISSIPPI|MISSOURI|MONTANA|NEBRASKA|NEVADA|NEW HAMPSHIRE|NEW JERSEY|NEW MEXICO|
                     NEW YORK|NORTH CAROLINA|NORTH DAKOTA|OHIO|OKLAHOMA|OREGON|PENNSYLVANIA|RHODE ISLAND|SOUTH CAROLINA|SOUTH DAKOTA|TENNESSEE|TEXAS|UTAH|VERMONT|
                     VIRGINIA|WASHINGTON|WEST VIRGINIA|WISCONSIN|WYOMING|ALBERTA|BRITISH COLUMBIA|MANITOBA|NEW BRUNSWICK|NEWFOUNDLAND AND LABRADOR|NOVA SCOTIA|
                     NORTHWEST TERRITORIES|NUNAVUT|ONTARIO|PRINCE EDWARD ISLAND|QUEBEC|SASKATCHEWAN|YUKON)","\\1",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^MONTREAL QUEBEC$|^SOUTHERN QUEBEC$|^QUBEC$|^SAINTALBAN QUEBEC$","QUEBEC",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^CONNECTIC$","CONNECTICUT",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^WASHINGRON$","WASHINGTON",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^SALT UTAH$","UTAH",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^GRANDE ALBERTA$|^FORT ALBERTA$","ALBERTA",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^REGINA SASKATCHEWAN$","SASKATCHEWAN",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^SAN TEXAS$","TEXAS",ds$SELLERSTATE)

ds$SELLERCOUNTRY<-gsub("\\d+","",ds$SELLERCOUNTRY) #Remove numeric
ds$SELLERCOUNTRY<-gsub("\\-","",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub("\\.","",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub("\\?","",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub("\\(","",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub("\\)","",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub("AND TOBAGO","",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub("IRVINE, CA","",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub("GP BKT,QUICK COUPLER","",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub("UM","",ds$SELLERCOUNTRY)
#================================================================================================================================
# Format Price
# ===============================================================================================================================
ds$PRICE<-as.character(ds$PRICE)
ds$PRICE<-parse_number(ds$PRICE)
ds$PRICE<-gsub("(\\d+)\\.\\d+","\\1",ds$PRICE) #drop anything after a decimal point
ds$PRICE <- as.numeric(ds$PRICE)
ds <- ds[!is.na(ds$PRICE), ]
ds <- ds[which(ds$PRICE!=0),]
ds <- ds[which(ds$PRICE!=""), ]
ds <- ds[which(ds$PRICE > 9 & ds$PRICE < 5000000),]
ds<-ds[!grepl("\\+",ds$PRICE),]
ds$PRICE<-gsub("(\\d+)\\.\\d+","\\1",ds$PRICE)

#length(ds2$PRICE[is.na(ds$PRICE)])
#length(ds2$PRICE[ds$PRICE==""])
#length(ds2$PRICE[ds$PRICE=="0"])
#ds2$PRICE<-parse_number(ds2$PRICE)

#da$PRICE <- as.numeric(da$PRICE)
#da <- da[!is.na(da$PRICE), ]
#da <- da[which(da$PRICE!=0),]
#da <- da[which(da$PRICE!=""), ]
#da <- da[which(da$PRICE > 9 & da$PRICE < 5000000),]

# ===============================================================================================================================
# Fill Missing Country as "US" and Convert Everything to "US"
# ===============================================================================================================================

ds$SELLERCOUNTRY<-gsub(".+(United States)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(UNITED STATES)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub("USA|United States|UNITED STATES","US",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(US)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(CANADA)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Canada)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub("CANADA","Canada",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Nicaragua)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(United Arab Emirates)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(China)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Thailand)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Netherlands)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(France)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Argentina)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Brazil)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Mexico)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Costa Rica)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(India)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Argentina)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Germany)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Australia)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Trinidad and Tobago)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Chile)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(United Kingdom)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Austria)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Guatemala)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Panama)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Spain)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Dominican Republic)","\\1",ds$SELLERCOUNTRY)
ds$SELLERCOUNTRY<-gsub(".+(Bolivia)","\\1",ds$SELLERCOUNTRY)

# ===============================================================================================================================
# Length Restriction
# ===============================================================================================================================
ds$SELLERADDRESS<-trimws(ds$SELLERADDRESS)
ds$SELLERCITY<-trimws(ds$SELLERCITY)
ds$SELLERCOUNTRY<-trimws(ds$SELLERCOUNTRY)
ds$SELLERNAME<-trimws(ds$SELLERNAME)
ds$SELLERSTATE<-trimws(ds$SELLERSTATE)
ds$SELLERZIP<-trimws(ds$SELLERZIP)
ds$CATEGORY<-trimws(ds$CATEGORY)
ds$CURRENCY<-trimws(ds$CURRENCY)
ds$SOURCE<-trimws(ds$SOURCE)
ds$ASSETURL<-trimws(ds$ASSETURL)
ds$METERREADS<-trimws(ds$METERREADS)
ds$SERIALNUMBER<-trimws(ds$SERIALNUMBER)
ds$DESCRIPTION<-trimws(ds$DESCRIPTION)
ds$LOTNUMBER<-trimws(ds$LOTNUMBER)
ds$MANUFACTURER<-trimws(ds$MANUFACTURER)
ds$MODEL<-trimws(ds$MODEL)
ds$SELLERNAME<-ifelse(nchar(ds$SELLERNAME)>100, NA,ds$SELLERNAME) #71
ds$SELLERADDRESS<-ifelse(nchar(ds$SELLERADDRESS)>60, NA,ds$SELLERADDRESS) #50
ds$SELLERCITY<-ifelse(nchar(ds$SELLERCITY)>50, NA,ds$SELLERCITY)#39
ds$SELLERCITY<-ifelse(nchar(ds$SELLERCITY)<2, NA,ds$SELLERCITY)
ds$SELLERSTATE<-ifelse(nchar(ds$SELLERSTATE)>50, NA,ds$SELLERSTATE) #22
ds$SELLERSTATE<-ifelse(nchar(ds$SELLERSTATE)<2, NA,ds$SELLERSTATE)
ds$SELLERZIP<-ifelse(nchar(ds$SELLERZIP)>10, NA,ds$SELLERZIP) #17
ds$SELLERCOUNTRY<-ifelse(nchar(ds$SELLERCOUNTRY)>42, NA,ds$SELLERCOUNTRY)
ds$SERIALNUMBER<-ifelse(nchar(ds$SERIALNUMBER)>19, NA, ds$SERIALNUMBER)
ds$SERIALNUMBER<-ifelse(nchar(ds$SERIALNUMBER)<3, NA, ds$SERIALNUMBER)
ds$STOCKNUMBER<-ifelse(nchar(ds$STOCKNUMBER)>10, NA,ds$STOCKNUMBER)
ds$SERIALNUMBER[nchar(ds$SERIALNUMBER)<3]<-NA
ds$CONDITION[nchar(ds$CONDITION)<3]<-NA

#sort(unique(nchar(ds$SELLERNAME)))
#unique(ds$SERIALNUMBER[nchar(ds$SERIALNUMBER)>18&nchar(ds$SERIALNUMBER)<20])
#nrow(ds[!is.na(ds$SERIALNUMBER),])

#===============================================================================================================================
# Year Constraint
# ===============================================================================================================================
ds <- ds[which(ds$YEAR >= 1900 & ds$YEAR < year(Sys.Date())+3),]
ds <- ds[!is.na(ds$YEAR),]
ds <- ds[which(ds$YEAR!=""),]

#===============================================================================================================================
#  Meter Reads
# ===============================================================================================================================
uom <- ifelse(grepl("H", ds$METERREADS) == "TRUE", "H",  
              ifelse(grepl("KM", ds$METERREADS) == "TRUE", "K",
                     ifelse(grepl("M", ds$METERREADS) == "TRUE", "M", "H")))

reads <- round(as.numeric(gsub("[[:alpha:]]", "", ds$METERREADS), digits = 0))

reads[which(nchar(reads)>9)]<-NA
reads[which(reads==999999999|reads==99999999|reads==9999999)]<-NA



ds$METERREADS <- ifelse(!is.na(reads), paste(reads, uom, sep = " "), reads)
ds$METERREADS <- ifelse((reads == 0 & ds$YEAR < year(Sys.Date())-2), NA, ds$METERREADS)

uom<-NULL
reads<-NULL

ds$METERREADS[ds$MANUFACTURER %in% c('Kenworth','Peterbilt','Freightliner','Ford','Chevrolet','GMC','International','Isuzu','Mack','Sterling','Western Star','Hino') & grepl("H",ds$METERREADS)]<-NA
ds$METERREADS[ds$MANUFACTURER %in% c('KENWORTH','PETERBILT','FREIGHTLINER','FORD','CHEVROLET','INTERNATIONAL','ISUZU','MACK','STERLING','WESTERN STAR','HINO') & grepl("H",ds$METERREADS)]<-NA


#===============================================================================================================================
#  Zip Code
# ===============================================================================================================================
###ds[nchar(ds$SELLERZIP>=4),]
ds$SELLERZIP<-gsub("-","",ds$SELLERZIP)
ds$SELLERZIP<-ifelse((ds$SELLERCOUNTRY=="US" &nchar(ds$SELLERZIP==9)),str_sub(ds$SELLERZIP,1,5),ds$SELLERZIP)
ds$SELLERZIP[ds$SELLERCOUNTRY=="Canada" & nchar(ds$SELLERZIP)<6]<-NA


###===============================================================================================================================
#  Date Assignment
# ===============================================================================================================================
##Change mon and yr if processing in the following month/year (e.g. mon-1; yr-1)
mon<-month(Sys.Date())
yr<-year(Sys.Date())
day<-day(Sys.Date())

ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('TRCTRHOUSE')]<-paste0(mon,"/24/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('TRUCKPAPER')]<-paste0(mon,"/23/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('MACHTRADER')]<-paste0(mon,"/22/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('TRUKACCESS')]<-paste0(mon,"/21/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('MACHPETE')]<-paste0(mon,"/20/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('RCKANDDIRT','TITANMACH')]<-paste0(mon,"/19/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('CTT','EQUIPTRADE','CONEQUIPGD')]<-paste0(mon,"/18/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('PENSKEUSED','JCBUSED','WIESEUSA')]<-paste0(mon,"/17/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('MASCUS','FASTLINE','VERMEERRM','NFLEXCH','DTCHWTCH','WNEUSONWI')]<-paste0(mon,"/16/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('PUBLIQUIP','WWM','ILLINOISLIFTEQUIP')]<-paste0(mon,"/15/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('MACHFINDER','CRANEHOT','USEDGENIE')]<-paste0(mon,"/14/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('UNITEDRENT','AMERICANAG','VOLVORM','Watts')]<-paste0(mon,"/13/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('FarmerHtln','50000TRUCK','CRANETRADER')]<-paste0(mon,"/12/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('BOBCATUSED','FRMEQPCNTR')]<-paste0(mon,"/11/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('ELS','MACALLCAT','BIGGE')]<-paste0(mon,"/10/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('HertzEquip','CRANEWEB')]<-paste0(mon,"/9/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('MACHACCESS','MIDWEST','CATUSED')]<-paste0(mon,"/7/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('EQUIPREADY','eLiftTruck','INFOMINE')]<-paste0(mon,"/6/",yr)
ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('CONTRHTLN','POINT2USED','CRANENETWK')]<-paste0(mon,"/5/",yr)

ds$AUCTIONSTARTDATE<-format(ds$AUCTIONSTARTDATE,"%m/%d/%Y")

#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE=='TRUKACCESS']<-gsub(day(Sys.Date()),20,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE=='MACHPETE']<-gsub(day(Sys.Date()),18,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('RCKANDDIRT','TITANMACH')]<-gsub(day(Sys.Date()),17,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('CTT','EQUIPTRADE')]<-gsub(day(Sys.Date()),16,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('PENSKEUSED','JCBUSED','WIESEUSA')]<-gsub(day(Sys.Date()),15,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('MASCUS','FASTLINE','VERMEERRM','NFLEXCH','DTCHWTCH','WNEUSONWI')]<-gsub(day(Sys.Date()),14,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('PUBLIQUIP','WWM','ILLINOISLIFTEQUIP')]<-gsub(day(Sys.Date()),13,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('MACHFINDER','CONEQUIPGD','CRANEHOT','USEDGENIE')]<-gsub(day(Sys.Date()),12,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('UNITEDRENT','AMERICANAG','VOLVORM','Watts')]<-gsub(day(Sys.Date()),11,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('FarmerHtln','50000TRUCK','CRANETRADER')]<-gsub(day(Sys.Date()),10,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('BOBCATUSED','FRMEQPCNTR')]<-gsub(day(Sys.Date()),9,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('ELS','MACALLCAT','BIGGE')]<-gsub(day(Sys.Date()),8,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('HertzEquip','CRANEWEB')]<-gsub(day,7,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('MACHACCESS','MIDWEST','CATUSED')]<-gsub(day(Sys.Date()),5,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('EQUIPREADY','eLiftTruck','INFOMINE')]<-gsub(day(Sys.Date()),4,format(Sys.Date(),"%m/%d/%Y"))
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('CONTRHTLN','POINT2USED','CRANENETWK')]<-gsub(day(Sys.Date()),3,format(Sys.Date(),"%m/%d/%Y"))

#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE=='TRUKACCESS']<-as.Date.factor(paste0(mon,"/20/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE=='MACHPETE']<-as.Date.factor(paste0(mon,"/18/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('RCKANDDIRT','TITANMACH')]<-as.Date.factor(paste0(mon,"/17/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('CTT','EQUIPTRADE')]<-as.Date.factor(paste0(mon,"/16/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('PENSKEUSED','JCBUSED','WIESEUSA')]<-as.Date.factor(paste0(mon,"/15/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('MASCUS','FASTLINE','VERMEERRM','NFLEXCH','DTCHWTCH','WNEUSONWI')]<-as.Date.factor(paste0(mon,"/14/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('PUBLIQUIP','WWM','ILLINOISLIFTEQUIP')]<-as.Date.factor(paste0(mon,"/13/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('MACHFINDER','CONEQUIPGD','CRANEHOT','USEDGENIE')]<-as.Date.factor(paste0(mon,"/12/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('UNITEDRENT','AMERICANAG','VOLVORM','Watts')]<-as.Date.factor(paste0(mon,"/11/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('FarmerHtln','50000TRUCK','CRANETRADER')]<-as.Date.factor(paste0(mon,"/10/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('BOBCATUSED','FRMEQPCNTR')]<-as.Date.factor(paste0(mon,"/9/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('ELS','MACALLCAT','BIGGE')]<-as.Date.factor(paste0(mon,"/8/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('HertzEquip','CRANEWEB')]<-as.Date.factor(paste0(mon,"/7/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('MACHACCESS','MIDWEST','CATUSED')]<-as.Date.factor(paste0(mon,"/5/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('EQUIPREADY','eLiftTruck','INFOMINE')]<-as.Date.factor(paste0(mon,"/4/",yr),format="%m/%d/%Y")
#ds$AUCTIONSTARTDATE[ds$PARTICIPANTCODE %in% c('CONTRHTLN','POINT2USED','CRANENETWK')]<-as.Date.factor(paste0(mon,"/3/",yr),format="%m/%d/%Y")

#ds$AUCTIONSTARTDATE<-format(ds$AUCTIONSTARTDATE,"%m/%d/%Y")
ds$AUCTIONENDDATE<-ds$AUCTIONSTARTDATE
ds$Created<-ds$AUCTIONSTARTDATE

#ds$AUCTIONSTARTDATE<-trimws(ds$AUCTIONSTARTDATE)
#ds$AUCTIONENDDATE<-trimws(ds$AUCTIONENDDATE)

#ds$AUCTIONSTARTDATE<-as.Date.factor(ds$AUCTIONSTARTDATE,format="%m/%d/%Y")
#ds$AUCTIONENDDATE<-as.Date.factor(ds$AUCTIONENDDATE,format="%m/%d/%Y")

#ds$AUCTIONSTARTDATE<-ifelse(ds$AUCTIONSTARTDATE > Sys.Date(), format(Sys.Date(), "%m/%d/%Y"), format(ds$AUCTIONSTARTDATE, "%m/%d/%Y"))
#ds$AUCTIONENDDATE<-ifelse(ds$AUCTIONENDDATE > Sys.Date(), format(Sys.Date(), "%m/%d/%Y"), format(ds$AUCTIONENDDATE, "%m/%d/%Y"))

#ds$AUCTIONSTARTDATE<-ifelse(is.na(ds$AUCTIONSTARTDATE)|ds$AUCTIONSTARTDATE=="", format(Sys.Date(), "%m/%d/%Y"), format(ds$AUCTIONSTARTDATE, "%m/%d/%Y"))
#ds$AUCTIONENDDATE<-ifelse(is.na(ds$AUCTIONENDDATE)|ds$AUCTIONENDDATE=="", format(Sys.Date(), "%m/%d/%Y"), format(ds$AUCTIONENDDATE, "%m/%d/%Y"))

#ds$Created<-as.Date.factor(ds$Created,format="%m/%d/%Y")
#ds$Created<-ifelse(ds$Created > Sys.Date(), format(Sys.Date(), "%m/%d/%Y"), format(ds$Created, "%m/%d/%Y"))
#ds$Created<-ifelse(is.na(ds$Created)|ds$Created=="", format(Sys.Date(), "%m/%d/%Y"), format(ds$Created, "%m/%d/%Y"))

###===============================================================================================================================
#  Serial Number
# ===============================================================================================================================
ds$SERIALNUMBER <- gsub("*","",ds$SERIALNUMBER)
ds$SERIALNUMBER <- gsub('"',"",ds$SERIALNUMBER)
ds$SERIALNUMBER <- gsub("^NA$",NA,ds$SERIALNUMBER)
ds$SERIALNUMBER<-gsub("\\w*[*]+\\w*",NA,ds$SERIALNUMBER)
ds$SERIALNUMBER<-gsub("\\d.\\d+[E][+]\\d+",NA,ds$SERIALNUMBER)
ds$SERIALNUMBER<-gsub("\\w*[x|X]{3,20}\\w*",NA,ds$SERIALNUMBER)
ds$SERIALNUMBER<-gsub("\\w*\\?+\\w*",NA,ds$SERIALNUMBER)
ds$SERIALNUMBER[grepl("CON-RUR|\\?|(XXX)+|NONE|/|\\.|\\,|FORKLIFT|Customer Use|NONE|\\'|185CFM|\\#|BROOM|ST-FW|LIFT|ASSY|n/a|CHIPPER|LEVELER|\\&|\\!|HOOD|60IN|NEW|MANAC|Sets|\\)|\\(|SAMPLE|SAME|LAST|USE|GET|WAX|\\+|STOCK|Truck",ds$SERIALNUMBER)]<-NA
#####ds$SERIALNUMBER<-ifelse(nchar(ds$SERIALNUMBER>7, as.character(ds$SERIALNUMBER), ds$SERIALNUMBER))
#nchar>7, convert to character
#as.character(ds$SERIALNUMBER(which[(nchar(ds$SERIALNUMBER>6))]))

tp<-ds[grepl("truckpaper",ds$ASSETURL),]
ntp<-ds[!grepl("truckpaper",ds$ASSETURL),]

tpns<-tp[(is.na(tp$SERIALNUMBER)|tp$SERIALNUMBER==""),]
tpsn<-tp[!(is.na(tp$SERIALNUMBER)|tp$SERIALNUMBER==""),]

tpns$DESCRIPTION <- toupper(tpns$DESCRIPTION)

VINsearch <- c("VIN:\\s([[:alnum:]]{17});","VIN:\\s([[:alnum:]]{19});","VIN\\s([[:alnum:]]{17});","VIN\\s([[:alnum:]]{17})\\s","VIN\\s([[:alnum:]]{19});","VIN\\s([[:alnum:]]{19})\\s")
# "Vin:\\s([[:alnum:]]{17});","Vin\\s([[:alnum:]]{17})\\s","vin:\\s([[:alnum:]]{17});"
vins <- tpns[grepl(paste(VINsearch,collapse="|"),tpns$DESCRIPTION),]

vins$VIN<-NA
vins<-mutate(vins, VIN = regmatches(vins$DESCRIPTION,regexpr(paste(VINsearch, collapse='|'),vins$DESCRIPTION)))

vins$VIN<- gsub("VIN:","",vins$VIN)
vins$VIN<- gsub(";","",vins$VIN)
vins$VIN<- gsub("\\s","",vins$VIN)

vins$SERIALNUMBER<-vins$VIN
vins$VIN<-NULL

tpns2<-tpns[!tpns$ASSETURL %in% vins$ASSETURL,]


tpns<-rbind(tpns2,vins)
tp<-rbind(tpns,tpsn)
ds<-rbind(tp,ntp)
nrow(ds[(is.na(ds$SERIALNUMBER)|ds$SERIALNUMBER==""),])

###===============================================================================================================================
# Manufacturer
# ===============================================================================================================================
ds$MANUFACTURER<-trimws(ds$MANUFACTURER)
ds$MANUFACTURER<-gsub("\\#|\\.|\\'|\\,","",ds$MANUFACTURER)
ds$MANUFACTURER<-gsub(" FORKLIFTS","", ds$MANUFACTURER)
ds$MANUFACTURER<-gsub("CAT LIFT TRUCKS","CATERPILLAR", ds$MANUFACTURER)
ds<-ds[!is.na(ds$MANUFACTURER),]
ds<-ds[which(ds$MANUFACTURER!=""),]

###===============================================================================================================================
# Model
# ===============================================================================================================================
ds$MODEL<-trimws(ds$MODEL)
ds$MODEL<-toupper(ds$MODEL)
#ds$MODEL<-gsub("(\\w+)(\\.)*\\w*","\\1",ds$MODEL)
ds$MODEL<-gsub("TRENCHERS|DOZERS|LOADERS|GRADERS|GENERATORS|LIFTS|TRUCKS|EXCAVATORS|MACHINES|SCRAPERS|PIPELAYERS|CRANES|TRACTORS|ARITICULATED|DITCH|CARRIER|PADDING|MARSH","",ds$MODEL)
ds$MODEL<-gsub("HYDRA|TRACTOR|LOADER|FORKLIFT|EXCAVATOR|BACKHOE|BALER|COMPRESSOR|SKID|STEER|MOUNTED|TIRE|RUBBER|PNEUMATIC|HAYBINE|HYDRAULIC|TELESCOPIC|TERRAIN|ROUGH|PORTABLE|AIR COMPRESSOR|UTILITY|CRAWLER|WHEEL|MOTOR|GRADER|DOZER|BOOM|SEMI|ELECTRIC|MULTI|STRAIGHT|MAST|TRACK|MANLIFT|TRUCK|BUNCHER|FELLER|DIGGER|DERRICK|ASPHALT|ARTICULATING|ARTICULATED|NARROW|FLATBED|FUEL|LUBE|FUEL AND LUBE|PICKUP|CAB AND CHASSIS|GENERATOR|AG TRACTOR|MACHINE|TRAILER|STEP|DECK|GOOSENECK|SHOVEL|DUMP|LOG|COMBINE|SKIDDER|TOWABLE|STREET|SWEPPER|VAN|SEDAN|4X4|6X6|SUV|GRINDER|TANDEM|VIBRATORY|ROLLER|CRANE|WATER|SEWER|CONCRETE|MIXER|WELDER|LIGHT|TOWER|PADFOOT|BUGGY|MINI|CART|WOOD|CHIPPER|TRASH|PUMP|LIGHT|TOWER|WALK|BEHIND|COMMERCIAL|TRI|AXLE|FORESTRY|MACHANICS|SCISSOR|FARM|DIRT|GRAIN|HAMMER|CRUSHER|SCRAPER|REACH|PAVER|TELEHANDLER|PRESSURE|STUMP|BREAKER|BROOM|ENGINE|PLANT|TANK|SPLITTER|SNOW|PLOW|WASHER|DIRECTIONAL|DRILL|MULCHING|PULL|TYPE|ROTARY|2WD|4WD|AWD|MFWD|ELEVATING|BEHIND|SELF|PROPELLED|SCREW|MOBILE|PIGGYBACK|MILLING|WITH|BULLDOZER|CORN|HEAD|MOWER|CONDITIONER|PLATFORM|PORTABLE|AIR|DISC|CHISEL|PAYLOADER|FLEX|RIPPER|FIELD|CULTIVATOR|CONSTRUCTION|EARLY|RISER|TRACKED|TRENCHER|TRAILER|TELESCOPIC|REACH|PAVEMENT|MILLER|HAMMER|GARBAGE|DOUBLE|DRUM|ROLL BACK|SERVICE|REEFER|EXTENDED CAB|CREW CAB|[,]|ROLL OFF|MECHANICS|MULCHER|HYDRUALIC|[.]|RUBBER TRACK MINI|BI-DIRECTIONAL|-HYDRAULIC|-SKID|REFRIGERATED|CONVENTIONAL|=|HYDROSTATIC|TEST|GOLF|S/A|SCHOOL BUS|STAND|ORDER PICKER|ONLY|SYSTEM|SPEED LEVEL|WETTER|4 WAY|CERTIFIED PREOWNED|FOR SALE|RAMMER|COMPACTOR|HIGH SPEED|COMPACT|QUAD|POWER UNIT|TRAVERSING|REEL CARRIER|SAW|EXTENDED WARRANTY|COMBON|ELECTRONIC GUIDANCE S|DROP|RIDE ON|VACUUM|GALLON|BEACON|PIPE|BENDING","",ds$MODEL)
ds$MODEL<-gsub("^\\-(.+)?$","\\1",ds$MODEL)
ds<-ds[!is.na(ds$MODEL),]
ds<-ds[which(ds$MODEL!=""),]
ds<-ds[!grepl("SUBCATEGORY",ds$MODEL),]
# ===============================================================================================================================
# Proper/Uppercase on Address, City, State
# ===============================================================================================================================
#proper=function(s) sub("(.)", ("\\U\\1"), tolower(s), pe=TRUE)
#ds$SELLERADDRESS<-ifelse(!is.na(ds$SELLERADDRESS),proper(ds$SELLERADDRESS),NA)
ds$SELLERADDRESS<-ifelse(!is.na(ds$SELLERADDRESS),toTitleCase(tolower(ds$SELLERADDRESS)),NA)
ds$SELLERCITY<-ifelse(!is.na(ds$SELLERCITY),toTitleCase(tolower(ds$SELLERCITY)),NA)
ds$DESCRIPTION<-ifelse(!is.na(ds$DESCRIPTION),toTitleCase(tolower(ds$DESCRIPTION)),NA)
ds$SELLERNAME<-ifelse(!is.na(ds$SELLERNAME),toTitleCase(tolower(ds$SELLERNAME)),NA)
ds$CONDITION<-ifelse(!is.na(ds$CONDITION),toTitleCase(tolower(ds$CONDITION)),NA)
ds$SELLERSTATE<-ifelse(!is.na(ds$SELLERSTATE),toupper(ds$SELLERSTATE),NA)
ds$SELLERCOUNTRY<-ifelse(!is.na(ds$SELLERCOUNTRY),toupper(ds$SELLERCOUNTRY),NA)
ds$CURRENCY<-ifelse(!is.na(ds$CURRENCY),toupper(ds$CURRENCY),NA)
ds$SERIALNUMBER<-ifelse(!is.na(ds$SERIALNUMBER),toupper(ds$SERIALNUMBER),NA)
ds$SELLERCOUNTRY<-ifelse(!is.na(ds$SELLERCOUNTRY),toupper(ds$SELLERCOUNTRY),NA)
ds$SELLERSTATE<-gsub("DR. BUTLER",NA,ds$SELLERSTATE)
ds$SELLERSTATE[ds$SELLERSTATE=="AUSTRALIA"]<-NA
ds$SELLERSTATE[ds$SELLERSTATE=="GERMANY"]<-NA
ds$SELLERSTATE[ds$SELLERSTATE=="MEXICO"]<-NA
ds$SELLERSTATE<-gsub("^ISON MN$","MN",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^A IL$","IL",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^AHO FALLS ID$","ID",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^ALE MN$","MN",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^AVID CITY NE$","NE",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^CITY SD$","SD",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^EEN SD$","SD",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^E MT$","MT",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^GE MN$","MN",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^GERTON MN$","MN",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^IAPOLIS IA$","IA",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^OAK IA$","IA",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^ONIA IA$","IA",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^OON IA$","IA",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^RID IA$","IA",ds$SELLERSTATE)
ds$SELLERSTATE<-gsub("^WIN WI$","WI",ds$SELLERSTATE)
ds$SERIALNUMBER[ds$SERIALNUMBER %like% "NEW"]<-NA
ds$SERIALNUMBER[ds$SERIALNUMBER %like% "USED"]<-NA
ds$SELLERZIP[ds$SELLERZIP %like% "View"]<-NA
ds$SELLERCITY[grepl("^\\d+$",ds$SELLERCITY)]<-NA
ds$SELLERSTATE[grepl("^\\d+$",ds$SELLERSTATE)]<-NA
ds$SELLERCOUNTRY[grepl("^\\d+$",ds$SELLERCOUNTRY)]<-NA
ds$SELLERCOUNTRY<-gsub("^AND TOBAGO$","",ds$SELLERCOUNTRY)
ds$SELLERCITY<-gsub("^\\(Marshall)$","Marshall",ds$SELLERCITY)
ds$SELLERCITY<-gsub("^\\Aberdeen  Nh$","Aberdeen",ds$SELLERCITY)
ds$SELLERCITY<-gsub("^\\Aberdeen  NH$","Aberdeen",ds$SELLERCITY)
ds$SELLERCITY<-gsub("^\\Aberdeen  Cih$","Aberdeen",ds$SELLERCITY)
ds$SELLERCITY<-gsub("^\\Aberdeen  CIH$","Aberdeen",ds$SELLERCITY)
ds$SELLERCITY<-gsub("^\\MONTANA,$","",ds$SELLERCITY)
ds$SELLERCITY<-gsub("^\\SACRAMENTO, CA$","",ds$SELLERCITY)

#===============================================================================================================================
#  Currency Fill
# ===============================================================================================================================
ds$CURRENCY[ds$SELLERCOUNTRY=="US" & is.na(ds$CURRENCY)]<-"USD"
ds$CURRENCY[ds$SELLERCOUNTRY=="US" & ds$CURRENCY==""]<-"USD"
ds$CURRENCY[ds$PARTICIPANTCODE=="CRANETRADER"] <- "USD"
ds$CURRENCY[ds$PARTICIPANTCODE=="USEDGENIE"] <- "USD"

#===============================================================================================================================
#  Country Fill
# ===============================================================================================================================
ds$SELLERCOUNTRY[is.na(ds$SELLERCOUNTRY) & ds$SELLERSTATE %in% c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY',
                                                                 'LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND',
                                                                 'OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')]<-"US"
ds$SELLERCOUNTRY[ds$SELLERCOUNTRY=="" & ds$SELLERSTATE %in% c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY',
                                                                 'LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND',
                                                                 'OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')]<-"US"

ds$SELLERCOUNTRY[ds$SELLERSTATE %in% c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY',
                                                              'LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND',
                                                              'OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')]<-"US"

ds$SELLERCOUNTRY[ds$SELLERCOUNTRY=="" & ds$SELLERSTATE %in% c('ALABAMA','ALASKA','ARIZONA','ARKANSAS','CALIFORNIA','COLORADO','CONNECTICUT','DELAWARE',
                                                             'FLORIDA','GEORGIA','HAWAII','IDAHO','ILLINOIS','INDIANA','IOWA','KANSAS','KENTUCKY',
                                                             'LOUISIANA','MAINE','MARYLAND','MASSACHUSETTS','MICHIGAN','MINNESOTA','MISSISSIPPI',
                                                             'MISSOURI','MONTANA','NEBRASKA','NEVADA','NEW HAMPSHIRE','NEW JERSEY','NEW MEXICO',
                                                             'NEW YORK','NORTH CAROLINA','NORTH DAKOTA','OHIO','OKLAHOMA','OREGON','PENNSYLVANIA',
                                                             'RHODE ISLAND','SOUTH CAROLINA','SOUTH DAKOTA','TENNESSEE','TEXAS','UTAH','VERMONT',
                                                             'VIRGINIA','WASHINGTON','WEST VIRGINIA','WISCONSIN','WYOMING')]<-"US"
ds$SELLERCOUNTRY[is.na(ds$SELLERCOUNTRY) & ds$SELLERSTATE %in% c('ALABAMA','ALASKA','ARIZONA','ARKANSAS','CALIFORNIA','COLORADO','CONNECTICUT','DELAWARE',
                                                              'FLORIDA','GEORGIA','HAWAII','IDAHO','ILLINOIS','INDIANA','IOWA','KANSAS','KENTUCKY',
                                                              'LOUISIANA','MAINE','MARYLAND','MASSACHUSETTS','MICHIGAN','MINNESOTA','MISSISSIPPI',
                                                              'MISSOURI','MONTANA','NEBRASKA','NEVADA','NEW HAMPSHIRE','NEW JERSEY','NEW MEXICO',
                                                              'NEW YORK','NORTH CAROLINA','NORTH DAKOTA','OHIO','OKLAHOMA','OREGON','PENNSYLVANIA',
                                                              'RHODE ISLAND','SOUTH CAROLINA','SOUTH DAKOTA','TENNESSEE','TEXAS','UTAH','VERMONT',
                                                              'VIRGINIA','WASHINGTON','WEST VIRGINIA','WISCONSIN','WYOMING')]<-"US"

ds$SELLERCOUNTRY[ds$SELLERSTATE %in% c('ALABAMA','ALASKA','ARIZONA','ARKANSAS','CALIFORNIA','COLORADO','CONNECTICUT','DELAWARE',
                                                                 'FLORIDA','GEORGIA','HAWAII','IDAHO','ILLINOIS','INDIANA','IOWA','KANSAS','KENTUCKY',
                                                                 'LOUISIANA','MAINE','MARYLAND','MASSACHUSETTS','MICHIGAN','MINNESOTA','MISSISSIPPI',
                                                                 'MISSOURI','MONTANA','NEBRASKA','NEVADA','NEW HAMPSHIRE','NEW JERSEY','NEW MEXICO',
                                                                 'NEW YORK','NORTH CAROLINA','NORTH DAKOTA','OHIO','OKLAHOMA','OREGON','PENNSYLVANIA',
                                                                 'RHODE ISLAND','SOUTH CAROLINA','SOUTH DAKOTA','TENNESSEE','TEXAS','UTAH','VERMONT',
                                                                 'VIRGINIA','WASHINGTON','WEST VIRGINIA','WISCONSIN','WYOMING')]<-"US"

ds$SELLERCOUNTRY[is.na(ds$SELLERCOUNTRY) & ds$SELLERSTATE %in% c('AB','BC','MB','NB','NL','NS','NT','NU','ON','PE','QC','SK','YT')]<-"CANADA"

ds$SELLERCOUNTRY[ds$SELLERCOUNTRY=="" & ds$SELLERSTATE %in% c('AB','BC','MB','NB','NL','NS','NT','NU','ON','PE','QC','SK','YT')]<-"CANADA"

ds$SELLERCOUNTRY[ds$SELLERSTATE %in% c('AB','BC','MB','NB','NL','NS','NT','NU','ON','PE','QC','SK','YT')]<-"CANADA"

ds$SELLERCOUNTRY[is.na(ds$SELLERCOUNTRY) & ds$SELLERSTATE %in% c('ALBERTA','BRITISH COLUMBIA','MANITOBA','NEW BRUNSWICK','NEWFOUNDLAND AND LABRADOR',
                                                                 'NOVA SCOTIA','NORTHWEST TERRITORIES','NUNAVUT','ONTARIO','PRINCE EDWARD ISLAND',
                                                                 'QUEBEC','SASKATCHEWAN','YUKON')]<-"CANADA"

ds$SELLERCOUNTRY[ds$SELLERCOUNTRY=="" & ds$SELLERSTATE %in% c('ALBERTA','BRITISH COLUMBIA','MANITOBA','NEW BRUNSWICK','NEWFOUNDLAND AND LABRADOR',
                                                                 'NOVA SCOTIA','NORTHWEST TERRITORIES','NUNAVUT','ONTARIO','PRINCE EDWARD ISLAND',
                                                                 'QUEBEC','SASKATCHEWAN','YUKON')]<-"CANADA"

ds$SELLERCOUNTRY[ds$SELLERSTATE %in% c('ALBERTA','BRITISH COLUMBIA','MANITOBA','NEW BRUNSWICK','NEWFOUNDLAND AND LABRADOR',
                                                              'NOVA SCOTIA','NORTHWEST TERRITORIES','NUNAVUT','ONTARIO','PRINCE EDWARD ISLAND',
                                                              'QUEBEC','SASKATCHEWAN','YUKON')]<-"CANADA"


ds$SELLERCOUNTRY[ds$PARTICIPANTCODE %in% c('BIGGE','FarmerHtln')]<-"US"
ds$SELLERCOUNTRY[ds$PARTICIPANTCODE=="PUBLIQUIP"]=""
ds$CURRENCY[ds$PARTICIPANTCODE=="PUBLIQUIP"&ds$CURRENCY=="CDN"]="CAD"
ds$SELLERCOUNTRY[ds$PARTICIPANTCODE=="PUBLIQUIP"&ds$CURRENCY=="CAD"]<-"CANADA"
ds$SELLERCOUNTRY[ds$PARTICIPANTCODE=="PUBLIQUIP"&ds$CURRENCY=="USD"]<-"US"

ds$SELLERCOUNTRY[ds$SELLERSTATE %like% "ARGANDA DEL RAY"]<-"SPAIN"
ds$SELLERCOUNTRY[ds$SELLERSTATE %like% "BANGKOK"]<-"THAILAND"
ds$SELLERCOUNTRY[ds$SELLERSTATE %like% "DE FRANCE"]<-"FRANCE"
ds$SELLERCOUNTRY[ds$SELLERSTATE %like% "KRASNOGORSK DISTRICT"]<-"RUSSIA"
ds$SELLERCOUNTRY[ds$SELLERSTATE %like% "MUNICH"]<-"GERMANY"
ds$SELLERCOUNTRY[ds$SELLERSTATE %like% "POLAND"]<-"POLAND"
ds$SELLERCOUNTRY[ds$SELLERSTATE %like% "SANTA CATARINA"]<-"MEXICO"
ds$SELLERCOUNTRY[ds$SELLERSTATE %like% "SLOVENIA"]<-"SLOVENIA"
ds$SELLERCOUNTRY[ds$SELLERSTATE %like% "SOUTH KOREA"]<-"SOUTH KOREA"
ds$SELLERCOUNTRY[ds$SELLERSTATE %like% "TAMAN PERINDUSTRIAN PUCHONG UTAMA"]<-"MALAYSIA"


###===============================================================================================================================
# SURVEYTYPE Replacement
# ===============================================================================================================================
ds$SURVEYTYPE<-gsub("^R$","Resale", ds$SURVEYTYPE)
ds$SURVEYTYPE[(is.na(ds$SURVEYTYPE)|ds$SURVEYTYPE=="")]<-"Resale"

# ===============================================================================================================================
# Remove Null Make/Model/Year
# ===============================================================================================================================
ds$YEAR <- as.numeric(gsub("[[:alpha:]]", "", ds$YEAR))
ds <- ds[which(grepl(pattern = "(19|20)[0-9]{2}", x=ds$YEAR)),]
ds<-ds[!is.na(ds$MANUFACTURER),]
ds<-ds[which(ds$MANUFACTURER!=""),]
ds<-ds[which(nchar(ds$MANUFACTURER)!=1),]
ds$MANUFACTURER<-trimws(ds$MANUFACTURER)
ds<-ds[which(!grepl("^\\s+$",ds$MANUFACTURER)),]
ds<-ds[which(!grepl("\\d+",ds$MANUFACTURER)),]
ds<-ds[!is.na(ds$MODEL),]
ds<-ds[which(ds$MODEL!=""),]
ds<-ds[which(nchar(ds$MODEL)!=1),]
ds$MODEL<-trimws(ds$MODEL)
ds<-ds[which(!grepl("^\\s+$",ds$MODEL)),]

# ===============================================================================================================================
# Remove Leading Special Characters
# ===============================================================================================================================

ds$CATEGORY<-gsub("(\\-|\\+|\\#|\\=|\\=-)(.+)","\\2",ds$CATEGORY)
ds$CURRENCY<-gsub("(\\-|\\+|\\#|\\=|\\=-)(.+)","\\2",ds$CURRENCY)
ds$MANUFACTURER<-gsub("(\\-|\\+|\\#|\\=|\\=-)(.+)","\\2",ds$MANUFACTURER)
ds$MODEL<-gsub("(\\-|\\+|\\#|\\=|\\=-)(.+)","\\2",ds$MODEL)
ds$DESCRIPTION<-gsub("(\\-|\\+|\\#|\\=|\\=-)(.+)","\\2",ds$DESCRIPTION)
ds$SERIALNUMBER<-gsub("(\\-|\\+|\\#|\\=|\\=-)(.+)","\\2",ds$SERIALNUMBER)
ds$CONDITION<-gsub("(\\-|\\+|\\#|\\=|\\=-)(.+)","\\2",ds$CONDITION)
ds$ASSETURL<-gsub("(\\-|\\+|\\#|\\=|\\=-)(.+)","\\2",ds$ASSETURL)
ds$STOCKNUMBER<-gsub("(\\-|\\+|\\#|\\=|\\=-)(.+)","\\2",ds$STOCKNUMBER)
ds$SOURCE<-gsub("(\\-|\\+|\\#|\\=|\\=-)(.+)","\\2",ds$SOURCE)
#(, "

#===============================================================================================================================
# Dedup Process
# ==============================================================================================================================
dd<-ds[ds$PARTICIPANTCODE %in% c('CTT','MACHPETE','RCKANDDIRT','ELS','MASCUS','UNITEDRENT','CONTRHTLN','FASTLINE','HertzEquip','AMERICANAG','POINT2USED',
                             'FarmerHtln','VOLVORM','TRUKACCESS','PENSKEUSED','50000TRUCK','CRANENETWK','BOBCATUSED','CRANEHOT','EQUIPREADY',
                             'FRMEQPCNTR','VERMEERRM','MIDWEST','eLiftTruck','PUBLIQUIP','JCBUSED','CRANEWEB','USEDGENIE','MACALLCAT','INFOMINE',
                             'CATUSED','EQPSELLSIT','NFLEXCH','BIGGE','DTCHWTCH','WNEUSONWI','CRANETRADER','ILLINOISLIFTEQUIP'),]
nd<-ds[!ds$PARTICIPANTCODE %in% c('CTT','MACHPETE','RCKANDDIRT','ELS','MASCUS','UNITEDRENT','CONTRHTLN','FASTLINE','HertzEquip','AMERICANAG','POINT2USED',
                                'FarmerHtln','VOLVORM','TRUKACCESS','PENSKEUSED','50000TRUCK','CRANENETWK','BOBCATUSED','CRANEHOT','EQUIPREADY',
                                'FRMEQPCNTR','VERMEERRM','MIDWEST','eLiftTruck','PUBLIQUIP','JCBUSED','CRANEWEB','USEDGENIE','MACALLCAT','INFOMINE',
                                'CATUSED','EQPSELLSIT','NFLEXCH','BIGGE','DTCHWTCH','WNEUSONWI','CRANETRADER','ILLINOISLIFTEQUIP'),]
dd<-dd[!duplicated(dd$ASSETURL),]
ds<-rbind(nd,dd)

###===============================================================================================================================
# Write output
# ===============================================================================================================================

dim(ds)

ds <- ds[order(ds$CATEGORY, ds$MANUFACTURER, ds$MODEL),]

table(ds$PARTICIPANTCODE)

ds$PARTICIPANTCODE <- factor(ds$PARTICIPANTCODE)


for (id in levels(ds$PARTICIPANTCODE)){
  
  tmp <- ds[ds$PARTICIPANTCODE == id,]
  fn <- paste(dirOut, id, "_", format(Sys.Date(), format = "%Y%m%d"), ".csv", sep = '')
  write.csv(tmp, fn, row.names = FALSE, na = "")
  
}
