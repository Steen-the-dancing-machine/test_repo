rm(list = ls());
if(class(dev.list()) == "integer") dev.off(dev.list()["RStudioGD"]);   # if some plots are open, close them
#=== === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === ===
#
#                                                      Load data and export in ... format
#
#=== === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === === ===
#'     R script -
#'     Steen Wettstein fot the ITS ERSD - 22.01.2018
#'     Instructions: 
#'     Short cuts:   alt + O             to reduce all the chapters
#'                   alt + Shift + O     to open all the chapters
#'                   Ctrl + Shift + O    to open list of chapters in the margin
#'     Description :   ....
#=== === === === === === === === === === === === === ===  === === === === === === === === === === === === === === === === === === === === === === === === ===

# Set working directory
  directory <- 'C:/Users/STEEN/Dropbox/autre/R/queries for Ying'
  setwd(directory)
  path_web_first <- paste0(directory,'/Data from web')

# date today
  Today <- Sys.Date()

# Create Folder for data of Today 
  if(!file.exists(paste0(path_web_first,'/',Today))) dir.create(file.path(path_web_first,Today))
  path_web <- paste0(path_web_first,'/',Today)
  
#  Notes                --------------------------------------------------------------------
  # check Kazakhstan website is not available
  
#  Functions            --------------------------------------------------------------

#' loading.package 
#' This function installs packages that are not installed 
#' and load packages not loaded
loading.package <- function(list.of.packages = names){ 
  
  # check if the packages are in memory
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  # if they are not install them
  if(length(new.packages)>0) {install.packages(new.packages)}
  
  # if they are not in the usable list of packages, load them
  # if(sum(paste0("package:",list.of.packages) %in% search()) >0) {
  
  sub <- list.of.packages[!paste0("package:",list.of.packages) %in% search()]
  explevel <- paste0( "require(",list.of.packages,")")
  for(name in sub){eval(parse(text=explevel))} # each of the text expressions e.g. library(foreach)
  # }
  
} # end function

# clear console
clear <- function() cat("/014")


#  Packages             ----------------------------------------------------------------

loading.package(c(
      'eurostat',     # contains function to pull out data directly from Eurostat 
      'CANSIM2R',     # contains function to pull out data directly from Statistics Canada 
      # 'xlsx',       # to read and write excel files - requires R-32 bit
      'magrittr',     # allows other way of witing code, including the pipes '%>%' for nested functions 
      'readxl',      
      'RODBC',        # to connect to a server
      'RCurl', 
      'tidyr',        # An evolution of 'reshape2' for data management (see e.g. spread / gather) 
      'dplyr',        # A fast, consistent tool for working with data frame like objects, both in memory and out of memory
      'pdftables',    # Allows the user to convert PDF tables to formats more amenable to analysis ('.csv', '.xml', or '.xlsx') by wrapping the PDFTables API. In order to use the package, the user needs to sign up for an API account on the PDFTables website (<https://pdftables.com/pdf-to-excel-api>).
      'curl',
      'Rcurl',
      'xml2',         # Work with XML files using a simple, consistent interface.
      'openxlsx'      # allows to get cells (and much more) info in a Excel Spreadsheet. It provides a high level interface to writing, styling and editing worksheets
))



#   - North America                  -----------------------------------------------------------

#           Canada                      -----------------------------------------------------------

Trade_CAN_raw <- getCANSIM(02280059, showLabels = TRUE, raw = TRUE) %>% group_by() %>% filter(NAPCS == 'Total of all merchandise (x 1,000,000)' &
                                                                                               Vector %in% c('v54056840','v54056284')) # v54056840 corresponds to Customs exports and the other for Customs imports (not BOP) 

# Exchange rate
# Ex_rate_CAN <- getCANSIM(1530114, showLabels = TRUE, raw = TRUE)

Trade_CAN_long <- Trade_CAN_raw %>% mutate(YEAR = as.character(substr(Ref_Date,1,4)), MONTH = as.character(substr(Ref_Date,6,7)),
                                           DATE = paste0('01.',MONTH,'.',YEAR)) %>%  filter(YEAR > 2005 & SA == 'Unadjusted')

# ex rate 
  

# Dates 
Trade_CAN_long$TIME = as.Date(Trade_CAN_long$DATE, format =  "%d.%m.%Y")

# convert final
Trade_final_CAN <- Trade_CAN_long %>% select(-DATE,-YEAR,-MONTH,-BASIS,-Ref_Date,-SA,-NAPCS,-Vector,-Coordinate,-GEO) %>% 
                    dplyr::rename(FLOW = TRADE, VALUE = Value) %>% 
                    mutate(COUNTRY = 'Canada  FOB', last_update = Today , PARTNER = 'WORLD' )


#           Mexico                      -----------------------------------------------------------

# information : 
    # - For import
    #   in N:\_Stat\Its\Statistical_Programmes\Total_merchandise_values\Press Release 2018\Work 
    #   take sheet "Mexico" yellow cell at the bottom is the growth rate to apply 
    #   the latest month has to be estimated 
    #   not necessary to update the Excel file

#           United States of America    -----------------------------------------------------------

# load data automatically
  filename.US = paste0(path_web,"/",Today," - Unites States - Exports, Imports and Balance of Goods.xls")
  url <- 'https://www.census.gov/foreign-trade/Press-Release/current_press_release/exh3s.xls' # right click on the link and copy the adress 
  download.file(url = url, destfile = filename.US, mode="wb")  # load data from website and write in folder created

# read file
  Trade_US_raw <- read_xls(path = filename.US,sheet = 1, skip = 6)
  
# Date
  Trade_US_raw$YEAR <- Trade_US_raw$X__1
  
  Trade_US_raw_selected <- Trade_US_raw[(which(Trade_US_raw$Customs == 'Not Seasonally Adjusted')+1):nrow(Trade_US_raw),]
  
# transform date format
  Trade_US_raw_selected$YEAR[which(substr( Trade_US_raw$YEAR,1,2) == '20')[1]: (which(substr( Trade_US_raw_selected$YEAR,1,2) == '20')[2]-1)] <-
                              Trade_US_raw$YEAR[which(substr( Trade_US_raw$YEAR,1,2) == '20')[1]] 
  
  Trade_US_raw_selected$YEAR[which(substr( Trade_US_raw$YEAR,1,2) == '20')[2]: nrow(Trade_US_raw_selected)] <- 
                              Trade_US_raw$YEAR[which(substr( Trade_US_raw$YEAR,1,2) == '20')[2]] 
  
# Transform months
  Trade_US_raw_TIME <- 
                  Trade_US_raw_selected %>% mutate( MONTH = case_when(
                                            substr(X__1, 1,4) == 'Janu' ~ 1,
                                            substr(X__1, 1,4) == 'Febr' ~ 2,
                                            substr(X__1, 1,4) == 'Marc' ~ 3,
                                            substr(X__1, 1,4) == 'Apri' ~ 4,
                                            substr(X__1, 1,3) == 'May' ~ 5,
                                            substr(X__1, 1,4) == 'June' ~ 6,
                                            substr(X__1, 1,4) == 'July' ~ 7,
                                            substr(X__1, 1,4) == 'Augu' ~ 8,
                                            substr(X__1, 1,4) == 'Sept' ~ 9,
                                            substr(X__1, 1,4) == 'Octo' ~ 10,
                                            substr(X__1, 1,4) == 'Nove' ~ 11,
                                            substr(X__1, 1,4) == 'Dece' ~ 12
                                            )) %>% 
                              filter(!is.na(MONTH)) %>%   
                              mutate( TIME = as.Date(paste0('01.',MONTH,'.',YEAR), format =  "%d.%m.%Y")) %>%  
                              select(`F.A.S. Value`, `C.I.F. Value` ,TIME )
  
 # FINAL 
  Trade_final_US   <-   Trade_US_raw_TIME %>% dplyr::rename(Export = `F.A.S. Value`, Import = `C.I.F. Value`)  %>% 
                        mutate(COUNTRY = 'United States of America',last_update = Today , PARTNER = 'WORLD') %>% 
                        gather_('FLOW','VALUE',names(.)[names(.) %in% c('Export','Import')]) 
                        
  
  
#   - South and Central America         ----------------------------------------------
#           Argentina                   -----------------------------------------------------------
#           Brazil                      -----------------------------------------------------------
#           Chile                       -----------------------------------------------------------
#           Colombia                    -----------------------------------------------------------
#           Peru                        -----------------------------------------------------------



#   - Other South & Central America  -----------------------------------------
#           Bolivia, Pluri. state of    -----------------------------------------------------------
#           Costa Rica                  -----------------------------------------------------------
#           El Salvador                 -----------------------------------------------------------

# information 
  # Enero 2006 - up to date
  # select Exportaciones; Importacioens 
    
# Trade_Salvador_raw <- 
  url.SAL <- 'http://www.bcr.gob.sv/bcrsite/downloadsxls.php?exportCDR=1&cdr=38&xls=1'  
  filename.KAZ.xsl <- paste0(path_web,'/',Today,'-',' Banlanza Comercial Salvador.xls')
  
  # write file 
  download.file(url = url.SAL, destfile = filename.KAZ.xsl, mode="w")  # load data from website and write in folder created
  
  # http://www.bcr.gob.sv/bcrsite/downloadsxls.php?exportCDR=1&cdr=38&xls=1
  
  # doesnt work when I save the file it is not an .xls but web file (html?)
  
 #  loading.package('XML')
 #  readHTMLTable(url.SAL,which=1)
 # 
 # library(RCurl);
 #  tabs = getURL(url.SAL)
 #  
  
#           Guatemala                   -----------------------------------------------------------
  
  # Guatemala:
  #   http://www.banguat.gob.gt/inc/ver.asp?id=/estaeco/comercio/por_producto/prod_mensDB001.HTM&e=118504
  # 
  
  
#           Paraguay                    -----------------------------------------------------------
#           Ecuador                     -----------------------------------------------------------
#           Uruguay                     -----------------------------------------------------------


#   - Europe                         ---------------------------------------------------------------

# Trade
  Trade_EU_raw <- get_eurostat('ext_st_28msbec') %>% 
                  filter(bec == 'TOTAL' & partner %in% c('EU28','EXT_EU28','WORLD') & time > '2005-12-31' & stk_flow %in% c('EXP','IMP') & indic_et =='TRD_VAL')
  
# Exchange rate
  Ex_rate_EU_raw <- get_eurostat('ert_bil_eur_m') %>% filter(currency == 'USD' & statinfo == 'AVG')


#  Transform data     
  
# EXPORT : EU trade Intra-EU (28) -Extra-EU (28)
  Trade_EU_long_usd <- Trade_EU_raw %>% left_join(Ex_rate_EU_raw %>% select(time, values) %>% dplyr::rename(exrate = values)) %>% 
                              mutate(values_USD  = values * exrate) %>% select(-values,-exrate) 
  
  Trade_EU_long_EU_INT_EXT <- Trade_EU_long_usd %>% filter(partner %in% c('EU28', 'EXT_EU28')) %>% 
                                 group_by(stk_flow,indic_et,bec,partner,time) %>% dplyr::summarise(values_USD = sum(values_USD)) %>% mutate(geo = 'EU_28') 
                  
  Trade_EU_wide_EU_INT_EXT <- Trade_EU_long_EU_INT_EXT 
  
# IMPORT : Eu trade Intra-EU (28) - Extra-EU(28)
  Trade_EU_wide_INT_EXT_I <- Trade_EU_wide_EU_INT_EXT %>% filter(partner == 'EU28' & stk_flow == 'EXP') %>% 
                                   rbind(  Trade_EU_wide_EU_INT_EXT %>% filter(partner == 'EXT_EU28' & stk_flow == 'IMP'))
  
  
# EU (28)
    # Exports
  Trade_EU_long_EU_WLD_X <- Trade_EU_long_usd %>% filter(partner == 'WORLD' &  stk_flow == 'EXP') %>%   
                                group_by(stk_flow,indic_et,bec,partner,time) %>% dplyr::summarise(values_USD = sum(values_USD)) %>% 
                                mutate(geo = 'EU_28') %>% ungroup 

  Trade_EU_wide_EU_WLD_X <- Trade_EU_long_EU_WLD_X %>% mutate(partner = 'WORLD' )
  
    # Imports  
  Trade_EU_long_EU_WLD_I <- Trade_EU_long_usd %>% filter(partner == 'EU28' &  stk_flow == 'EXP') %>% 
                                   rbind(Trade_EU_long_usd %>% filter(partner == 'EXT_EU28' &  stk_flow == 'IMP')) %>% select(-stk_flow) %>% 
                                group_by(indic_et,bec,time) %>% dplyr::summarise(values_USD = sum(values_USD)) %>% mutate(geo = 'Intra-EU (28)') %>% ungroup 
  
  Trade_EU_wide_EU_WLD_I <- Trade_EU_long_EU_WLD_I %>% mutate(stk_flow = 'IMP',partner = 'WORLD' )
  
  
# all individual countries 
  # Exports
  Trade_EU_wide_country_WLD_I <-  Trade_EU_long_usd %>% filter(partner == 'WORLD' & stk_flow == 'IMP')   %>% mutate(partner = 'WORLD')

  # Imports
  Trade_EU_wide_country_WLD_X <-  Trade_EU_long_usd %>% filter(partner == 'WORLD' & stk_flow == 'EXP')   %>% mutate(partner = 'WORLD')
  
  
# order the names 
  order_names         <- read.csv( file =  'N:/_Stat/Its/Statistical_Programmes/Short-term time series/Monthly_merchandise_trade_values/R queries/order_names.csv')
  order_names_partner <- read.csv( file =  'N:/_Stat/Its/Statistical_Programmes/Short-term time series/Monthly_merchandise_trade_values/R queries/order_names_partne.csv')
  
# Final table stored in two tables (Export and Import)

 # Export
  Trade_inter_EU_X <-
    rbind(as.data.frame(Trade_EU_wide_EU_WLD_X),
          as.data.frame(Trade_EU_wide_EU_INT_EXT %>% filter(stk_flow == 'EXP')),
          as.data.frame(Trade_EU_wide_country_WLD_X)
  ) %>%  left_join(order_names) %>% arrange(order) %>% 
        left_join((order_names_partner)) %>% mutate( ORDER = ifelse(!is.na(order_p), order_p, order)) %>%
      select(ORDER, everything()) %>% arrange(ORDER)  %>%  mutate(FLOW = 'Export')
  
  # Import
  Trade_inter_EU_I <-
    rbind(as.data.frame(Trade_EU_wide_EU_WLD_I),
          as.data.frame(Trade_EU_wide_INT_EXT_I),
          as.data.frame(Trade_EU_wide_country_WLD_I)
    ) %>%  left_join(order_names) %>% arrange(order) %>% select(-order) %>%  left_join(order_names) %>% arrange(order) %>% 
    left_join((order_names_partner)) %>% mutate( ORDER = ifelse(!is.na(order_p), order_p, order)) %>%
    select(ORDER, everything()) %>% arrange(ORDER) %>% mutate(FLOW = 'Import')
  
 # final 
  Trade_inter_EU_ORDER <- rbind(Trade_inter_EU_X,Trade_inter_EU_I) %>% group_by()  %>% 
                          mutate( last_update = Today, TIME = as.Date(time, format =  "%d.%m.%Y") ) %>%
                          dplyr::rename(VALUE = values_USD,  PARTNER = partner)
  
  Trade_final_EU <-       Trade_inter_EU_ORDER %>% select(-stk_flow,-indic_et,-ORDER,-order,-order_p,-bec) 
  
  
# change name countries  
  corr_cou <- read_excel('N:/_Stat/Its/Statistical_Programmes/Short-term time series/Monthly_merchandise_trade_values/R queries/country_correspondance.xlsx')
  
  Trade_final_EU %<>% left_join(corr_cou, by = "geo") %>% mutate( COUNTRY = ifelse(is.na(Name),geo,Name)) %>% select(-geo,-Name, -time)
  
  Trade_final_EU %<>% mutate(TIME = as.Date(paste0('01.',substr(Trade_final_EU$TIME,6,7),'.',substr(Trade_final_EU$TIME,1,4)), format =  "%d.%m.%Y"))
  
#           Switzerland                 ---------------------------------------------------------------------
  
  # information : Jan 2012 - up to date, only Import and Export and convert it in USD
  
#           Iceland                     ----------------------------------------------------------------------------

# Information 
    # -- Select latest year (from 2010 -> copy the rest from old Excel file)
    # with  Export fob and import cif
  
   # http://px.hagstofa.is/pxen/pxweb/en/Efnahagur/Efnahagur__utanrikisverslun__1_voruvidskipti__01_voruskipti/UTA06002.px/table/tableViewLayout1/?rxid=16172cfa-dd90-4098-b6e5-5d9b9df0f9b4&downloadfile=FileTypeCsvWithHeadingAndSemiColon
  
#           Norway                      ----------------------------------------------------------------------------  
  
# Information  
  # select Value , Total imports, total exports, and Select latest year (from 2006)
  
#           Turkey                      ----------------------------------------------------------------------------  
  # Turkey:
  #   http://www.turkstat.gov.tr/PreTablo.do?alt_id=1046
  
  
  # Statistical Tables and Dynamis Search -> Statistiical tables -> Foreign Trade by Years -> Foreingn Trade by Months
  # link adress 
  # http://www.turkstat.gov.tr/PreIstatistikTablo.do?istab_id=1544
  
  
#   - Commonwealth of Ind. States   --------------------------------------
#           Belarus                     -----------------------------------------------------------
#           Kazakhstan                  -----------------------------------------------------------
  
# import and write word doc
  filename.KAZ = paste0(path_web,"/",Today," - Kazakhstan - Foreign Trade.doc")
  filename.KAZ.xml = paste0(path_web,"/",Today," - Kazakhstan - Foreign Trade.xml")
  filename.KAZ.docx = paste0(path_web,"/",Today," - Kazakhstan - Foreign Trade.docx")
  
# generate url  
  url.KAZ <- 'http://stat.gov.kz/getImg?id=ESTAT105723'   # find a way to automate the way this link is generated
  
# write file 
  download.file(url = url.KAZ, destfile = filename.KAZ.docx, mode="wb")  # load data from website and write in folder created

# change formal to word xml    == == == == == == == == == == == == == == == == == ==
 
# find the files that you want
  list.of.files <- list.files(paste0(path_web,'/'))
  
# copy the files to the new folder
  file.copy(filename.KAZ, filename.KAZ.xml)
  
# read word file               == == == == == == == == == == == == == == == == == ==  
  # loading.package('readtext')
  # Trade_KAZ_raw <- readtext(filename.KAZ)
  
  # loading.package('xml2')
  Trade_KAZ_raw <-  read_xml( paste0(path_web,"/",Today," - Kazakhstan - Foreign trade2.xml"))
  
# there is an egregious use of namespaces in these files
  ns <- xml_ns(Trade_KAZ_raw)
  
  # extract all the table cells (this is assuming one table in the document)
  cells <- xml_find_all(Trade_KAZ_raw, ".//w:tbl/w:tr/w:tc", ns=ns)
  
  # convert the cells to a matrix then to a data.frame)
  dat <- data.frame(matrix(xml_text(cells), ncol=14, byrow=TRUE), stringsAsFactors=FALSE)

dat_sel <- dat[4:nrow(dat),-ncol(dat)]

names(dat_sel) <- c('YEAR','01','02','03','04','05','06','07','08','09','10','11','12')
  
cut_kaz <- which(substr(dat_sel[,1],1,2) != '20')

# extract 3 tables
  Trade_Foreign_KAZ <- dat_sel[1:(min(cut_kaz)-1),]
  Trade_KAZ_X <-       dat_sel[-c(1:(min(cut_kaz)-1), which(substr(dat_sel[,1],1,2) != '20'),(max(cut_kaz)+1):nrow(dat_sel)),]      
  Trade_KAZ_I <-       dat_sel[(max(cut_kaz)+1):nrow(dat_sel),]

# format date and time series
  # export
  Trade_inter_KAZ_X <- Trade_KAZ_X %>% gather('MONTH','VALUE',names(.)[names(.) %in% c('01','02','03','04','05','06','07','08','09','10','11','12')]) %>% 
                                        dplyr::mutate( TIME = as.Date(paste0('01.',MONTH,'.',YEAR),format =  "%d.%m.%Y")) %>% 
                                        mutate(COUNTRY = 'Kazakhstan', FLOW = 'Export', last_update = Today )
   
  Trade_inter_KAZ_I <- Trade_KAZ_I %>% gather('MONTH','VALUE',names(.)[names(.) %in% c('01','02','03','04','05','06','07','08','09','10','11','12')]) %>% 
                                        dplyr::mutate( TIME = as.Date(paste0('01.',MONTH,'.',YEAR),format =  "%d.%m.%Y")) %>% 
                                        mutate(COUNTRY = 'Kazakhstan', FLOW = 'Import', last_update = Today )
  
                           
  Trade_final_KAZ <- rbind(Trade_inter_KAZ_X,Trade_inter_KAZ_I) %>% select(-MONTH,-YEAR) %>% group_by()  %>%  
                        mutate(PARTNER = 'WORLD')  
  
# TRANSFORM VALUE INTO NUMERIC  
  Trade_final_KAZ$VALUE <-  gsub(substr(Trade_final_KAZ$VALUE[1],2,2), "", Trade_final_KAZ$VALUE) # remove spacese
  
  # pbl ! 
  Trade_final_KAZ$VALUE[Trade_final_KAZ$VALUE == "2 765,1"] <- "2765,1"
  
  Trade_final_KAZ$VALUE <-  gsub(',', ".", Trade_final_KAZ$VALUE) # change separator decimal
  
  Trade_final_KAZ$VALUE <- as.numeric(Trade_final_KAZ$VALUE)

  
#           Russian Federation          -----------------------------------------------------------
#           Ukraine                     -----------------------------------------------------------



#   - Africa                         ------------------------------------------------------------------
#           Algeria                     -----------------------------------------------------------
#           Egypt                       -----------------------------------------------------------

## SEE laptop -> update R version to use package !

# write the pdf file in my folder
  # url.EGY <- "http://www.capmas.gov.eg/Admin/Pages%20Files/20171220101254trade_oct_2017.pdf"
  # filename.EGY = paste0(path_web,"/",Today," - Egypt - Exports - Imports.pdf")
  # download.file(url.EGY, filename.EGY, mode="wb")
  #
  # Trade_EGY_pdf <- pdf_text(filename.EGY)
  # cat(Trade_EGY_pdf[1])
  #
  # pdf_render_page(filename.EGY, page = 1, dpi = 72, numeric = TRUE)
  #

# load and write pdf == == == == == == == == == == == == == == == == == == == == == == == == ==

# write the pdf file in my folder
  url.EGY <- "http://www.capmas.gov.eg/Admin/Pages%20Files/20171220101254trade_oct_2017.pdf"
  filename.EGY = paste0(path_web,"/",Today," - Egypt - Exports - Imports.pdf")
  download.file(url.EGY, filename.EGY, mode="wb")


# pdftables == == == == == == == == == == == == == == == == == == == == == == == == ==

# install.packages('pdftables')
# library(pdftables)
# install.packages('curl')
# library(curl) # I need a version >= 3.5
# install.packages('RCurl')
# library(RCurl)

# The connection is not authorized from the WTO
# GO TO :
  #               https://pdftables.com/
  
  # and save the pdf in .csv format in the same directory
# create an account and convert your .pdf into .csv
# I will see with the helpdesk whether we can give the access to R... 

# convert pdf into csv - /!\ make sure the R version is a recent one (>=2017)
  # convert_pdf( input_file  = paste0(path_web,"/",Today,' - Egypt - Exports - Imports.pdf'),
  #              output_file = paste0(path_web,"/",Today,'  - Egypt - Exports - Imports.csv'),
  #              api_key     =  '8m9ga769f1sz') # create an api_key on this website (/!\ 50 free trials)

# import csv
  Trade_EGY_raw <- read.csv(paste0(path_web,"/",Today,' - Egypt - Exports - Imports.csv')) %>% group_by()


# Exchange rate == == == == == == == == == == == == == == == == == == == == == == == == ==

  # extract first table Exchange rate
  j <- min(which(Trade_EGY_raw[,1] == ''))
  while(j < 100){
    if(Trade_EGY_raw[j+1,1] == '') {
      j = j + 1
    }
    if(Trade_EGY_raw[j+1,1] != '') {
      line = j
      stop(paste0('Stop and cut at line ',j))
    }
  }

# select first table
  ER_EGY_select <- Trade_EGY_raw[1:(line-1),-c(ncol(Trade_EGY_raw))]

# create MONTH
  # Trade_EGY_select$MONTH <- c('January', 'February','March','April','May','June','July','August','September','October','November','December')
  ER_EGY_select$MONTH <- c('01', '02','03','04','05','06','07','08','09','10','11','12')

# make table in column
  ER_EGY__long <- ER_EGY_select %>% gather_('YEAR','VALUE',names(.)[substr(names(.),1,1) == 'X'])

# create new format date
  ER_EGY__long$YEAR  <-  as.character(substr(ER_EGY__long$YEAR,2,5))
  ER_EGY__long$VALUE <-    as.numeric(ER_EGY__long$VALUE)
  ER_EGY__long$DATE  <- paste0('01.',ER_EGY__long$MONTH,'.',ER_EGY__long$YEAR)
  ER_EGY__long$TIME <- as.Date(ER_EGY__long$DATE , format =  "%d.%m.%Y"  )

# Final time series Exchange rate
  ER_time_series <- ER_EGY__long %>% select(VALUE,TIME) %>% spread(TIME,VALUE)

# View(ER_time_series)



# Trade == == == == == == == == == == == == == == == == == == == == == == == == ==


# extract first table Exchange rate
  j2 <- line + 1   # begging of 2nd table
  while(j2 < 100){
    if(Trade_EGY_raw[j2+1,1] != '') {
      j2 = j2 + 1
    }
    if(Trade_EGY_raw[j2+1,1] == '') {
      cut = j2+1
      stop(paste0('Stop and cut at line ',j2+1))
    }
  }

  while(cut < 100){
    if(Trade_EGY_raw[cut+1,1] == '') {
      cut = cut + 1
    }
    if(Trade_EGY_raw[cut+1,1] != '') {
      cut = cut -1
      stop(paste0('Stop and cut at line ',cut-1))
    }
  }

# extract 2nd table
  Trade_EGY_latest <-  Trade_EGY_raw[(line+1):cut,]

# remove only empty columns
  Trade_EGY_latest_select <- Trade_EGY_latest[,apply(Trade_EGY_latest == rep('',nrow(Trade_EGY_latest)),2,function(x) sum(x)) != nrow(Trade_EGY_latest)]

# change name columns
  names(Trade_EGY_latest_select) <- c('col1','col2','col3','col4','col5','col6')

# remove the first row
  Trade_EGY_latest_select <- Trade_EGY_latest_select[-1,]

# select the first thwo columns    
  Trade_EGY_latest_select <- Trade_EGY_latest_select[,1:2]
  names(Trade_EGY_latest_select)[1:2] <- c('Import','Export')
  
# CREATE DATE  
  Trade_EGY_latest_select$MONTH <- c('01', '02','03','04','05','06','07','08','09','10','11','12')
  Trade_EGY_latest_select$YEAR  <-  max(ER_EGY__long$YEAR)
  Trade_EGY_latest_select$DATE  <- paste0('01.',Trade_EGY_latest_select$MONTH,'.',Trade_EGY_latest_select$YEAR)
  Trade_EGY_latest_select$TIME <- as.Date(Trade_EGY_latest_select$DATE , format =  "%d.%m.%Y"  )
  
# change format to numeric  
  Trade_EGY_latest_select$Import <- as.numeric(Trade_EGY_latest_select$Import)
  Trade_EGY_latest_select$Export <- as.numeric(Trade_EGY_latest_select$Export)
  
# Remove blanks if any
  Trade_EGY_latest_select$Import[Trade_EGY_latest_select$Import == ''] <- NA
  Trade_EGY_latest_select$Export[Trade_EGY_latest_select$Export == ''] <- NA
  
# pivot the table  
  Trade_final_EGY <- Trade_EGY_latest_select %>% select(Import,Export,TIME) %>% 
                        gather('FLOW','VALUE',names(.)[names(.)%in% c('Import','Export')]) %>% 
                        mutate(COUNTRY = 'Egypt', last_update = Today , PARTNER = 'WORLD' )

  
#           Morocco                     -----------------------------------------------------------
#           Tunisia                     -----------------------------------------------------------
#           South Africa                -----------------------------------------------------------
  # 

#   - Middle East                    -------------------------------------------------------------
#           Israel                      -----------------------------------------------------------

#   - Asia                           --------------------------------------------------------------------
#           Australia                   -----------------------------------------------------------
#           New Zealand                 -----------------------------------------------------------
#           China                       -----------------------------------------------------------
#           Hong Kong, China            -----------------------------------------------------------
# domestic exports
  # re-exports
#           India                       -----------------------------------------------------------
  
  # India:
  #    #   http://commerce.gov.in/InnerContent.aspx?Type=TradeStatisticsmenu&Id=254
  # http://commerce.gov.in/writereaddata/UploadedFile/MOC_636516369928343634_SDDS_Data_Dec_2017.pdf
# Information 
  # go to Merchandise Trade for SDDS
  # in pdf take "Latest data (2)"
  # update the latest month and same month of previous year 
  # all other data points are taken from the old Excel file
  
  
  # second step go to https://www.rbi.org.in/scripts/BS_ViewBulletin.aspx
  # go to # 31. take 5 latest , i.e. (t-1)+(t-2)+(t-3)+(t-4)+(t-5)
  
#           Indonesia                   -----------------------------------------------------------
#           Japan                       -----------------------------------------------------------
  
  # Japan:
  # http://www.customs.go.jp/toukei/shinbun/happyou_e.htm
  #  in new page -> latest month (second column (Excports + Imports)) 
  
  # http://www.customs.go.jp/toukei/suii/html/data/d41ma.csv
  # this is the rest of the series until the second last 
  # i.e. take 2006 -> second last month.
  
  
#           Korea, Republic of          -----------------------------------------------------------
#           Malaysia                    -----------------------------------------------------------
#           Philippines                 -----------------------------------------------------------
  

# our month is
mydatedf <- as.data.frame(Today)
Mymonth <- mydatedf %>% mutate(month = case_when( substr(Today,6,7) == '03' ~ "January"  ,
                                               substr(Today,6,7) == '04' ~  "February",
                                               substr(Today,6,7) == '05' ~ "March"  ,
                                               substr(Today,6,7) == '06' ~ "April"   ,  
                                               substr(Today,6,7) == '07' ~ "May"      , 
                                               substr(Today,6,7) == '08' ~ "June",
                                               substr(Today,6,7) == '09' ~ "July" , 
                                               substr(Today,6,7) == '10' ~ "August",   
                                               substr(Today,6,7) == '11' ~ "September",
                                               substr(Today,6,7) == '12' ~ "October",
                                               substr(Today,6,7) == '01' ~ "November",  
                                               substr(Today,6,7) == '02' ~ "December"))
# our year is 
Myyear <- as.numeric(substr(mydatedf$Today,1,4))

Mydate  <- Mymonth %>% mutate( year = ifelse(month %in% c("November",'December'), Myyear-1, Myyear  )) 


# load data 
# path name
  filename.PHI = paste0(path_web,"/",Today," - Philipines - PEIP_PR Statistical Tables.xls")

# create url  
  common_path <- 'https://psa.gov.ph/sites/default/files/attachments/itsd/trade/PEIP_PR%20Statistical%20Tables%20%28'
  url.PHI <- paste0(common_path,Mydate$month ,'%20',Mydate$year,'%29.xls') # right click on the link and copy the adress 
  
# test url 
  download.file(url = url.PHI, destfile = filename.PHI, mode="wb")  # load data from website and write in folder created

  
# if it doesnt work try other url (see below), otherwise take a month before
  
# variants names /!/ , i.e. not consistent over the different files !! 
         # 'https://psa.gov.ph/sites/default/files/attachments/itsd/trade/PEIP_PR%20Statistical%20Tables%20%28October%202017%29_0.xls'
         # 'https://psa.gov.ph/sites/default/files/attachments/itsd/trade/PEIP_PR%20Statistical%20Tables%20%28September%202017%29.pdf.xls'
         # 'https://psa.gov.ph/sites/default/files/attachments/itsd/trade/PEIP_PR%20Statistical%20Tables%20%28August%202017%29.xls'
         # 'https://psa.gov.ph/sites/default/files/attachments/itsd/trade/PEIP_PR%20Statistical%20Tables%20%28July%202017%29.xls'
  
# read data
  Trade_PHI_raw <- read_xls(path = filename.PHI ,sheet = 1, skip = 9, col_types =c('text',rep('numeric',8)))
  
  Trade_PHI_long <- Trade_PHI_raw %>% select(X__1,Imports, Exports) 
  
# Date format
  Trade_PHI_long <- Trade_PHI_long %>% mutate(YEAR = ifelse(substr(X__1,1,2) == '20',X__1, NA ))
  
  Trade_PHI_long <- Trade_PHI_long %>% mutate(MONTH = case_when(
                                              substr(X__1, 1,4) == 'Janu' ~ 1,
                                              substr(X__1, 1,4) == 'Febr' ~ 2,
                                              substr(X__1, 1,4) == 'Marc' ~ 3,
                                              substr(X__1, 1,4) == 'Apri' ~ 4,
                                              substr(X__1, 1,3) == 'May'  ~ 5,
                                              substr(X__1, 1,4) == 'June' ~ 6,
                                              substr(X__1, 1,4) == 'July' ~ 7,
                                              substr(X__1, 1,4) == 'Augu' ~ 8,
                                              substr(X__1, 1,4) == 'Sept' ~ 9,
                                              substr(X__1, 1,4) == 'Octo' ~ 10,
                                              substr(X__1, 1,4) == 'Nove' ~ 11,
                                              substr(X__1, 1,4) == 'Dece' ~ 12
                                            )) %>% filter(!is.na(X__1)) %>% filter(!is.na(YEAR) | !is.na(MONTH))
                      
# repeat months
  Trade_PHI_long$MONTH_REP <-   Trade_PHI_long$MONTH
  for( i in 1 : nrow(Trade_PHI_long)){
    Trade_PHI_long$MONTH_REP[i] <-   ifelse(!is.na(Trade_PHI_long$MONTH_REP[i]),Trade_PHI_long$MONTH_REP[i],Trade_PHI_long$MONTH_REP[i-1])
  }

# format date
  Trade_final_PHI <-  Trade_PHI_long  %>% filter(is.na(MONTH)) %>%
                          mutate(TIME = as.Date(paste0('01.',MONTH_REP,'.',YEAR), format =  "%d.%m.%Y")) %>% 
                          arrange(YEAR, MONTH_REP) %>%
                          select(-YEAR,-MONTH,-MONTH_REP,-X__1)  %>% 
                          gather_('FLOW','VALUE', names(.)[names(.) %in% c('Imports','Exports')] ) %>% 
                          mutate(COUNTRY = 'Philippines' , last_update = Today, PARTNER = 'WORLD')
  
#           Singapore                   -----------------------------------------------------------
#           Chinese Taipei              -----------------------------------------------------------
#           Thailand                    -----------------------------------------------------------
#           Viet Nam                    -----------------------------------------------------------
  

  
  
  
# Import old data       ---------------------------------------------
  
  loading.package('openxlsx')

df1 <- readWorkbook(xlsxFile = xlsxFile, sheet = 1, rows = c(6:103), cols = 7:212)
df1

# get cell format
  # wb <- loadWorkbook('N:/_Stat/Its/Statistical_Programmes/Short-term time series/Monthly_merchandise_trade_values/R queries/Imports.xlsx')
  # xlsxFile <- 'N:/_Stat/Its/Statistical_Programmes/Short-term time series/Monthly_merchandise_trade_values/R queries/Template_Imports.xlsx'
  # wb <- loadWorkbook(xlsxFile = xlsxFile)
  # 
  # sheets(wb)
  # # getTables(wb,sheet = 'Imports')
  # 
  # style <- getStyles(wb)
  # 
  # for(i in 1:95){
  #   if(style[[i]]$fill$fillBg == '64' & !is.null(style[[i]]$fill$fillBg)) {print(i)}
  # }
  # # Cell fill foreground:  rgb: #FFFF99 

# Merge all tables      --------------------------------------------------------


# Trade_final_US
# Trade_final_CAN
# Trade_final_EU
# Trade_final_KAZ
# Trade_final_PHI

rbind(Trade_final_CAN,Trade_final_EU,Trade_final_US) %>% spread(TIME,VALUE) %>% View()

rbind(Trade_final_KAZ,Trade_final_PHI) %>% spread(TIME,VALUE) %>% View()

# Append all the "final" tables   
  Trade_output <- mget( ls(pattern = "Trade_final_")) %>% bind_rows()

# Export data in Excel  ---------------------------------------------------

  
Trade_output_wide <- Trade_output %>% spread(TIME,VALUE)

# Vizualize results  
fix(Trade_output_wide)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
