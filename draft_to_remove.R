
# write the pdf file in my folder
url.EGY <- "http://www.capmas.gov.eg/Admin/Pages%20Files/20171220101254trade_oct_2017.pdf"
filename.EGY = paste0(path_web,"/",Today," - Egypt - Exports - Imports.pdf")
download.file(url.EGY, filename.EGY, mode="wb")


# other     ------------------------------------------------------------------



install.packages('tm')
library(tm)

library(devtools)
slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
install_url(slam_url)



install.packages('slam')
library(slam)


uri <- sprintf("file://%s",
               system.file(file.path("doc", "tm.pdf"), package = "tm"))
engine <- if(nzchar(system.file(package = "pdftools"))) {
  "pdftools"
} else {
  "ghostscript"
}
reader <- readPDF(engine)



# tabulizer ---------------------------------------------------------------


install.packages('tabulizer')
library(tabulizer)

install.packages('ghit')
library(ghit)

ghit::install_github("ropensci/tabulizer")
devtools::install_github("ropensci/tabulizer")
ghit::install_github("ropensci/tabulizer", verbose = TRUE)

install.packages("tabulizer-master")


install.packages("rJava")

library("ghit")
ghit::install_github(c("ropensci/tabulizerjars","ropensci/tabulizer"))

install.packages('ghit')
library(ghit)
ghit::install_github(c("leeper/tabulizerjars", "leeper/tabulizer"), INSTALL_opts = "--no-multiarch")
ghit::install_github(c("leeper/tabulizerjars", "leeper/tabulizer"), INSTALL_opts = "--no-multiarch", dependencies = c("Depends", "Imports"))


install.packages('pdftables')
library(pdftables)



install.packages('pdftools')
library(pdftools)
download.file("http://arxiv.org/pdf/1403.2805.pdf", "1403.2805.pdf", mode = "wb")
txt <- pdf_text("1403.2805.pdf")



install.packages('devtools')
library(devtools)

slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
install_url(slam_url)



# load data automatically from website if load_web is set to TRUE
tmp = paste0(getwd(),'/test.gz')

  url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"

  download.file(url = url, destfile = tmp, mode="wb")  # load data from website and write in folder created
  unzip(tmp,exdir = path_web )                         # unzip file

  read.table(file=tmp, header=FALSE)  # read file


  
  
  
  
# WORD      -------------------------------------------------------------------


  
  # try to read word .doc
  loading.package('docxtractr')
  docx_describe_tbls(filename.KAZ.docx)
  complx <- read_docx(filename.KAZ.docx)
  
  
  
  
  
  
# XML  --------------------------------------------------------------------
 
# xml2  
  
  xml_link <- 'http://www.customs.go.jp/toukei/shinbun/trade-st_e/2017/201711ee'
  
  loading.package('xml2')
  Japan_data <- read_xml(xml_link)
  
# xML   
  
  # Load the packages required to read XML files.
  loading.package(c("XML","methods"))
  
  # Convert the input xml file to a data frame.
  xmldataframe <- xmlToDataFrame(xml_link)
  print(xmldataframe)
  
  
  install.packages("XML")
  library(XML)
  
  
  # Use the xmlTreePares-function to parse xml file directly from the web
  xmlfile <- xmlTreeParse(xml_link)
  class(xmlfile)
  
  
  loading.package('RCurl')
  opts <- list(
    proxy         = "999.999.999.999",                # how do I get my proxy ?
    proxyusername = "cwr.wto.org\\wettstein", 
    proxypassword = "Manager1290", 
    proxyport     = 8080
  )
  getURL("http://stackoverflow.com")
  
  
  
  # other tests ------------------
  
  library(xml2)
  loading.package('rvest')
  
  URL <- "http://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r"
  
  pg <- read_html(URL)
  
  html("http://google.com")
  
  head(html_attr(html_nodes(pg, "a"), "href"))
  
  url = "http://www.bcr.gob.sv/bcrsite/?cdr=38"
  download.file(url, destfile = "test.html", quiet=TRUE)
  content <- read_html("test.html")
  
  
  
  
# xml indian  -------------------------------------------------------------

  url <- "https://www.zomato.com/ncr/restaurants/north-indian"
  
  library(RCurl)
  library(XML)
  
  content <- getURL(url)
  doc <- htmlParse(content)
  
  summary(doc)
  
  
  
  
  
#  httr  ------------------------------------------------------------------


  
  loading.package('httr')
  r <- GET("http://httpbin.org/get")
  
  
  
  
  
  
#   -----------------------------------------------------------------------

  loading.package('rvest')
  loading.package('xml2')
  lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")
  
  
  
  
#  url  -------------------------------------------------------------------

  baseURL <- 'http://www.wunderground.com/history/airport/KMDW'
  suffixURL <- 'DailyHistory.html?HideSpecis=1&format=1'
  Date <- Sys.Date()
  datestring <- format(Date, '%Y/%m/%d')
  url2fetch <- paste(baseURL, datestring, suffixURL, sep='/')
  url_content <- getURL(url2fetch)
  url_content <- gsub('"', '', url_content)
  url_content <- gsub(' ', '', url_content)
  
  weather_data <- read.csv(textConnection(url_content))
  head(weather_data)
  
  
  x = readLines('http://www.bcr.gob.sv/bcrsite/?cdr=38')
  pos = grep('javascript:entrar_i',x)
  x[pos]
  
  base_url<-"http://www.bcr.gob.sv/bcrsite/?cdr=38"
  base_html<-getURLContent(base_url)[[1]]
  links<-strsplit(base_html,"a href=")[[1]]

  
  get_data_url<-function(s) {
      u_split1<-strsplit(s,"/")[[1]][1]
      u_split2<-strsplit(u_split1,'\\"')[[1]][2]
      ifelse(grep("[[:upper:]]",u_split2)==1 & length(strsplit(u_split2,"#")[[1]])<2,return(u_split2),return(NA))
  }
  
  # Extract only those element that are relevant
  genomes<-unlist(lapply(links,get_data_url))
  genomes<-genomes[which(is.na(genomes)==FALSE)]
  
  ### 2) Now, scrape the genome data from all of those URLS ###
  
  # This requires two complementary functions that are designed specifically
  # for the UCSC website. The first parses the data from a -structs.html page
  # and the second collects that data in to a multi-dimensional list
  parse_genomes<-function(g) {
      g_split1<-strsplit(g,"\n")[[1]]
      g_split1<-g_split1[2:5]
      # Pull all of the data and stick it in a list
      g_split2<-strsplit(g_split1[1],"\t")[[1]]
      ID<-g_split2[1]                             # Sequence ID
      LEN<-strsplit(g_split2[2],": ")[[1]][2]     # Length
      g_split3<-strsplit(g_split1[2],"\t")[[1]]
      TYPE<-strsplit(g_split3[1],": ")[[1]][2]    # Type
      AC<-strsplit(g_split3[2],": ")[[1]][2]      # Anticodon
      SEQ<-strsplit(g_split1[3],": ")[[1]][2]     # ID
      STR<-strsplit(g_split1[4],": ")[[1]][2]     # String
      return(c(ID,LEN,TYPE,AC,SEQ,STR))
  }
  
  # This will be a high dimensional list with all of the data, you can then manipulate as you like
  get_structs<-function(u) {
      struct_url<-paste(base_url,u,"/",u,"-structs.html",sep="")
      raw_data<-getURLContent(struct_url)
      s_split1<-strsplit(raw_data,"<PRE>")[[1]]
      all_data<-s_split1[seq(3,length(s_split1))]
      data_list<-lapply(all_data,parse_genomes)
      for (d in 1:length(data_list)) {data_list[[d]]<-append(data_list[[d]],u)}
      return(data_list)
  }
  
  # Collect data, manipulate, and create data frame (with slight cleaning)
  genomes_list<-lapply(genomes[1:2],get_structs) # Limit to the first two genomes (Bdist & Spurp), a full scrape will take a LONG time
  genomes_rows<-unlist(genomes_list,recursive=FALSE) # The recursive=FALSE saves a lot of work, now we can just do a straigh forward manipulation
  genome_data<-t(sapply(genomes_rows,rbind))
  colnames(genome_data)<-c("ID","LEN","TYPE","AC","SEQ","STR","NAME")
  genome_data<-as.data.frame(genome_data)
  genome_data<-subset(genome_data,ID!="</PRE>")   # Some malformed web pages produce bad rows, but we can remove them
  
  head(genome_data)
  
# rvest  ------------------------------------------------------------------

  library(rvest)
  
  # Store web url
  lego_movie <- html("http://www.imdb.com/title/tt1490017/")
  
  #Scrape the website for the movie rating
  rating <- lego_movie %>% 
      html_nodes("strong span") %>%
      html_text() %>%
      as.numeric()
  rating
 
  # Scrape the website for the cast
  cast <- lego_movie %>%
      html_nodes("#titleCast .itemprop span") %>%
      html_text()
  cast
 
  #Scrape the website for the url of the movie poster
  poster <- lego_movie %>%
      html_nodes("#img_primary img") %>%
      html_attr("src")
  poster
  
  # Extract the first review
  review <- lego_movie %>%
      html_nodes("#titleUserReviewsTeaser p") %>%
      html_text()
  review
  
  
  
  
  
  
  
  
  
  
  
  
#  new  -------------------------------------------------------------------


  
  
  
      
      library(rvest)
  
  URL <- "http://www.fantasypros.com/nfl/projections/qb.php?max-yes=true&min-yes=true"
  pg <- read_html(URL)
  
  html_attr(html_nodes(pg, "a#export-xls"), "href")
  
  dl_query <- html_attr(html_nodes(pg, "a#export-xls"), "href")
  
  download.file(sprintf("http://www.fantasypros.com/nfl/projections/qb.php%s", dl_query),
  "filenametosaveitas.csv")
      
      dat <- read.table("filenametosaveitas.csv", skip=4, header=TRUE, sep="\t")
  
  
      dl_query <- html_attr(html_nodes(pg, "a#export-xls"), "href")
      dl_query <- 'sadfasdf'
  
      sprintf("http://www.fantasypros.com/nfl/projections/qb.php%s", dl_query)
  
  
  
  
  
  
  
  
  
  
  
  
  
  