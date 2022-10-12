# Request url and html from PIHPS
library(httr)
library(readxl)
library(readr)
library(rvest)
library(stringr)
library(rebus)
library(dplyr)

wd_primary = ""
wd_backup = ''
setwd(wd_backup)

# Function to scrap PIHPS
scrap_pihps = function(provinces_selected, kota_selected, market_selected, initial_date, end_date) {
  # 0 parameter in filter_market_ids[] return to all markets (average)
  post_data = POST("https://hargapangan.id/tabel-harga/pasar-tradisional/daerah",
                   query = list(task=NULL, "filter_commodity_ids[]" = 0,
                                "filter_regency_ids[]"=kota_selected, "filter_province_ids[]" = provinces_selected,
                                "filter_market_ids[]"=market_selected, "filter_all_commodities"=0,
                                "format"="xls", "price_type_id"=1, 
                                "661ac72163b437a5611f7ff4df98d46a"=1,
                                "filter_layout"="default", "filter_start_date"=initial_date,
                                "filter_end_date"=end_date),
                   write_disk("trial1.xls", overwrite=TRUE))
  
  data_xls = data.frame(read_excel("trial1.xls"))
  data1 = data_xls[8:nrow(data_xls),-1]
  TANGGAL = as.character(data1[1,])[-1]
  komoditas = (data1[,1])[-1]
  remove_komoditas = c("Beras", "Daging Ayam", "Daging Sapi", "Telur Ayam",
                       "Bawang Merah", "Bawang Putih", "Cabai Merah",
                       "Cabai Rawit", "Minyak Goreng", "Gula Pasir")
  komoditas = komoditas[! komoditas %in% remove_komoditas]
  pattern_kelompok = or("Beras", "Daging Ayam", "Daging Sapi", "Telur Ayam",
                        "Bawang Merah", "Bawang Putih", "Cabai Merah",
                        "Cabai Rawit", "Minyak Goreng", "Gula Pasir")
  
  TABEL_FINAL = data.frame()
  for (i in 1:length(komoditas)) {
    KELOMPOK = rep(str_extract(komoditas[i], pattern = pattern_kelompok), length(TANGGAL))
    JENIS = rep(komoditas[i], length(TANGGAL))
    indeks_jenis = which(data1[,1]==komoditas[i])
    HARGA = as.numeric(data1[indeks_jenis,2:ncol(data1)])
    KOTA = rep(as.character(data_xls[4,3]), length(TANGGAL))
    PASAR = rep(as.character(data_xls[5,3]), length(TANGGAL))
    PROVINSI = rep(as.character(data_xls[3,3]), length(TANGGAL))
    TABEL = cbind(TANGGAL, PROVINSI, KOTA, PASAR, KELOMPOK, JENIS, HARGA)
    TABEL_FINAL = rbind(TABEL_FINAL, TABEL)
  }
  return(TABEL_FINAL)
}

# request provinces ID
url_req_prov = GET("https://hargapangan.id/tabel-harga/pasar-modern/daerah")
konten_prov = content(url_req_prov)
select_elements = konten_prov %>%
  html_elements("select")
select_elements = select_elements[2]
provinces_id = select_elements %>%
  html_elements("option") %>%
  html_attr("value") %>%
  parse_integer()
provinces_id
provinces_selected = provinces_id[16:length(provinces_id)] # very important parameter

# Inputting parameters
provinces = provinces_selected
# city = kota_selected[2:length(kota_id)]
start_date = "01-06-2021"
final_date = "31-03-2022"

# Checking the input parameters
provinces
# city
start_date
final_date

# Running the web_scrapping function
total = length(provinces)
pb <- winProgressBar(title = "progress bar", min = 0,
                     max = total, width = 300)
sleep_time = 5 # delay in seconds
TABEL_SCRAP = data.frame()
for (prov_i in 1:total) {
  Sys.sleep(sleep_time)
  prov_input = provinces[prov_i]
  
  # request kota/kabupaten ID given provinces ID
  url_start = "https://hargapangan.id/?option=com_gtpihps&task=stats_province.loadRegencies&"
  url_custom = paste("filter_province_ids%5B%5D=", as.character(prov_input), sep="")
  url_end = "&price_type_id=1"
  url_request = paste(url_start, url_custom, url_end, sep="")
  url_req = GET(url_request)
  konten = content(url_req) # siap menggunakan rvest karena formatnya sudah html
  kota_id = konten %>%
    html_elements("option") %>%
    html_attr("value") %>%
    parse_integer()
  kota_id
  
  for (city_i in 1:length(kota_id)) {
    Sys.sleep(sleep_time)
    city_input = kota_id[city_i]
    
    # request market ID
    url_start_market = "https://hargapangan.id/?option=com_gtpihps&task=stats_province.loadMarkets&"
    url_custom_market = paste("filter_regency_ids%5B%5D=", as.character(city_input), sep="")
    url_end_market = "&price_type_id=1"
    url_request_market = paste(url_start_market, url_custom_market, url_end_market, sep="")
    url_req_market = GET(url_request_market)
    konten_market = content(url_req_market) # siap menggunakan rvest karena formatnya sudah html
    if (is.null(konten_market)==TRUE) {
      next
    }
    market_id = konten_market %>%
      html_elements("option") %>%
      html_attr("value") %>%
      parse_integer()
    
    for (m in market_id) {
      Sys.sleep(sleep_time)
      scrap_result = scrap_pihps(prov_input, city_input, m, start_date, final_date)
      TABEL_SCRAP = rbind(TABEL_SCRAP, scrap_result)
    }
  }
  setWinProgressBar(pb, prov_i, title=paste( round(prov_i/total*100, 0), "% done"))
}
close(pb)

TABEL_SCRAP_export = TABEL_SCRAP %>%
  filter(KELOMPOK=='Minyak Goreng')
write.csv(TABEL_SCRAP_export,"output scrap2.csv", row.names = FALSE)

