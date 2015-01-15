# get bills

years = paste0(1998:2014, "-", 1999:2015)
a = data.frame()
pass = TRUE
root = "https://www.stortinget.no"
sponsors = "data/representanter.csv"

for(i in years) {
  
  cat("Scraping years", i, "...\n")
  file = paste0("raw/index", i, ".html")
  
  if(!file.exists(file))
    download(paste0("https://www.stortinget.no/no/Saker-og-publikasjoner/Publikasjoner/Representantforslag/?pid=", i),
             file, mode = "wb", quiet = TRUE)
  
  h = htmlParse(file)
  h = xpathSApply(h, "//a[contains(@href, '/dok')]/@href")
  
  # avoid known permanent errors
  h = h[ !grepl("dok8-(200809-020|200809-045|201112-012|201112-023|201112-050|201415-016|201415-017)", h) ]
  
  for(j in rev(h)) {
    
    file = gsub("(.*)dok(.*)/", "raw/doc\\2.html", j)
    
    if(!file.exists(file)) {
      
      download(paste0(root, j), file, mode = "wb", quiet = TRUE)
      hh = htmlParse(file)
      hh = xpathSApply(hh, "//div[@class='externallink']/a[contains(@href, '/Saker')]/@href")
      download(paste0(root, hh), file, mode = "wb", quiet = TRUE)
      
    }
    
    hh = htmlParse(file)
    if(grepl("Sak -", xpathSApply(hh, "//title", xmlValue))) {
      
      hh = data.frame(
        years = i,
        doc = gsub("doc|\\.html$", "", file),
        aut = paste0(xpathSApply(hh, "//a[contains(@href, 'Representant/')]", xmlValue), collapse = ";"),
        url = paste0(xpathSApply(hh, "//a[contains(@href, 'Representant/')]/@href"), collapse = ";"),
        kwd = paste0(xpathSApply(hh, "//a[contains(@href, 'stid')]", xmlValue), collapse = ";"),
        stringsAsFactors = FALSE)
      
      a = rbind(a, hh)
      
      # cat("Document", sprintf("%3.0f", which(h == j)), file, ":", 1 + str_count(hh$aut, ";"), "sponsors")
      
      hh = n_distinct(unlist(str_extract_all(hh$aut, "\\([a-zA-Z]+\\)")))
      # cat(ifelse(hh > 1, paste("", hh, "parties\n"), "\n"))
      
    } else {
      
      pass = FALSE
      hh = htmlParse(file)
      hh = xpathSApply(hh, "//div[@class='externallink']/a[contains(@href, '/Saker')]/@href")
      download(paste0(root, hh), file, mode = "wb", quiet = TRUE)
      
      cat("Document", sprintf("%3.0f", which(h == j)), file, "(additional run required) \n")
      
    }
    
  }
  
  cat("Scraped", nrow(a), "documents", ifelse(pass, "\n", "(additional run required)\n"))
  
}

# get sponsors

a$url = gsub("/no/Representanter-og-komiteer/Representantene/Representantfordeling/Representant/\\?perid=", "", a$url)

m = unique(unlist(strsplit(a$url, ";")))
s = data.frame()

cat("Found", nrow(a), "documents", sum(a$n_au > 1), "cosponsored", length(m), "unique sponsors\n")

for(k in rev(m)) {
  
  file = paste0("raw/rep", k, ".html")
  
  if(!file.exists(file))
    download(paste0(root,
                    "/no/Representanter-og-komiteer/Representantene/Representantfordeling/Representant/?perid=",
                    gsub("Æ", "%C3%86", gsub("Å", "%C3%85", gsub("Ø", "%C3%98", k)))),
             file, mode = "wb", quiet = TRUE)
  
  h = htmlParse(file)
  
  name = gsub("Biografi: ", "", xpathSApply(h, "//title", xmlValue))
  party = xpathSApply(h, "//span[@id='ctl00_MainRegion_RepShortInfo_lblParty']", xmlValue)
  type = xpathSApply(h, "//span[@id='ctl00_MainRegion_RepShortInfo_lblRepresentativeType']", xmlValue)
  county = xpathSApply(h, "//span[@id='ctl00_MainRegion_RepShortInfo_lblCounty']", xmlValue)
  mandate = xpathSApply(h, "//span[@id='ctl00_MainRegion_RepShortInfo_lblParliamentPeriod']", xmlValue)
  seniority = xpathSApply(h, "//span[@id='ctl00_MainRegion_RepShortInfo_lblSeniority']", xmlValue)
  born = xpathSApply(h, "//span[@id='ctl00_MainRegion_RepShortInfo_lblBirthDate']", xmlValue)
  photo = xpathSApply(h, "//img[@id='ctl00_MainRegion_RepShortInfo_imgRepresentative']/@src")
  sex = xpathSApply(h, "//div[@class='mainbody'][2]", xmlValue)
  sex = ifelse(length(sex), str_extract(sex, "Datter|Sønn"), NA)
  
  cat("Sponsor", sprintf("%3.0f", which(m == k)), k, name, "\n")
  
  s = rbind(s, data.frame(uid = gsub("^raw/rep|\\.html$", "", file),
                          name, party, type, county, mandate, seniority, born, sex, 
                          photo,
                          stringsAsFactors = FALSE))
  
}

if(!file.exists(sponsors)) {
  
  dd = data.frame()
  
} else {
  
  dd = read.csv(sponsors)
  
}

# get gender from XML listing
for(ii in rev(na.omit(m[ !grepl("_", m) & !m %in% dd$uid & m != "NA" ]))) {
  
  cat(which(ii == m), "Finding gender of MP", ii, "\n")
  hh = xmlToList(paste0("http://data.stortinget.no/eksport/person?personid=", ii))
  dd = rbind(dd, cbind(hh[ grepl("id", names(hh)) ],
                       hh[ grepl("kjoenn", names(unlist(hh))) ]))
  
}
dd = unique(dd)
names(dd) = c("uid", "sex2")
dd$uid = toupper(dd$uid)
dd$sex2 = as.character(dd$sex2)

write.csv(dd, sponsors, row.names = FALSE)

# prepare sponsors

s = merge(s, dd, by = "uid", all.x = TRUE)

s$sex[ s$sex == "Datter" | s$sex2 == "kvinne" ] = "F"
s$sex[ s$sex == "Sønn" | s$sex2 == "mann" ] = "M"

# if missing gender
# table(gsub("(.*), (.*)", "\\2", s$name[is.na(s$sex)]))

s$born = as.numeric(substr(s$born, 1, 4))
s$name = gsub("(.*), (.*)", "\\2 \\1", s$name)

s$party[ grepl("Kystpartiet", s$party) ] = "Kystpartiet"
s$party[ grepl("Uavhengig", s$party) ] = "Independent"

# official party codes
s$partyname = s$party
s$party[ s$partyname == "Sosialistisk Venstreparti" ] = "SV"
s$party[ s$partyname == "Miljøpartiet De Grønne" ] = "MDG"
s$party[ s$partyname == "Arbeiderpartiet" ] = "Ap"
s$party[ s$partyname == "Senterpartiet" ] = "Sp"
s$party[ s$partyname == "Venstre" ] = "V"
s$party[ s$partyname == "Høyre" ] = "H"
s$party[ s$partyname == "Kristelig Folkeparti" ] = "KrF"
s$party[ s$partyname == "Fremskrittspartiet" ] = "FrP"
s$party[ s$partyname == "Kystpartiet" ] = "KyP" # unofficial
s$party[ s$partyname == "Independent" ] = "Ind" # unofficial
s$party = toupper(s$party)

s$county = gsub(" for |\\s$", "", s$county)
s$county = gsub("\\s", "_", s$county)

s$nyears = as.numeric(gsub("(\\d+) år, (\\d+) dager", "\\1", s$seniority)) +
  as.numeric(as.numeric(gsub("(\\d+) år, (\\d+) dager", "\\2", s$seniority)) > 365 / 2)

# download photos (identical to unique profile identifier)
for(i in unique(s$photo)) {
  
  photo = gsub("/Personimages/PersonImages_Large/(.*)_stort(.*)", "photos/\\1\\2", i)
  photo = gsub("%c3", "_", gsub("%85", "A", gsub("%98", "O", photo)))
  
  if(!file.exists(photo))
    try(download(paste0(root, i), photo, mode = "wb", quiet = TRUE), silent = TRUE)
  
  if(!file.info(photo)$size | grepl("Default", photo)) {
    
    file.remove(photo)
    s$photo[ s$photo == i ] = 0
    
  } else {
    
    s$photo[ s$photo == i ] = 1
    
  }
  
}

s = s[, c("uid", "name", "born", "sex", "party", "partyname", "nyears", "type", "county", "mandate", "photo") ]

# prepare bills

a$n_au = 1 + str_count(a$url, ";")

a$legislature = "1997-2001"
a$legislature[ a$years %in% c("2001-2002", "2002-2003", "2003-2004", "2004-2005") ] = "2001-2005"
a$legislature[ a$years %in% c("2005-2006", "2006-2007", "2007-2008", "2008-2009") ] = "2005-2009"
a$legislature[ a$years %in% c("2009-2010", "2010-2011", "2011-2012", "2012-2013") ] = "2009-2013"
a$legislature[ a$years %in% c("2013-2014", "2014-2015", "2015-2016", "2016-2017") ] = "2013-2017"

a$kwd = gsub("Særavgifter", "Skatter", a$kwd)  # border and domestic taxation
a$kwd = gsub("Vegtrafikk", "Vegvesen", a$kwd) # roads and traffic
a$kwd = gsub(" og påtalemyndighet| og konkurranseforhold", "", a$kwd)
