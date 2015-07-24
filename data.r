root = "https://www.stortinget.no"
sponsors = "data/representanter.csv"
bills = "data/bills.csv"

if(!file.exists(bills)) {

  # legislatures
  # http://data.stortinget.no/eksport/stortingsperioder
  # 2013-2017 (only 2013-2014 and 2014-2015, n = 833), 2009-2013, 2005-2009,
  # 2001-2005, 1997-2001, 1993-97, 1989-93, 1985-89 (missing 1985-86, n > 1000)

  # sessions
  h = htmlParse("http://data.stortinget.no/eksport/sesjoner")
  h = unique(xpathSApply(h, "//id", xmlValue))

  # scrape ~ 43 MB of open data (excluding empty current session)
  b = data.frame()
  for(i in h[ h != "2015-2016" ]) {

    file = paste0("raw/bills", i, ".xml")

    if(!file.exists(file))
      download.file(paste0("http://data.stortinget.no/eksport/saker?sesjonId=", i), file,
                    mode = "wb", quiet = TRUE)

    hh = htmlParse(file)
    hh = xpathSApply(hh, "//sak")

    cat(i, ":", length(hh), "bills\n")

    # uid
    id = sapply(hh, xpathSApply, "id", xmlValue)

    # session
    yr = sapply(hh, xpathSApply, "behandlet_sesjon_id", xmlValue)

    # title
    ti = sapply(hh, xpathSApply, "korttittel", xmlValue)

    # sponsor(s)
    au = lapply(hh, xpathSApply, "forslagstiller_liste/representant/id", xmlValue)
    au = sapply(au, paste0, collapse = ";")

    # keyword(s)
    kw = lapply(hh, xpathSApply, "emne_liste/emne/navn", xmlValue)
    kw = sapply(kw, paste0, collapse = ";")

    # committee (only one but some bills have none, so sapply() is required)
    cm = lapply(hh, xpathSApply, "komite/navn", xmlValue)
    cm = sapply(cm, paste0, collapse = ";")

    # committee supervisor(s)
    co = lapply(hh, xpathSApply, "saksordfoerer_liste/representant/id", xmlValue)
    co = sapply(co, paste0, collapse = ";")

    # status
    st = sapply(hh, xpathSApply, "status", xmlValue)

    # type
    ty = sapply(hh, xpathSApply, "type", xmlValue)

    b = rbind(b, data.frame(
      id, year = i, session = yr, title = ti, committee = cm, supervisors = co,
      keywords = kw, status = st, type = ty, authors = au,
      stringsAsFactors = FALSE
    ))

  }

  # # impute session from filename (tentative)
  # b$session[ b$session == "" ] = b$year[ b$session == "" ]

  # remove 2,678 duplicated bills that appear in multiple files
  b = unique(b[, -2 ])

  # author and committee supervision counts
  b$n_au = ifelse(b$authors == "", 0, 1 + str_count(b$authors, ";"))
  b$n_co = ifelse(b$supervisors == "", 0, 1 + str_count(b$supervisors, ";"))

  # session: 369 missing (~ 2.5% of total)
  # 198 have no author, 171 have authors, 136 have more than one author
  table(b$session, exclude = NULL)
  table(b$n_au[ b$session == "" ] > 0)
  table(b$n_au[ b$session == "" ] > 1)

  # losing ~ 1.5% of all bills; 14,204 rows left
  b = b[ b$session != "", ]

  # type: ~ 9,500 general, ~ 1,800 budget, ~ 2,800 legislative
  table(b$type, exclude = NULL)

  # status: all bills but 6 have been processed
  table(b$status, exclude = NULL)

  # keywords: 172 unique terms
  # 4,875 bills (34% of total), including many with authors, have no keywords
  table(ifelse(b$keywords == "", 0, 1 + str_count(b$keywords, ";")), exclude = NULL)
  table(unlist(strsplit(b$keywords, ";")))
  length(unique(unlist(strsplit(b$keywords, ";"))))

  # cosponsored bills
  table(b$n_au > 0, exclude = NULL) # n = 2,949 bills with 1+ author(s) (20%)
  table(b$n_au > 1, exclude = NULL) # n = 2,413 bills with 2+ authors (17%)
  nrow(b[ b$n_au > 1, ]) / nrow(b[ b$n_au > 0, ]) # ~ 80% cosponsored bills

  # legislatures
  b$legislature = NA
  b$legislature[ b$session %in% c("2013-2014", "2014-2015") ] = "2013-2017" # "2015-2016", "2016-2017"
  b$legislature[ b$session %in% c("2009-2010", "2010-2011", "2011-2012", "2012-2013") ] = "2009-2013"
  b$legislature[ b$session %in% c("2005-2006", "2006-2007", "2007-2008", "2008-2009") ] = "2005-2009"
  b$legislature[ b$session %in% c("2001-2002", "2002-2003", "2003-2004", "2004-2005") ] = "2001-2005"
  b$legislature[ b$session %in% c("1997-98", "1998-99", "1999-2000", "2000-2001") ] = "1997-2001"
  b$legislature[ b$session %in% c("1993-94", "1994-95", "1995-96", "1996-97") ] = "1993-1997"
  b$legislature[ b$session %in% c("1989-90", "1990-91", "1991-92", "1992-93") ] = "1989-1993"
  b$legislature[ b$session %in% c("1986-87", "1987-88", "1988-89") ] = "1985-1989" # missing "1985-86"
  table(b$legislature[ b$n_au > 0 ], exclude = NULL)
  table(b$legislature[ b$n_au > 1 ], exclude = NULL)

  # from ~ 40% to ~ 100% of authored bills are cosponsored
  table(b$legislature[ b$n_au > 1 ], exclude = NULL) /
    table(b$legislature[ b$n_au > 0 ], exclude = NULL)

  write.csv(b, bills, row.names = FALSE)

}

b = read.csv(bills, stringsAsFactors = FALSE)

# get sponsors

m = unique(unlist(strsplit(b$authors, ";")))

cat("Scraping", length(m), "unique sponsors\n")

s = data.frame()
for(k in rev(m)) {

  file = paste0("raw/rep", k, ".html")
  file = gsub("Å", "_A", file) # special case, encoding issue

  if(!file.exists(file)) {
    
    h = paste0(root, "/no/Representanter-og-komiteer/Representantene/",
               "Representantfordeling/Representant/", "?perid=",
               gsub("Æ", "%C3%86", gsub("Å", "%C3%85", gsub("Ø", "%C3%98", k))))
    h = GET(h)
    writeLines(content(h, "text"), paste("GET", file))
    
  }

  h = htmlParse(file)

  # panelized information
  nfo = xpathSApply(h, "//h3[text()=' Stortingsperioder']/following-sibling::div[1]", xmlValue)
  nfo = str_clean(unlist(strsplit(nfo, "\\.")))
  nfo = nfo[ nfo != "" ]
  county = gsub("(Varar|R)epresentant nr \\d+ for (.*),\\s?(.*),\\s?(.*)", "\\2", nfo)
  mandate = str_extract(nfo, "\\d{4} - \\d{4}")
  mandate = gsub("\\s", "", mandate)
  nyears = as.numeric(range(unlist(str_extract_all(mandate, "\\d{4}"))))
  nyears = paste0(seq(nyears[1], nyears[2]), collapse = ";")
  party = gsub("(.*),\\s?(.*),\\s?(.*)", "\\3", nfo)

  # panel-agnostic details
  name = gsub("Biografi: ", "", xpathSApply(h, "//title", xmlValue))
  born = xpathSApply(h, "//span[@id='ctl00_MainRegion_RepShortInfo_lblBirthDate']", xmlValue)
  photo = xpathSApply(h, "//img[@id='ctl00_MainRegion_RepShortInfo_imgRepresentative']/@src")
  sex = xpathSApply(h, "//div[@class='mainbody'][2]", xmlValue)
  sex = ifelse(length(sex), str_extract(sex, "Datter|Sønn"), NA)
  # type = xpathSApply(h, "//span[@id='ctl00_MainRegion_RepShortInfo_lblRepresentativeType']", xmlValue)

  cat("Sponsor", sprintf("%3.0f", which(m == k)), k, "\n")

  s = rbind(s, data.frame(uid = k, name, born, sex, photo,
                          mandate, party, county, nyears,
                          stringsAsFactors = FALSE))

}

# subset to covered legislatures (to avoid having to code old parties)
s = subset(s, as.numeric(substr(mandate, 1, 4)) >= 1985)

# use latest party id for 15 sponsors with multiple party mandates per legislature
s = mutate(group_by(s, name, mandate), np = 1:n()) %>% filter(np == max(np))

if(!file.exists(sponsors)) {

  dd = data.frame()

} else {

  dd = read.csv(sponsors)

}

# get gender from XML listing ("NA" is because the page is missing; MP is male)
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

s$born = as.numeric(substr(s$born, 1, 4))
s$name = gsub("(.*), (.*)", "\\2 \\1", s$name)

# party abbreviations
s$party[ s$party == "TF" ] = "KP" # TVF (predecessor to KyP)
s$party[ s$party == "Uav" ] = "IND" # Ind (Uavhengig)
s$party = toupper(s$party)

# party names
s$partyname = s$party
s$partyname[ s$party == "SV" ] = "Sosialistisk Venstreparti"
s$partyname[ s$party == "MDG" ] = "Miljøpartiet De Grønne" # n = 1
s$partyname[ s$party == "A" ] = "Arbeiderpartiet" # Ap
s$partyname[ s$party == "SP" ] = "Senterpartiet"
s$partyname[ s$party == "V" ] = "Venstre"
s$partyname[ s$party == "H" ] = "Høyre"
s$partyname[ s$party == "KRF" ] = "Kristelig Folkeparti"
s$partyname[ s$party == "FRP" ] = "Fremskrittspartiet"
s$partyname[ s$party %in% c("TF", "KP") ] = "Kystpartiet"
s$partyname[ s$party == "IND" ] = "Independent"
s$partyname[ s$party == "FFF" ] = "Framtid for Finnmark" # 1989, n = 1
s$partyname[ s$party == "RV" ] = "Rød Valgallianse" # 1993-1997, dissolved 2007; n = 1

# checked: all found in WP-EN
s$county = gsub("\\s", "_", s$county)

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

s = s[, c("uid", "name", "born", "sex", "party", "partyname", "nyears", "county", "mandate", "photo") ]
