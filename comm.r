# add committee co-memberships

load("data/net_no.rda")
raw = data.frame()

# find unique committees

# remove check below due to inconsistencies between UIDs and URL identifiers (special characters)
# stopifnot(s$uid %in% gsub("raw/mps/mp-|\\.html$", "",
#                           list.files("raw/mps", full.names = TRUE)))

for (i in list.files("raw/mps", full.names = TRUE)) {

  h = htmlParse(i, encoding = "UTF-8")
  r = xpathSApply(h, "//div[@id='ctl00_ctl00_MainRegion_MainRegion_RepresentativeInfoContainer_BiographyContent_RepresentativeMemberships1_ctl05_GroupMembershipItem1_MembershipGroup']", xmlValue)
  r = str_clean(r)
  l = unlist(strsplit(r, "\\d{4}-\\d{2,4}"))
  l = l[ l!= "" ]
  y = unlist(str_extract_all(r, "\\d{4}-\\d{2,4}"))
  if (length(l) & !length(y)) # ministerial appointments, ignored later
    y = NA
  stopifnot(length(l) == length(y))

  if (length(l))
    raw = rbind(raw, data_frame(i, y, l))

}


raw = filter(raw, !is.na(y))
comm = data.frame()

for (i in 1:nrow(raw)) {

  l = raw$l[ i ]
  l = strsplit(l, ", \\d{2}\\.\\d{2}\\.\\d{4} - \\d{2}\\.\\d{2}\\.\\d{4}")
  l = str_clean(unlist(l))
  l = gsub("(.*), (.*)", "\\2", l)
  l = l[ grepl("komit", l) ]
  if (length(l))
    comm = rbind(comm, data.frame(i = raw$i[ i ], y = raw$y[ i ], l, stringsAsFactors = FALSE))

}

# new biography format for 2013-2017
# str_clean(xpathSApply(h, "//div[contains(@class, 'biography-affiliation')]//a[contains(@href, 'Komiteene')]/../following-sibling::p", xmlValue))
for (i in list.files("raw/mps", full.names = TRUE)) {
  
  h = htmlParse(i, encoding = "UTF-8")
  
  j = xpathSApply(h, "//div[contains(@class, 'biography-affiliation')]//a[contains(@href, 'Komiteene')]", 
                  xmlValue)
  
  if (length(j))
    comm = rbind(comm, data.frame(i, y = "2013-2017", l = j, stringsAsFactors = FALSE))

}

raw = filter(comm, substr(y, 1, 4) >= 1985) # to fit the network data
raw$u = paste(raw$y, raw$l)

# save flat list
write.csv(raw[, c("y", "l") ] %>%
            group_by(y, l) %>%
            mutate(n = n()) %>%
            arrange(y, l) %>%
            unique, "data/committees.csv", row.names = FALSE)

comm = unique(raw[, c("y", "l") ])
comm$u = paste(comm$y, comm$l)

# add sponsor columns
for (i in list.files("raw/mps", full.names = TRUE))
  comm[, gsub("raw/mps/mp-|\\.html", "", i) ] = 0

for (i in 1:nrow(comm)) {

  l = gsub("raw/mps/mp-|\\.html", "", raw$i[ raw$u == comm$u[i] ])
  comm[ i, colnames(comm) %in% l ] = 1

}

# solve encoding issue with filenames
colnames(comm) = gsub("_A", "Å", colnames(comm))

comm$y = substr(comm$y, 1, 4)

for (i in ls(pattern = "^net_no")) {

  cat("Network:", i)

  n = get(i)
  sp = network.vertex.names(n)
  names(sp) = gsub("https://www.stortinget.no/no/Representanter-og-komiteer/Representantene/Representantfordeling/Representant/\\?perid=", "", n %v% "url")
  
  stopifnot(names(sp) %in% colnames(comm))
  
  m = filter(comm, y == gsub("\\D", "", i))
  m = m[ , names(m) %in% names(sp) ]
  cat(":", nrow(m), "committees", ncol(m), "MPs")
  M = m

  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix

  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]

  e = data_frame(i = n %e% "source", j = n %e% "target")
  e$committee = NA

  for (j in 1:nrow(e))
    e$committee[ j ] = m[ e$i[ j ], e$j[ j ] ]

  cat(" co-memberships:",
      str_pad(paste0(range(e$committee), collapse = "-"), 6, "right"),
      sum(e$committee == 0), "null,",
      sum(e$committee == 1), "single,",
      sum(e$committee > 1), "> 1\n")

  nn = network(e[, 1:2], directed = FALSE)
  nn %e% "committee" = e$committee

  print(table(nn %e% "committee", exclude = NULL))
  stopifnot(network.size(n) == network.size(nn))

  n %e% "committee" = e$committee
  assign(i, n)

  nn %n% "committees" = as.table(rowSums(M))
  assign(paste0("co", i), nn)

}

save(list = ls(pattern = "^((co)?net|edges|bills)_no\\d{4}$"),
     file = "data/net_no.rda")
