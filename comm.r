# add committee co-memberships

load("data/net_no.rda")
raw = data.frame()

# find unique committees

stopifnot(s$uid %in% gsub("raw/rep|\\.html$", "", dir("raw", pattern = "rep\\w+\\.html$", full.names = TRUE)))
for(i in dir("raw", pattern = "rep\\w+\\.html$", full.names = TRUE)) {
  
  h = htmlParse(i, encoding = "UTF-8")
  r = xpathSApply(h, "//div[@id='ctl00_MainRegion_RepresentativeInfoContainer_BiographyContent_RepresentativeMemberships1_ctl01_GroupMembershipItem1_MembershipGroup']", xmlValue)
  r = str_clean(r)
  l = unlist(strsplit(r, "\\d{4}-\\d{2,4}"))
  l = l[ l!= "" ]
  y = unlist(str_extract_all(r, "\\d{4}-\\d{2,4}"))
  if(length(l) & !length(y)) # ministerial appointments, ignored later
    y = NA
  stopifnot(length(l) == length(y))

  if(length(l))
    raw = rbind(raw, data.frame(i, y, l, stringsAsFactors = FALSE))
  
}

raw = filter(raw, !is.na(y))
comm = data.frame()

for(i in 1:nrow(raw)) {
  
  l = raw$l[ i ]
  l = strsplit(l, ", \\d{2}\\.\\d{2}\\.\\d{4} - \\d{2}\\.\\d{2}\\.\\d{4}")
  l = str_clean(unlist(l))
  l = gsub("(.*), (.*)", "\\2", l)
  l = l[ grepl("komit", l) ]
  if(length(l))
    comm = rbind(comm, data.frame(i = raw$i[ i ], y = raw$y[ i ], l, stringsAsFactors = FALSE))
  
}

raw = filter(comm, substr(y, 1, 4) >= 1997) # to fit the network data
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
for(i in dir("raw", pattern = "rep\\w+\\.html$", full.names = TRUE))
  comm[, gsub("raw/rep|\\.html", "", i) ] = 0

for(i in 1:nrow(comm)) {
  
  l = gsub("raw/rep|\\.html", "", raw$i[ raw$u == comm$u[i] ])
  comm[ i, colnames(comm) %in% l ] = 1
  
}

comm$y = substr(comm$y, 1, 4)

for(i in ls(pattern = "^net_no")) {
  
  cat("Network:", i)
  
  n = get(i)
  sp = network.vertex.names(n)
  names(sp) = n %v% "url"
  
  m = filter(comm, y == gsub("\\D", "", i))
  m = m[ , names(m) %in% names(sp) ]
  cat(":", nrow(m), "committees", ncol(m), "MPs")
  
  m = t(as.matrix(m)) # sponsors in rows, committees in columns
  m = m %*% t(m) # adjacency matrix
  
  colnames(m) = sp[ colnames(m) ]
  rownames(m) = sp[ rownames(m) ]
  
  e = data.frame(i = n %e% "source", 
                 j = n %e% "target", 
                 stringsAsFactors = FALSE)
  e$committee = NA
  
  for(j in 1:nrow(e))
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
  assign(paste0("co", i), nn)
  
}

save(list = ls(pattern = "^((co)?net|edges|bills)_no\\d{4}$"),
     file = "data/net_no.rda")
