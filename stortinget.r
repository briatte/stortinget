library(downloader)
library(network)
library(XML)
library(stringr)
library(tnet)
library(GGally)
library(rgexf)

plot = FALSE
gexf = FALSE

colors = c(
  "Sosialistisk Venstreparti" = "#E41A1C", # Left party, red
  "Miljøpartiet De Grønne" = "#4DAF4A", # Miljöpartiet, green
  "Arbeiderpartiet" = "#F781BF", # Labour party, pink
  "Senterpartiet" = "#A65628", # Senterpartiet, agrarian, brown
  "Venstre" = "#FF7F00", # historical centrist, orange
  "Høyre" = "#80B1D3", # centre-right conservatives, light blue
  "Kristelig Folkeparti" = "#377EB8", # conservatives, blue
  "Fremskrittspartiet" = "#984EA3", # liberal conservative, purple
  "Kystpartiet" = "#444444", # non-partisan euroskeptic, dark grey
  "Independent" = "#AAAAAA" # unaffiliated, light grey
)
order = names(colors)

root = "https://www.stortinget.no"
years = paste0(1998:2013, "-", 1999:2014)
# years = gsub("index|\\.html$", "", dir("data", pattern = "index"))

# dossiers

a = data.frame()
pass = TRUE

for(i in years) {

  cat("Scraping years", i, "...\n")
  file = paste0("data/index", i, ".html")

  if(!file.exists(file))
    download(paste0("https://www.stortinget.no/no/Saker-og-publikasjoner/Publikasjoner/Representantforslag/?pid=", i),
             file, mode = "wb", quiet = TRUE)

  h = htmlParse(file)
  h = xpathSApply(h, "//a[contains(@href, '/dok')]/@href")
  
  for(j in rev(h)) {
    
    file = gsub("(.*)dok(.*)/", "data/doc\\2.html", j)
    
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

# sponsors

a$url = gsub("/no/Representanter-og-komiteer/Representantene/Representantfordeling/Representant/\\?perid=", "", a$url)

m = unique(unlist(strsplit(a$url, ";")))
s = data.frame()

for(k in rev(m)) {
  
  file = paste0("data/rep", k, ".html")
  
  if(!file.exists(file))
    download(paste0(root,
                    "/no/Representanter-og-komiteer/Representantene/Representantfordeling/Representant/?perid=", k),
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
  sex = str_extract(xpathSApply(h, "//div[@class='mainbody'][2]", xmlValue), "Datter|Sønn")
    
  cat("Sponsor", sprintf("%3.0f", which(m == k)), name, sex, "\n")

  s = rbind(s, data.frame(uid = gsub("^data/rep|\\.html$", "", file),
                          name, party, type, county, mandate, seniority, born, sex, 
                          photo,
                          stringsAsFactors = FALSE))
  
}

s$fullname = gsub("(.*), (.*)", "\\2 \\1", s$name)
s$party[ grepl("Kystpartiet", s$party) ] = "Kystpartiet"
s$party[ grepl("Uavhengig", s$party) ] = "Independent"
s$county = gsub(" for |\\s$", "", s$county)
s$nyears = as.numeric(gsub("(\\d+) år, (\\d+) dager", "\\1", s$seniority)) +
  as.numeric(as.numeric(gsub("(\\d+) år, (\\d+) dager", "\\2", s$seniority)) > 365 / 2)

# missing gender
table(gsub("(.*), (.*)", "\\2", s$name[is.na(s$sex)]))

a$n_au = 1 + str_count(a$url, ";")

a$legislature = 1997
a$legislature[ a$years %in% c("2001-2002", "2002-2003", "2003-2004", "2004-2005") ] = 2001
a$legislature[ a$years %in% c("2005-2006", "2006-2007", "2007-2008", "2008-2009") ] = 2005
a$legislature[ a$years %in% c("2009-2010", "2010-2011", "2011-2012", "2012-2013") ] = 2009
a$legislature[ a$years %in% c("2013-2014", "2014-2015", "2015-2016", "2016-2017") ] = 2013

a$kwd = gsub("Særavgifter", "Skatter", a$kwd)  # border and domestic taxation
a$kwd = gsub("Vegtrafikk", "Vegvesen", a$kwd) # roads and traffic
a$kwd = gsub(" og påtalemyndighet| og konkurranseforhold", "", a$kwd)

t = table(unlist(strsplit(a$kwd, ";")))
t = t[ t >= quantile(t, .9) ]

for(ii in unique(names(t))) {
  
  cat(ii)
  data = subset(a, grepl(ii, kwd) & n_au > 1)

  cat(":", nrow(data), "cosponsored documents, ")
  
  edges = lapply(unique(data$url), function(d) {
    
    d = unlist(strsplit(d, ";"))
    d = s$fullname[ s$uid %in% d ]
    d = expand.grid(d, d)
    d = subset(d, Var1 != Var2)
    d$uid = apply(d, 1, function(x) paste0(sort(x), collapse = "_"))
    d = unique(d$uid)
    if(length(d)) {
      d = data.frame(i = gsub("(.*)_(.*)", "\\1", d),
                     j = gsub("(.*)_(.*)", "\\2", d),
                     w = length(d))
      return(d)
    } else {
      return(data.frame())
    }
    
  })
  
  edges = rbind.fill(edges)
  edges$uid = apply(edges, 1, function(x) paste0(sort(x[ 1:2 ]), collapse = "_"))
  
  # raw edge counts
  count = table(edges$uid)
  
  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ uid, function(x) sum(1 / x), data = edges)
  
  # raw counts
  edges$count = as.vector(count[ edges$uid ])
  
  edges = data.frame(i = gsub("(.*)_(.*)", "\\1", edges$uid),
                     j = gsub("(.*)_(.*)", "\\2", edges$uid),
                     w = edges$w, n = edges[, 3])
  
  cat(nrow(edges), "edges, ")
  
  # network
  
  n = network(edges[, 1:2 ], directed = FALSE)
  n %n% "title" = paste("Stortinget", paste0(range(unique(data$years)), collapse = " to "))
  n %n% "n_bills" = nrow(data)
  
  cat(network.size(n), "nodes")
  
  rownames(s) = s$fullname
  n %v% "uid" = s[ network.vertex.names(n), "uid" ]
  n %v% "name" = s[ network.vertex.names(n), "name" ]
  n %v% "fullname" = s[ network.vertex.names(n), "fullname" ]
  n %v% "party" = s[ network.vertex.names(n), "party" ]
  n %v% "type" = s[ network.vertex.names(n), "type" ]
  n %v% "county" = s[ network.vertex.names(n), "county" ]
  n %v% "born" = s[ network.vertex.names(n), "born" ]
  n %v% "mandate" = s[ network.vertex.names(n), "mandate" ]
  n %v% "seniority" = s[ network.vertex.names(n), "seniority" ]
  n %v% "photo" = s[ network.vertex.names(n), "photo" ]
  
  network::set.edge.attribute(n, "source", as.character(edges[, 1]))
  network::set.edge.attribute(n, "target", as.character(edges[, 2]))
  
  network::set.edge.attribute(n, "weight", edges[, 3])
  network::set.edge.attribute(n, "count", edges[, 4])
  network::set.edge.attribute(n, "alpha",
                              as.numeric(cut(n %e% "count", c(1:4, Inf),
                                             include.lowest = TRUE)) / 5)
  
  # modularity
  
  nn = graph.edgelist(as.matrix(edges[, 1:2 ]), directed = FALSE)
  E(nn)$weight = edges[, 3]
  
  i = s[ V(nn)$name, "party" ]
  i[ i %in% c("Independent") ] = NA # unaffiliateds
  
  nn = nn - which(is.na(i))
  i = as.numeric(factor(i[ !is.na(i) ]))
  
  n %n% "modularity" = modularity(nn, membership = i, weights = E(nn)$weight)
  
  walktrap = lapply(1:50, function(x) walktrap.community(nn, steps = x))
  
  # max. partition
  maxwalks = order(sapply(walktrap, modularity), decreasing = TRUE)[1]
  walktrap = walktrap[[ maxwalks ]]
  
  n %n% "modularity_walktrap" = modularity(walktrap)
  
  louvain = multilevel.community(nn)
  
  n %n% "modularity_louvain" = modularity(louvain)
  
  # weighted adjacency matrix to tnet
  tnet = as.tnet(as.sociomatrix(n, attrname = "weight"), type = "weighted one-mode tnet")
  
  # weighted degree and distance
  wdeg = as.data.frame(degree_w(tnet, measure = "degree"))
  dist = distance_w(tnet)
  wdeg$distance = NA
  wdeg[ attr(dist, "nodes"), ]$distance = colMeans(dist, na.rm = TRUE)
  wdeg = cbind(wdeg, clustering_local_w(tnet)[, 2])
  names(wdeg) = c("node", "degree", "distance", "clustering")
  
  n %v% "degree" = wdeg$degree
  n %n% "degree" = mean(wdeg$degree, na.rm = TRUE)
  
  n %v% "distance" = wdeg$distance
  n %n% "distance" = mean(wdeg$distance, na.rm = TRUE)
  
  n %v% "clustering" = wdeg$clustering    # local
  n %n% "clustering" = clustering_w(tnet) # global
  
  party = n %v% "party"
  names(party) = network.vertex.names(n)
  
  i = colors[ s[ n %e% "source", "party" ] ]
  j = colors[ s[ n %e% "target", "party" ] ]
  
  party = as.vector(i)
  party[ i != j ] = "#AAAAAA"
  
  print(table(n %v% "party", exclude = NULL))
  
  if(plot) {
    
    q = unique(quantile(n %v% "degree"))
    n %v% "size" = as.numeric(cut(n %v% "degree", q, include.lowest = TRUE))
    g = suppressWarnings(ggnet(n, size = 0, segment.alpha = 1/2, # mode = "kamadakawai",
                               segment.color = party) +
                           geom_point(alpha = 1/3, aes(size = n %v% "size", color = n %v% "party")) +
                           geom_point(alpha = 1/2, aes(size = min(n %v% "size"), color = n %v% "party")) +
                           scale_size_continuous(range = c(6, 12)) +
                           scale_color_manual("", values = colors, breaks = order) +
                           theme(legend.key.size = unit(1, "cm"),
                                 legend.text = element_text(size = 16)) +
                           guides(size = FALSE, color = guide_legend(override.aes = list(alpha = 1/3, size = 6))))
    
    ggsave(paste0("net_", gsub("\\s", "_", ii), "_", nrow(data), ".pdf"), g, width = 12, height = 9)
    
  }

  assign(paste0("net_", ii), n)
  
  # gexf
  if(gexf) {
    
    rgb = t(col2rgb(colors[ names(colors) %in% as.character(n %v% "party") ]))
    mode = "fruchtermanreingold"
    meta = list(creator = "rgexf",
                description = paste0(mode, " placement"),
                keywords = "Parliament, Sweden")
    
    people = data.frame(url = n %v% "uid",
                        name = network.vertex.names(n), 
                        party = n %v% "party",
                        county = n %v% "county",
                        born = n %v% "born",
                        degree = n %v% "degree",
                        distance = n %v% "distance",
                        photo = n %v% "photo",
                        stringsAsFactors = FALSE)
    
    node.att = c("url", "party", "county", "born", "degree", "distance", "photo")
    node.att = cbind(label = people$name, people[, node.att ])
    
    people = data.frame(id = as.numeric(factor(people$name)),
                        label = people$name,
                        stringsAsFactors = FALSE)
    
    relations = data.frame(
      source = as.numeric(factor(n %e% "source", levels = levels(factor(people$label)))),
      target = as.numeric(factor(n %e% "target", levels = levels(factor(people$label)))),
      weight = n %e% "weight"
    )
    relations = na.omit(relations)
    
    nodecolors = lapply(node.att$party, function(x)
      data.frame(r = rgb[x, 1], g = rgb[x, 2], b = rgb[x, 3], a = .3 ))
    nodecolors = as.matrix(rbind.fill(nodecolors))
    
    net = as.matrix.network.adjacency(n)
    
    # placement method (Kamada-Kawai best at separating at reasonable distances)
    position = paste0("gplot.layout.", mode)
    if(!exists(position)) stop("Unsupported placement method '", position, "'")
    
    position = do.call(position, list(net, NULL))
    position = as.matrix(cbind(position, 1))
    colnames(position) = c("x", "y", "z")
    
    # compress floats
    position[, "x"] = round(position[, "x"], 2)
    position[, "y"] = round(position[, "y"], 2)
    
    write.gexf(nodes = people,
               edges = relations[, -3],
               edgesWeight = relations[, 3],
               nodesAtt = data.frame(label = as.character(node.att$label),
                                     url = node.att$url,
                                     party = node.att$party,
                                     county = node.att$county,
                                     born = node.att$born,
                                     degree = node.att$degree,
                                     distance = node.att$distance,
                                     photo = node.att$photo,
                                     stringsAsFactors = FALSE),
               nodesVizAtt = list(position = position,
                                  color = nodecolors,
                                  size = round(node.att$degree)),
               # edgesVizAtt = list(size = relations[, 3]),
               defaultedgetype = "undirected", meta = meta,
               output = paste0("net_", gsub("\\s", "_", ii), ".gexf"))
    
  }
  
}

save(list = ls(pattern = "net_"), file = "stortinget.rda")

m = data.frame(id = ls(pattern = "net_"),
               d = sapply(ls(pattern = "net_"), function(x) network.density(get(x))),
               n = sapply(ls(pattern = "net_"), function(x) get.network.attribute(get(x), "n_bills")),
               m = sapply(ls(pattern = "net_"), function(x) get.network.attribute(get(x), "modularity")),
               w = sapply(ls(pattern = "net_"), function(x) get.network.attribute(get(x), "modularity_walktrap")),
               l = sapply(ls(pattern = "net_"), function(x) get.network.attribute(get(x), "modularity_louvain"))
)
m$r = m$m / apply(m[, c("w", "l") ], 1, max)

g = qplot(data = m, y = n, label = gsub("net_", "", id), x = r, size = d, geom = "text") +
  labs(y = "Number of bills\n", x = "\nEmpirical / Maximized Modularity") +
  guides(size = FALSE) +
  scale_size_continuous(range = c(4, 6)) +
  theme_linedraw(16)

ggsave("modularity.png", g, width = 9, height = 9)

# have a nice day
