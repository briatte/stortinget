meta = c("Norway", "Storting")
mode = "fruchtermanreingold"

# splits (legislatures and themes)

t = table(unlist(strsplit(a$kwd, ";")))
t = t[ t >= quantile(t, .9) ]
t = c(unique(a$legislature), names(t))

for(ii in unique(t)) {
  
  cat("\n", ii)
  if(grepl("\\d{4}", ii))
    data = subset(a, legislature == ii & n_au > 1)
  else
    data = subset(a, grepl(ii, kwd) & n_au > 1)
  
  cat(":", nrow(data), "cosponsored documents, ")
  
  # check for missing sponsors
  u = unlist(strsplit(data$url, ";"))
  u = na.omit(u[ !u %in% s$uid ])
  if(length(u)) {
    cat("Missing", length(u), "sponsors:\n")
    print(table(u))
  }
  
  #
  # directed edge list
  #
  
  edges = rbind_all(lapply(data$url, function(d) {
    
    w = unlist(strsplit(d, ";"))
    
    d = expand.grid(i = s$name[ s$uid %in% w ],
                    j = s$name[ s$uid == w[1]], stringsAsFactors = FALSE)

    return(data.frame(d, w = length(w) - 1)) # number of cosponsors
    
  }))

  #
  # edge weights
  #
  
  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)
  
  # count number of bills per first author
  n_au = table(self$j)
  
  # remove self-loops from directed edge list
  edges = subset(edges, i != j)
  
  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)
  
  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")
  
  # raw edge counts
  raw = table(edges$ij)
  
  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)
  
  # expand to edge list
  edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w, stringsAsFactors = FALSE)
  
  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w
  
  # final edge set: cosponsor, first author, weights
  edges = edges[, c("i", "j", "raw", "nfw", "gsw") ]
  
  cat(nrow(edges), "edges, ")

  #
  # directed network
  #
  
  n = network(edges[, 1:2 ], directed = TRUE)
  
  n %n% "country" = meta[1]
  n %n% "title" = paste(meta[2], paste0(range(unique(substr(data$years, 1, 4))),
                                        collapse = " to "))
  
  n %n% "n_bills" = nrow(data)
  
  if(grepl("\\d{4}", ii))
    n %n% "n_sponsors" = table(subset(a, legislature == ii)$n_au)
  else
    n %n% "n_sponsors" = table(subset(a, grepl(ii, kwd))$n_au)
  
  n_au = as.vector(n_au[ network.vertex.names(n) ])
  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")
  
  rownames(s) = s$name
  n %v% "url" = as.character(s[ network.vertex.names(n), "uid" ])
  n %v% "sex" = as.character(s[ network.vertex.names(n), "sex" ])
  n %v% "born" = as.numeric(substr(s[ network.vertex.names(n), "born" ], 1, 4))
  n %v% "party" = as.character(s[ network.vertex.names(n), "party" ])
  n %v% "partyname" = as.character(s[ network.vertex.names(n), "partyname" ])
  n %v% "lr" = as.numeric(scores[ s[ network.vertex.names(n), "party" ] ])
  
  if(grepl("\\d{4}", ii)) {
    n %v% "nyears" = as.numeric(substr(s[ network.vertex.names(n), "mandate" ], 6, 9)) # current mandate end
    n %v% "nyears" = n %v% "nyears" - as.numeric(substr(ii, 6, 9)) # penalty if current mandate > legislature
    n %v% "nyears" = as.numeric(s[ network.vertex.names(n), "nyears" ] - n %v% "nyears") # seniority
    n %v% "nyears"= ifelse(n %v% "nyears" > 0, n %v% "nyears", 0) # avoid negative values
  }

  n %v% "constituency" = as.character(s[ network.vertex.names(n), "county" ])
  n %v% "photo" = as.numeric(s[ network.vertex.names(n), "photo" ]) # 0/1
  
  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
  
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
  
  #
  # weighted measures
  #
  
  n = get_modularity(n, weights = "raw", exclude = c("IND", "KYP", "MDG"))
  n = get_modularity(n, weights = "nfw", exclude = c("IND", "KYP", "MDG"))
  n = get_modularity(n, weights = "gsw", exclude = c("IND", "KYP", "MDG"))
  
  n = get_centrality(n, weights = "raw")
  n = get_centrality(n, weights = "nfw")
  n = get_centrality(n, weights = "gsw")
 
  #
  # network plot
  #
  
  if(plot) {
    
    q = n %v% "degree"
    q = as.numeric(cut(q, unique(quantile(q)), include.lowest = TRUE))
    
    ggnet_save(n, file = paste0("plots/net_no", ii),
               i = colors[ s[ n %e% "source", "party" ] ],
               j = colors[ s[ n %e% "target", "party" ] ],
               q, colors, order)
    
  }
  
  #
  # save objects (legislatures only)
  #
  
  if(grepl("\\d{4}", ii)) {
    
    assign(paste0("net_no", substr(ii, 1, 4)), n)
    assign(paste0("edges_no", substr(ii, 1, 4)), edges)
    assign(paste0("bills_no", substr(ii, 1, 4)), data)
    
  }
    
  # 
  # export gexf (only for themes)
  #

  if(gexf & !grepl("\\d{4}", ii))
    get_gexf(paste0("net_no", ii), n, meta, mode, colors, extra = "constituency")
  
}

save(list = ls(pattern = "^(net|edges|bills)_no\\d{4}$"), file = "data/net_no.rda")

# zip gexf (only for themes)
if(gexf)
  zip("net_no.zip", dir(pattern = "^net_no\\w+\\.gexf$"))

# have a nice day
