# order sponsors (in order to append the later ones)
s = arrange(s, name, mandate)

# remove government bills
b = filter(b, n_au > 0)

for(ii in rev(unique(b$legislature))) {
  
  cat("\n", ii)
  data = subset(b, legislature == ii & n_au > 1)
  sp = subset(s, mandate == ii)
  
  cat(":", nrow(data), "cosponsored documents, ")
  
  # merge authors and cosponsors
  data$authors
  
  # check for missing sponsors
  u = unlist(strsplit(data$authors, ";"))
  u = na.omit(u[ !u %in% sp$uid ])
  
  if(length(u)) {
    
    cat("Appending", n_distinct(u), "missing sponsors:\n")
    print(table(u))
    
    missing = filter(s, uid %in% u)
    missing = mutate(missing,
                     # compute distance to current legislature
                     delta = as.numeric(substr(ii, 1, 4)) - as.numeric(substr(mandate, 1, 4))) %>%
      # keep only earlier mandates
      filter(delta > 0) %>%
      group_by(name) %>%
      # keep closest mandate
      filter(delta == min(delta))
    
    # all missing sponsors should be found
    stopifnot(nrow(missing) == n_distinct(u))
    
    # append missing sponsors to current ones
    missing$delta = NULL
    sp = rbind(sp, missing)
    
  }
  
  #
  # directed edge list
  #
  
  edges = bind_rows(lapply(data$authors, function(d) {
    
    w = unlist(strsplit(d, ";"))
    
    d = expand.grid(i = sp$name[ sp$uid %in% w ],
                    j = sp$name[ sp$uid == w[1]], stringsAsFactors = FALSE)
    
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
  
  # sanity check
  stopifnot(edges$gsw <= 1)
  
  # final edge set: cosponsor, first author, weights
  edges = select(edges, i, j, raw, nfw, gsw)
  
  cat(nrow(edges), "edges, ")
  
  #
  # directed network
  #
  
  n = network(edges[, 1:2 ], directed = TRUE)
  
  n %n% "country" = meta[1]
  n %n% "title" = paste(meta[2], paste0(range(unique(substr(data$session, 1, 4))),
                                        collapse = " to "))
  
  n %n% "n_bills" = nrow(data)
  n %n% "n_sponsors" = table(subset(b, legislature == ii)$n_au)
  
  n_au = as.vector(n_au[ network.vertex.names(n) ])
  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")
  
  rownames(sp) = sp$name
  n %v% "url" = as.character(sp[ network.vertex.names(n), "uid" ])
  n %v% "sex" = as.character(sp[ network.vertex.names(n), "sex" ])
  n %v% "born" = as.numeric(substr(sp[ network.vertex.names(n), "born" ], 1, 4))
  n %v% "party" = as.character(sp[ network.vertex.names(n), "party" ])
  n %v% "partyname" = as.character(sp[ network.vertex.names(n), "partyname" ])
  n %v% "lr" = as.numeric(scores[ sp[ network.vertex.names(n), "party" ] ])
  
  sp$nyears = sapply(sp$nyears, function(x) {
    sum(unlist(strsplit(x, ";")) < substr(ii, 1, 4))
  })
  n %v% "nyears" = as.numeric(sp[ network.vertex.names(n), "nyears" ])
  
  n %v% "constituency" = as.character(sp[ network.vertex.names(n), "county" ])
  n %v% "photo" = as.numeric(sp[ network.vertex.names(n), "photo" ]) # 0/1
  
  # unweighted degree
  n %v% "degree" = degree(n)
  q = n %v% "degree"
  q = as.numeric(cut(q, unique(quantile(q)), include.lowest = TRUE))
  
  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
  
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
  
  #
  # network plot
  #
  
  if(plot) {
    
    save_plot(n, file = paste0("plots/net_no", ii),
              i = colors[ sp[ n %e% "source", "party" ] ],
              j = colors[ sp[ n %e% "target", "party" ] ],
              q, colors, order)
    
  }
  
  #
  # save objects
  #
  
  assign(paste0("net_no", substr(ii, 1, 4)), n)
  assign(paste0("edges_no", substr(ii, 1, 4)), edges)
  assign(paste0("bills_no", substr(ii, 1, 4)), data)
  
  #
  # export gexf
  #
  
  if(gexf)
    save_gexf(paste0("net_no", ii), n, meta, mode, colors, extra = "constituency")
  
}

#
# save
#

if(gexf)
  zip("net_no.zip", dir(pattern = "^net_no\\d{4}-\\d{4}\\.gexf$"))

save(list = ls(pattern = "^(net|edges|bills)_no\\d{4}$"),
     file = "data/net_no.rda")

# kthxbye
