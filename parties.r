# party colors

colors = c(
  "RV"  = "#B2182B", # RV  - Rød Valgallianse (1989)   -- dark red (not used)
  "SV"  = "#E41A1C", # SV  - Sosialistisk Venstreparti -- red
  "MDG" = "#B3DE69", # MDG - Miljøpartiet De Grønne    -- light green/olive
  "A"   = "#F781BF", # Ap  - Arbeiderpartiet           -- pink
  "SP"  = "#4DAF4A", # Sp  - Senterpartiet             -- green
  "V"   = "#01665E", # V   - Venstre                   -- dark green/teal
  "KRF" = "#FFFF33", # KrF - Kristelig Folkeparti      -- yellow
  "KP"  = "#444444", #     - Kystpartiet (includes TF) -- dark grey
  "H"   = "#80B1D3", # H   - Høyre                     -- light blue
  "FRP" = "#377EB8", # FrP - Fremskrittspartiet        -- blue
  "FFF" = "#984EA3", # FfF - Framtid for Finnmark (n = 1) -- purple
  "IND" = "#AAAAAA"  #     - Independent               -- light grey
)

# ParlGov Left/Right scores

scores = c(
  # Left
  "RV"  = 0.4,
  "SV"  = 1.6,
  "MDG" = 2.6,
  "A"   = 3.4,
  # Centre
  "SP"  = 4.7,
  "V"   = 5.1,
  "KRF" = 5.9,
  # Right
  "KP"  = 7.4,
  "H"   = 7.9,
  "FRP" = 8.8,
  # Other
  "FFF" = Inf, # missing
  "IND" = Inf
)

stopifnot(names(colors) == names(scores))
order = names(colors)[ order(scores) ]
