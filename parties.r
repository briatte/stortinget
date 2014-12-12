# party colors

colors = c(
  "SV"  = "#E41A1C", # SV  - Sosialistisk Venstreparti -- red
  "MDG" = "#B3DE69", # MDG - Miljøpartiet De Grønne    -- light green/olive
  "AP"  = "#F781BF", # Ap  - Arbeiderpartiet           -- pink
  "SP"  = "#4DAF4A", # Sp  - Senterpartiet             -- green
  "V"   = "#01665E", # V   - Venstre                   -- dark green/teal
  "KRF" = "#FFFF33", # KrF - Kristelig Folkeparti      -- yellow
  "KYP" = "#444444", #     - Kystpartiet               -- dark grey
  "H"   = "#80B1D3", # H   - Høyre                     -- light blue
  "FRP" = "#377EB8", # FrP - Fremskrittspartiet        -- blue
  "IND" = "#AAAAAA"  #     - Independent               -- light grey
)

# ParlGov Left/Right scores

scores = c(
  # Left
  "SV"  = 1.6,
  "MDG" = 2.6,
  "AP"  = 3.4,
  # Centre
  "SP"  = 4.7,
  "V"   = 5.1,
  "KRF" = 5.9,
  # Right
  "KYP" = 7.4,
  "H"   = 7.9,
  "FRP" = 8.8,
  # Other
  "IND" = Inf
)

stopifnot(names(colors) == names(scores))
order = names(colors)[ order(scores) ]
