This repository contains code to build cosponsorship networks from bills passed in the [Norwegian Parliament](https://www.stortinget.no/).

- [interactive demo](http://briatte.org/stortinget)
- [static plots](http://briatte.org/stortinget/plots.html)

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors. Some sponsors do not have photos on their profiles, although most of them do. Because the photo identifier is identical to the profile identifier, the `photo` variable on each sponsor is a numeric dummy (0/1) to mark if a photo exists.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

Note -- Excluding minor parties like the Kystpartiet or the sole Green MP tends to increase modularity by a maximum of 0.1. Most graphs are not affected by the exclusions (tested on both legislatures and thematic graphs).

# DATA

## Bills

- `years` -- the year session of the bill (yyyy-yyyy)
- `doc` -- the filename of the bill
- `aut` -- bill sponsors, full text
- `url` -- bill sponsors, semicolon-separated profile identifiers
- `kwd` -- semicolon-separated keywords
- `n_au` -- total number of sponsors
- `legislature` -- legislature of the bill, computed from `years`

## Sponsors

- `uid` -- profile URL, shortened to unique identifier (a few letters and sometimes "_")
- `name` -- sponsor name
- `born` -- year of birth (num)
- `sex` -- sponsor gender (F/M)
- `party` -- political party, abbreviated
- `partyname` -- political party, full name (in Norwegian)
- `nyears` -- seniority, from the sponsor's profile (see below)
- `mandate` -- the last legislature in which the sponsor sat (see below)
- `type` -- parliamentary status
- `county` -- constituency
- `photo` -- 0 or 1 if the photo URL returned a picture for the sponsor

The `nyears` seniority variable counts all years up to the last legislature in which the sponsor has sat. The original variable also indicates days, but for simplicity, any amount of days superior to 365 / 2 is counted as one additional full year.

Before this variable is added to a graph, it receives a penalty equal to the time that separates the last legislature in which the sponsor sat (stored in the `mandate` variable) from the legislature for which seniority is being computed.

Consider, for instance, [MP "BB"](https://www.stortinget.no/no/Representanter-og-komiteer/Representantene/Representantfordeling/Representant/?perid=BB) (Berit Brørby) in the graph `net_no2001` for legislature 2001-2005:

- last legislature: 2005-2009
- legislature under consideration: 2001-2005
- seniority: 24 years

In this case, `nyears` will be equal to 24 - (2009-2005) = 20 years.
