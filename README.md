This repository contains code to build cosponsorship networks from bills passed in the [Norwegian Parliament](https://www.stortinget.no/).

- [interactive demo](http://briatte.org/stortinget)
- [static plots](http://briatte.org/stortinget/plots.html)

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors. Some sponsors do not have photos on their profiles, although most of them do. Because the photo identifier is identical to the profile identifier, the `photo` variable on each sponsor is a numeric dummy (0/1) to mark if a photo exists.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

Note -- excluding minor parties (the Kystpartiet, Framtid for Finnmark, Rød Valgallianse and the sole Green MP from the Miljøpartiet De Grønne) tends to increase partisan modularity by a maximum of 0.01. Most graphs are not affected by the exclusions.

# DATA

## Bills

- `id` -- unique identifier
- `session` -- the year session of the bill (yyyy-[yy]?yy)
- `legislature` -- legislature of the bill, identified from `session`
- `title` -- the title of the bill
- `status` -- the status of the bill (processed or withdrawn)
- `type` -- the type of the bill (general, budget or legislative)
- `committee` -- the committee of introduction of the bill
- `supervisors` -- the committee supervistors of the bill (semicolon-separated profile identifiers)
- `authors` -- bill sponsors (semicolon-separated profile identifiers)
- `keywords` -- semicolon-separated keywords
- `n_au` -- total number of sponsors
- `n_au` -- total number of committee supervisors
 
## Sponsors

The sponsors data have multiple entries for each sponsor (one per legislature in which the sponsor sat).

- `uid` -- profile URL, shortened to unique identifier (a few letters and sometimes "_")
- `name` -- sponsor name
- `born` -- year of birth (num)
- `sex` -- sponsor gender (F/M)
- `party` -- political party, abbreviated
- `partyname` -- political party, full name (in Norwegian)
- `mandate` -- the legislature in which the sponsor sat
- `nyears` -- all years in which the sponsor sat, computed from `mandate`
- `constituency` -- constituency, stored as the string to its Wikipedia English entry
- `photo` -- 0 or 1 if the photo URL returned a picture for the sponsor
