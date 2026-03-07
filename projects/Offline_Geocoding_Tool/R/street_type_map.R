# =============================================================================
# Street type: full words and abbreviations -> canonical abbreviation
# =============================================================================
# Sources: Canada Post addressing guidelines (symbols and abbreviations),
# StatCan Table 4.2 Street type (NAR/address standards), French (F) and English (E).
# Used to normalize input addresses so "avenue" / "av" / "ave" match consistently.
# =============================================================================

#' Map every variant (full word or abbreviation) to one canonical street-type code (lowercase).
#' Names = variant (lowercase), values = canonical (lowercase).
#' Enables matching across English/French and full/abbreviated input.
#' @export
STREET_TYPE_MAP <- c(
  # A
  "abbey" = "abbey", "abbeys" = "abbey",
  "access" = "access",
  "acres" = "acres",
  "aire" = "aire",
  "alley" = "alley", "alleys" = "alley",
  "allée" = "allee", "allee" = "allee", "allées" = "allee",
  "autoroute" = "aut", "aut" = "aut",
  "avenue" = "ave", "ave" = "ave", "av" = "ave", "avenues" = "ave",
  # B
  "bay" = "bay", "bays" = "bay",
  "beach" = "beach", "beaches" = "beach",
  "bend" = "bend", "bends" = "bend",
  "block" = "block", "blocks" = "block", "bloc" = "block",
  "boulevard" = "blvd", "blvd" = "blvd", "boul" = "blvd", "boulevards" = "blvd",
  "bourg" = "bourg",
  "barrage" = "brge", "brge" = "brge",
  "brook" = "brook", "brooks" = "brook",
  "bypass" = "bypass", "by-pass" = "bypass",
  "byway" = "byway", "byways" = "byway",
  # C
  "campus" = "campus",
  "cape" = "cape", "capes" = "cape",
  "carré" = "car", "carre" = "car", "car" = "car",
  "carrefour" = "carref", "carref" = "carref",
  "centre" = "ctr", "center" = "ctr", "ctr" = "ctr", "c" = "ctr",
  "chase" = "chase", "chases" = "chase",
  "chemin" = "ch", "ch" = "ch", "chemins" = "ch",
  "circle" = "cir", "cir" = "cir", "circles" = "cir", "cercle" = "cir",
  "circuit" = "circt", "circt" = "circt",
  "close" = "close", "closes" = "close",
  "common" = "common", "commons" = "common",
  "concession" = "conc", "conc" = "conc", "concessions" = "conc",
  "côte" = "cote", "cote" = "cote", "cotes" = "cote",
  "corners" = "crnrs", "crnrs" = "crnrs",
  "court" = "crt", "crt" = "crt", "ct" = "crt", "courts" = "crt", "cour" = "crt", "cours" = "crt",
  "cove" = "cove", "coves" = "cove",
  "crescent" = "cres", "cres" = "cres", "crescents" = "cres", "croissant" = "crois", "crois" = "crois",
  "crest" = "crest", "crests" = "crest",
  "crossing" = "cross", "cross" = "cross", "crossings" = "cross",
  "crossroads" = "crssrd", "crssrd" = "crssrd",
  "croft" = "croft", "crofts" = "croft",
  "cul-de-sac" = "cds", "cds" = "cds", "culdesac" = "cds",
  # D
  "dale" = "dale", "dales" = "dale",
  "dell" = "dell", "dells" = "dell",
  "desserte" = "desste", "desste" = "desste",
  "diversion" = "divers", "divers" = "divers",
  "downs" = "downs",
  "drive" = "dr", "dr" = "dr", "drives" = "dr",
  "droit de passage" = "drpass", "drpass" = "drpass",
  # E
  "échangeur" = "ech", "echangeur" = "ech", "ech" = "ech",
  "end" = "end", "ends" = "end",
  "esplanade" = "espl", "espl" = "espl", "esplanades" = "espl",
  "estate" = "estate", "estates" = "estate",
  "expressway" = "expy", "expy" = "expy", "expressways" = "expy",
  "extension" = "exten", "exten" = "exten", "ext" = "exten",
  # F
  "farm" = "farm", "farms" = "farm",
  "field" = "field", "fields" = "field",
  "forest" = "forest", "forests" = "forest",
  "front" = "front",
  "fsr" = "fsr",
  "freeway" = "fwy", "fwy" = "fwy", "freeways" = "fwy",
  # G
  "gate" = "gate", "gates" = "gate",
  "gardens" = "gdns", "gdns" = "gdns", "garden" = "gdns",
  "glade" = "glade", "glades" = "glade",
  "glen" = "glen", "glens" = "glen",
  "green" = "green", "greens" = "green",
  "grounds" = "grnds", "grnds" = "grnds",
  "grove" = "grove", "groves" = "grove",
  # H
  "harbour" = "harbr", "harbor" = "harbr", "harbr" = "harbr", "harbours" = "harbr",
  "haven" = "haven", "havens" = "haven",
  "heath" = "heath",
  "heights" = "hts", "hts" = "hts",
  "highlands" = "hglds", "hglds" = "hglds",
  "highway" = "hwy", "hwy" = "hwy", "highways" = "hwy",
  "hill" = "hill", "hills" = "hill",
  "hollow" = "hollow", "hollows" = "hollow",
  # I
  "inlet" = "inlet", "inlets" = "inlet",
  "île" = "ile", "ile" = "ile", "island" = "island", "islands" = "island",
  "impasse" = "imp", "imp" = "imp", "impasses" = "imp",
  # K
  "key" = "key", "keys" = "key",
  "knoll" = "knoll", "knolls" = "knoll",
  # L
  "landing" = "landng", "landng" = "landng", "landings" = "landng",
  "lane" = "lane", "lanes" = "lane", "ln" = "lane",
  "laneway" = "lanewy", "lanewy" = "lanewy", "laneways" = "lanewy",
  "limits" = "lmts", "lmts" = "lmts",
  "line" = "line", "lines" = "line",
  "link" = "link", "links" = "link",
  "lookout" = "lkout", "lkout" = "lkout", "lookouts" = "lkout",
  "loop" = "loop", "loops" = "loop",
  # M
  "mall" = "mall", "malls" = "mall",
  "manor" = "manor", "manors" = "manor",
  "maze" = "maze", "mazes" = "maze",
  "meadow" = "meadow", "meadows" = "meadow",
  "mews" = "mews",
  "montée" = "montee", "montee" = "montee", "montées" = "montee",
  "moor" = "moor", "moors" = "moor",
  "mount" = "mount", "mounts" = "mount",
  "mountain" = "mtn", "mtn" = "mtn", "mountains" = "mtn",
  # O
  "orchard" = "orch", "orch" = "orch", "orchards" = "orch",
  # P
  "parade" = "parade", "parades" = "parade",
  "parc" = "parc", "park" = "pk", "pk" = "pk", "parks" = "pk",
  "passage" = "pass", "pass" = "pass", "passages" = "pass",
  "path" = "path", "paths" = "path",
  "pathway" = "ptway", "ptway" = "ptway", "pathways" = "ptway",
  "peak" = "peak", "peaks" = "peak",
  "pines" = "pines",
  "place" = "pl", "pl" = "pl", "places" = "pl",
  "plateau" = "plat", "plat" = "plat", "plateaux" = "plat",
  "plaza" = "plaza", "plazas" = "plaza",
  "pointe" = "pointe", "pointes" = "pointe", "point" = "pt", "pt" = "pt", "points" = "pt",
  "port" = "port", "ports" = "port",
  "private" = "pvt", "pvt" = "pvt",
  "promenade" = "prom", "prom" = "prom", "promenades" = "prom",
  # Q
  "quai" = "quai", "quays" = "quay", "quay" = "quay",
  # R
  "ramp" = "ramp", "ramps" = "ramp",
  "rang" = "rang", "range" = "rg", "rg" = "rg", "ranges" = "rg",
  "reach" = "reach", "reaches" = "reach",
  "ridge" = "ridge", "ridges" = "ridge",
  "rise" = "rise", "rises" = "rise",
  "rond point" = "rdpt", "rdpt" = "rdpt",
  "road" = "rd", "rd" = "rd", "roads" = "rd",
  "route" = "rte", "rte" = "rte", "routes" = "rte",
  "row" = "row", "rows" = "row",
  "ruelle" = "rle", "rle" = "rle", "ruelles" = "rle",
  "rue" = "rue", "rues" = "rue",
  "ruisseau" = "ruis", "ruis" = "ruis",
  "run" = "run", "runs" = "run",
  "right of way" = "rtofwy", "rtofwy" = "rtofwy",
  # S
  "section" = "sectn", "sectn" = "sectn", "sections" = "sectn",
  "sentier" = "sent", "sent" = "sent", "sentiers" = "sent",
  "sideroad" = "siderd", "siderd" = "siderd", "sideroads" = "siderd",
  "square" = "sq", "sq" = "sq", "squares" = "sq",
  "street" = "st", "st" = "st", "streets" = "st",
  "stroll" = "stroll", "strolls" = "stroll",
  "subdivision" = "subdiv", "subdiv" = "subdiv", "subdivisions" = "subdiv",
  # T
  "terrace" = "terr", "terr" = "terr", "terraces" = "terr", "terrasse" = "tsse", "tsse" = "tsse",
  "thicket" = "thick", "thick" = "thick", "thickets" = "thick",
  "townline" = "tline", "tline" = "tline", "townlines" = "tline",
  "towers" = "towers",
  "trace" = "trace", "traces" = "trace",
  "trail" = "trail", "trails" = "trail",
  "trunk" = "trunk", "trunks" = "trunk",
  "turnabout" = "trnabt", "trnabt" = "trnabt", "turnabouts" = "trnabt",
  # V
  "vale" = "vale", "vales" = "vale",
  "via" = "via", "vias" = "via",
  "view" = "view", "views" = "view",
  "village" = "villge", "villge" = "villge", "villages" = "villge",
  "villas" = "villas",
  "vista" = "vista", "vistas" = "vista",
  "voie" = "voie", "voies" = "voie",
  # W
  "walk" = "walk", "walks" = "walk",
  "way" = "way", "ways" = "way",
  "wharf" = "wharf", "wharves" = "wharf", "wharfs" = "wharf",
  "wood" = "wood", "woods" = "wood",
  "wynd" = "wynd", "wynds" = "wynd"
)

# Duplicate names would overwrite; ensure unique (first occurrence wins per variant)
if (any(duplicated(names(STREET_TYPE_MAP)))) {
  keep <- !duplicated(names(STREET_TYPE_MAP))
  STREET_TYPE_MAP <- STREET_TYPE_MAP[keep]
}
