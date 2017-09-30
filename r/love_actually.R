### TIDYING/PARSING RAW SCRIPT TO DATA FRAME

library(dplyr)
library(stringr)
library(tidyr)

# Get path of executing script
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
print(getwd())

raw <- readLines("../rawdata/love_actually.txt")

# get all lines of dialog in script, with line #, scene #, and speaker attribution

# dense construction of "lines" below...

# dplyr/data_frame: create a tibble (a trimmed down version of data.frame())
# magrittr/%>%: forward pipe operator (reexported by dplyr)
# dplyr/filter: pick cases based on their values
# stringr/str_detect: tells if there is any match to the pattern
# dplyr/mutate: adds new variables that are functions of existing variables
# base/cumsum: returns a vector whose elements are the cumulative sums of the elements of the argument
# tidyr/separate: given either a regex or a vector of char positions, turns a single character column into multiple columns
# dplyr/group_by: takes an existing tbl and converts it into a grouped tbl where operations are performed "by group"
# dplyr/summarize: reduces multiple values down to a single summary
# stringr/str_c: joins multiple strings into a single string
lines <- data_frame(raw = raw) %>%
  filter(raw != "", !str_detect(raw, "(song)")) %>%
  mutate(is_scene = str_detect(raw, " Scene "),
         scene = cumsum(is_scene)) %>%
  filter(!is_scene) %>%
  separate(raw, c("speaker", "dialogue"), sep = ":", fill = "left") %>%
  group_by(scene, line = cumsum(!is.na(speaker))) %>%
  summarize(speaker = speaker[1], dialogue = str_c(dialogue, collapse = " "))

cast <- read.csv("../rawdata/love_actually_cast.csv")

# dplyr/inner_join: join two tbls together
# base/paste0: concatenate vectors after converting to character
lines <- lines %>%
  inner_join(cast) %>%
  mutate(character = paste0(speaker, " (", actor, ")"))

# dplyr/count: count observations by group
by_speaker_scene <- lines %>%
  count(scene, character)

by_speaker_scene

library(reshape2)
# reshape2/acast: cast a "molten" (i.e., long-format) data frame into a wide-format matrix
  # fun.aggregate: aggregate function needed if variables do not id a single observation for each output cell
  # setting to "length" below guarantees binary encoding (1, 0) since each character-scene pair has exactly one row in by_speaker_scece
# "character ~ scene"  represents a casting formula: characters become id vars for each row, and scenes become column names (see: http://seananderson.ca/2013/10/19/reshape.html)
speaker_scene_matrix <- by_speaker_scene %>%
  acast(character ~ scene, fun.aggregate = length)

# dimensions are # characters by # scenes (w/ dialog)
dim(speaker_scene_matrix)

### ANALYSIS

# (source: http://varianceexplained.org/r/love-actually-network/#fn:hclust) Normalized so that the # of scenes for each character adds up to 1;
# otherwise, we wouldn't be clustering based on a character's distribution across scenes so much as the number of scenes they're in
norm <- speaker_scene_matrix / rowSums(speaker_scene_matrix)

# stats/hclust: hierarchical clustering; analysis on a set of dissimilarities and methods for analyzing it
  # default method = "complete"
# stats/dist: distance matrix computation; computes and returns the distance matrix computed by using the specified distance measure to compute the distances between the rows of a data matrix
# Manhattan distance: for a binary matrix, this means "how many scenes is one of these characters in that the other isn't"
h <- hclust(dist(norm, method = "manhattan"))

# graphics/plot: generic x-y plotting
plot(h)

ordering <- h$labels[h$order]
ordering

# dplyr/n: the number of observations in the current group
# dplyr/ungroup: remove groupings from group_by
# dplyr/mutate: adds new variables that are functions of existing variables
# base/factor: used to encode a vector as a factor
scenes <- by_speaker_scene %>%
  filter(n() > 1) %>%        # scenes with > 1 character
  ungroup() %>%
  mutate(scene = as.numeric(factor(scene)),
         character = factor(character, levels = ordering))

library(ggplot2)
# ggplot2/ggplot: initialize a ggplot object
# ggplot2/aes: construct aesthetic mappings, which describe how vars in the data are mapped to visual properties (aethetics) of geoms
# ggplot2/geom_point: the point geom used to create scatterplots
# ggplot2/geom_path: connect observations in the order in which they appear in the data
ggplot(scenes, aes(scene, character)) +
  geom_point() +
  geom_path(aes(group = scene))

## Cooccurence matrix (http://stackoverflow.com/questions/13281303/creating-co-occurrence-matrix)

non_airport_scenes <- speaker_scene_matrix[, colSums(speaker_scene_matrix) < 10]

# base/%*% (matmult): multiples two matrices, if they are conformable
# base/t: given a matrix or data.frame x, t returns the transpose of x
cooccur <- non_airport_scenes %*% t(non_airport_scenes)

# stats/heatmap: draw a heat map
heatmap(cooccur)

library(igraph)
# igraph/graph.adjacency (graph_from_adjacency_matrix): create igraph graphs from adjacency matrices
# igraph/E: edges of a graph
g <- graph.adjacency(cooccur, weighted = TRUE, mode = "undirected", diag = FALSE)
plot(g, edge.width = E(g)$weight)
