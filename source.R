options(menu.graphics = FALSE)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install(c("graph", "Rgraphviz"))
setRepositories()
2
install.packages("gRain", dependencies=TRUE);
install.packages("bnlearn")

library(bnlearn)
df <- data.frame(read.csv("./songs_normalize.csv"))
colnames(df)
head(df[c("artist",  "song", "genre")], 3)

library(stringr)
library(forcats)
one_gen <- function(entry)
  str_split(entry, ", ")[[1]][1]
df$genre <- as_factor(sapply(c(df$genre), one_gen))
df$explicit <- df$explicit == "True"
head(df[c("artist",  "song", "genre")], 3)

par(mfrow = c(2, 1))
hist(df$instrumentalness)
hist(log(df$instrumentalness))

pdf("hists.pdf")
par(mfrow = c(2, 1))
hist(df$instrumentalness)
hist(log(df$instrumentalness))
a <- dev.off()
par(mfrow = c(1, 1))

df <- subset(df, select = -c(artist, song))
df$duration_ms <- df$duration_ms/1.0
df$popularity <- df$popularity/1.0
head(df, 3)

df$instrumentalness <- log(df$instrumentalness)
df$instrumentalness[df$instrumentalness == -Inf] <- min(df[df$instrumentalness != -Inf, ]$instrumentalness) - 1
explicit <- df$explicit
year <- df$year
key <- df$key
mode <- df$mode
instrumentalness <- df$instrumentalness

df <- discretize(subset(df, select = -c(explicit, year, key, mode, instrumentalness)), breaks = 10)
df$explicit <- as.factor(explicit)
df$year <- as.factor(year)
df$key <- as.factor(key)
df$mode <- as.factor(mode)
tmp <- discretize(data.frame(instrumentalness), breaks = 10, method = "interval")
df$instrumentalness <- tmp$instrumentalness

iamb_graph <- iamb(df)
graphviz.plot(iamb_graph, layout = "dot")

pdf("graph.pdf", height = 3.5)
graphviz.plot(iamb_graph, layout = "dot")
a <- dev.off()

gs_graph <- gs(df)
graphviz.plot(gs_graph, layout = "dot")

pdf("graph_gs.pdf", height = 3.5)
graphviz.plot(gs_graph, layout = "dot")
a <- dev.off()

pc_stable_graph <- pc.stable(df)
graphviz.plot(pc_stable_graph, layout = "dot")

pdf("graph_pc.pdf", height=3.5)
graphviz.plot(pc_stable_graph, layout = "dot")
a <- dev.off()

# IAMB
iamb_graph <- set.arc(iamb_graph, "tempo", "speechiness")
iamb_graph <- set.arc(iamb_graph, "key", "mode")
iamb_graph <- set.arc(iamb_graph, "year", "popularity")
# PC.STABLE
pc_stable_graph <- set.arc(pc_stable_graph, "loudness", "energy")
pc_stable_graph <- set.arc(pc_stable_graph, "key", "mode")

c(loss(bn.cv(df, iamb_graph)), loss(bn.cv(df, gs_graph)), loss(bn.cv(df, pc_stable_graph)))

fit_mle <- bn.fit(iamb_graph, df, method = "mle")

library(gRain)
junc <- compile(as.grain(fit_mle))
querygrain(setEvidence(junc, evidence = list(explicit="yes")), nodes=c("genre", "speeciness"), type="conditional")
ev_explicit <- querygrain(setEvidence(junc, evidence=list(explicit="yes")))
plot(ev_explicit$genre)

pdf("explicit.pdf")
plot(ev_explicit$genre)
a <- dev.off()

ev_energy <- querygrain(setEvidence(junc, evidence=list(energy="(0.861,0.906]")))
plot(ev_energy$loudness)

pdf("energy.pdf")
plot(ev_explicit$loudness)
a <- dev.off()
