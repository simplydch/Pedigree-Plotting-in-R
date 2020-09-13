## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------

load("short_ped.RData")
head(short_ped)
dim(short_ped)


## ------------------------------------------------------------------------

## We need to take the Cohorts from the pedigree to provide our y-axis values

y_coor <- short_ped[, "Cohort"]

## We then need x-coordinates that spread each set of cohorts out evenly across the x-axis

## First we can use table to find how many individuals are in each cohort
## For each of these values we generate an evenly spaced sequence between 1 and 10
## The output of sapply is a list, so do.call is used to join the values
## into a single vector

y_counts <- unname(table(y_coor))
x_coor <- do.call(c, sapply(y_counts, function(x) seq(1, 10, length = x)))

# And we can have our first attempt at plotting

plot(x_coor, y_coor)


## ------------------------------------------------------------------------

## First we need to all pull the ids, sires and dams from the pedigree.

ids <- short_ped[, "ID"]
sires <- short_ped[, "SireID"]
dams <- short_ped[, "MumID"]

# And then get row references, and then coordinates for the sires.

sire_ref <- match(sires, ids)

x_sire <- x_coor[sire_ref]
y_sire <- y_coor[sire_ref]

# Segments creates lines between the points in the provided x and y - values

plot(x_coor, y_coor)
segments(x_coor, y_coor, x_sire, y_sire)


## ------------------------------------------------------------------------
# Lets rearrange the plot reversing the y-values.

plot(x_coor, y_coor, ylim = c(max(y_coor), min(y_coor)))

# And add those sire links again, this time making them orange.

segments(x_coor, y_coor, x_sire, y_sire, col = "orange")

# And we can then also add in the dams using blue lines

dam_ref <- match(dams, ids)

x_dam <- x_coor[dam_ref]
y_dam <- y_coor[dam_ref]

segments(x_coor, y_coor, x_dam, y_dam, col = "blue")


## ------------------------------------------------------------------------
par(mar = c(0.5, 4, 0.5, 0.5))
plot(x_coor, y_coor, ylim = c(max(y_coor), min(y_coor)), col = rgb(0, 0, 0, 0.5), pch = ".", cex = 1, frame.plot = FALSE, ylab = "", xlab = "", axes = FALSE)

## We can add in the Cohort information
axis(side = 2, labels = TRUE, tick = FALSE, las = 2, at = unique(y_coor))

segments(x_coor, y_coor, x_sire, y_sire, col = "orange")
segments(x_coor, y_coor, x_dam, y_dam, col = "blue")


## ------------------------------------------------------------------------

# Lets start with the background pedigree, we can specify the colours as rgb
# and make them grey and transparent.

plot(x_coor, y_coor,
  ylim = c(max(y_coor), min(y_coor)), col = rgb(0, 0, 0, 0.5), pch = ".", cex = 1,
  frame.plot = FALSE, axes = F, ylab = "", xlab = "")

axis(side = 2, labels = TRUE, tick = FALSE, las = 2, at = unique(y_coor))
segments(x_coor, y_coor, x_sire, y_sire, col = rgb(0.9, 0.9, 0.9, 0.6))
segments(x_coor, y_coor, x_dam, y_dam, col = rgb(0.9, 0.9, 0.9, 0.6))



# Lets plot the focal id (in red) using points

id_plot <- which(ids %in% "ID4")

points(x_coor[id_plot], y_coor[id_plot], col = "red")

# And also plot the points representing his offspring, this time in black.

ID_wSire <- which(sire_ref %in% id_plot)

points(x_coor[ID_wSire], y_coor[ID_wSire])

# So to plot the links, lets just turn off all the links that aren't connected to individual ID4. Since he is a sire we only have to plot sire links

x_sire2 <- x_sire
y_sire2 <- y_sire
x_sire2[sire_ref != id_plot] <- NA
y_sire2[sire_ref != id_plot] <- NA
segments(x_coor, y_coor, x_sire2, y_sire2, col = "orange")


## ------------------------------------------------------------------------
## Lets start again with a plain background

plot(x_coor, y_coor,
  ylim = c(max(y_coor), min(y_coor)), col = rgb(0, 0, 0, 0.5), pch = ".", cex = 1,
  frame.plot = FALSE, axes = F, ylab = "", xlab = ""
)
axis(side = 2, labels = TRUE, tick = FALSE, las = 2, at = unique(y_coor))
segments(x_coor, y_coor, x_sire, y_sire, col = rgb(0.9, 0.9, 0.9, 0.6))
segments(x_coor, y_coor, x_dam, y_dam, col = rgb(0.9, 0.9, 0.9, 0.6))


# The main hurdle is identifying all the descendants in the first place, so we'll use
# a simple loop

#  We set up vectors to hold the parents each iteration and also all the descendants
# our focal individual is included in both.  At each iteration of the loop we find all the
# individuals that are offspring of the individuals in the list of parents, and continue until there are no further offspring.

parents <- id_plot
desc_ref <- c(id_plot)

repeat{
  offspring <- c(ids[sire_ref %in% parents], ids[dam_ref %in% parents])
  off_ref <- which(ids %in% offspring)
  if (length(offspring) == 0) {
    break
  }
  desc_ref <- c(desc_ref, off_ref)
  parents <- off_ref
}

#
# We have now got all the relevant individual references
desc_ref

# So again we can highlight the focal individual and all it's descendants

points(x_coor[id_plot], y_coor[id_plot], col = "red")
points(x_coor[desc_ref], y_coor[desc_ref])


# This time we are interested in plotting both sire and dam links, but only to other
# individuals in the list of descendants

x_sire2[!(sire_ref %in% desc_ref & ids %in% ids[desc_ref])] <- NA
y_sire2[!(sire_ref %in% desc_ref & ids %in% ids[desc_ref])] <- NA
segments(x_coor, y_coor, x_sire2, y_sire2, col = "orange")

x_dam2 <- x_dam
y_dam2 <- y_dam
x_dam2[!(dam_ref %in% desc_ref & ids %in% ids[desc_ref])] <- NA
y_dam2[!(dam_ref %in% desc_ref & ids %in% ids[desc_ref])] <- NA
segments(x_coor, y_coor, x_dam2, y_dam2, col = "blue")


## ------------------------------------------------------------------------

plot(x_coor, y_coor,
  ylim = c(max(y_coor), min(y_coor)), col = rgb(0, 0, 0, 0.5), pch = ".", cex = 1,
  frame.plot = FALSE, axes = F, ylab = "", xlab = ""
)
axis(side = 2, labels = TRUE, tick = FALSE, las = 2, at = unique(y_coor), cex = 0.8)
segments(x_coor, y_coor, x_sire, y_sire, col = rgb(0.9, 0.9, 0.9, 0.6))
segments(x_coor, y_coor, x_dam, y_dam, col = rgb(0.9, 0.9, 0.9, 0.6))


x_sire2 <- x_sire
y_sire2 <- y_sire
x_sire2[!(sire_ref %in% desc_ref & ids %in% ids[desc_ref])] <- NA
y_sire2[!(sire_ref %in% desc_ref & ids %in% ids[desc_ref])] <- NA
segments(x_coor, y_coor, x_sire2, y_sire2, col = "#f1a340")


x_dam2 <- x_dam
y_dam2 <- y_dam
x_dam2[!(dam_ref %in% desc_ref & ids %in% ids[desc_ref])] <- NA
y_dam2[!(dam_ref %in% desc_ref & ids %in% ids[desc_ref])] <- NA
segments(x_coor, y_coor, x_dam2, y_dam2, col = "#998ec3")

