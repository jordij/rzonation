library(extrafont)
library(ggplot2)
library(reshape2)


#############
## WARNING ##
#############
## I like using Gill Sans Nova typeface for plots and maps.
## If you need to load your fonts in Windows, uncomment the line below.
## If you don't have Gill Sans Nova locally just replace all occurences of `pfamily="Gill Sans Nove"`
## or with your typeface of choice.

# loadfonts(device="win")

pfamily <- "Gill Sans Nova"
ptextsize <- 20
species <- c("Species.1", "Species.2", "Species.3", "Species.4", "Species.5", "Species.6", "Species.7")
species_labels <- c("Average", "Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7")
palette <- c("#000000", "#ff0000", "#ceb600", "#048c4e", "#007cff", "#ffa72d", "#b05bff", "#41ff1c")

dset <- read.table(file="./data/exercise1.csv", sep=",", header=TRUE)
# find species distribution when lost landscape is 0.25 0.8 and 0.9
for (i in species) {
    cat(c(i, " "))
    cat(approx(dset$Prop_landscape_lost, dset[,i], xout=c(0.25, 0.8, 0.9))$y)
    cat("\n")
}
# find average as well when lost landscape is 0.25 0.8 and 0.9
cat(approx(dset$Prop_landscape_lost, dset$ave_prop_rem, xout=c(0.25, 0.8, 0.9))$y)
# tidy data and plot curves
dmelt <- melt(dset, id.vars="Prop_landscape_lost")
dtall <- subset(dmelt, (variable=="ave_prop_rem" | variable %in% species))
colnames(dtall) <- c("Proportion", "Species", "Value")
pplot <- ggplot(dtall, aes(Proportion, Value, color=Species)) + 
    geom_line(aes(linetype=Species), size=1) +
    scale_linetype_manual(guide=FALSE, values=c("dashed", "solid", "solid", "solid", "solid", "solid", "solid", "solid"))+
    ggtitle("")  +
    labs(title="", x="Lost landscape", y="Remaining distributions", color="") +
    scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    #scale_colour_manual(labels=species_labels, values=palette) +
    scale_colour_manual(labels=species_labels, values=palette) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize, family=pfamily),
        axis.title.y=element_text(size=ptextsize, family=pfamily),
        axis.text=element_text(size=ptextsize-2 , family=pfamily),
        text=element_text(size=ptextsize, family=pfamily),
        legend.spacing=unit(0, "cm"),
        rect=element_blank()) +
    theme(axis.line.x=element_line(color="grey", size=0.5),
        axis.line.y=element_line(color="grey", size=0.5)) +
    theme(panel.grid=element_blank(), panel.border=element_blank())

png(file="./plots/exercise1.png", width=800, height=500)
print(pplot)
dev.off()
remove(pplot)
remove(dtall)
remove(dmelt)
remove(dset)
