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

InfoDataset <- function(data, output) {
    # find species distribution when lost landscape is 0.25 0.8 and 0.9
    for (i in species) {
        cat(c(i, " "))
        cat(approx(data$Prop_landscape_lost, data[,i], xout=c(0.25, 0.8, 0.9))$y)
        cat("\n")
    }
    # find average as well when lost landscape is 0.25 0.8 and 0.9
    cat(approx(data$Prop_landscape_lost, data$ave_prop_rem, xout=c(0.25, 0.8, 0.9))$y)
    # tidy data and plot curves
    dmelt <- melt(data, id.vars="Prop_landscape_lost")
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

    png(file=output, width=800, height=500)
    print(pplot)
    dev.off()
    remove(pplot)
    remove(dtall)
    remove(dmelt)
    remove(data)
}

dset1 <- read.table(file="./data/exercise1.csv", sep=",", header=TRUE)
InfoDataset(dset1, "./plots/exercise1.png")
dset2 <- read.table(file="./data/exercise2.csv", sep=",", header=TRUE)
InfoDataset(dset2, "./plots/exercise2.png")

# Compare exercise 1 and exercise 2 with a plot facet wrap by species
# prepare data
dmelt1 <- melt(dset1, id.vars="Prop_landscape_lost")
dtall1 <- subset(dmelt1, (variable=="ave_prop_rem" | variable %in% species))
colnames(dtall1) <- c("Proportion", "Species", "Value")

dmelt2 <- melt(dset2, id.vars="Prop_landscape_lost")
dtall2 <- subset(dmelt2, (variable=="ave_prop_rem" | variable %in% species))
colnames(dtall2) <- c("Proportion", "Species", "Value")

dtall1$exercise <- "1"
dtall2$exercise <- "2"
dtall12 <- rbind(dtall1, dtall2)
# arrange levels
levels(dtall12$Species) <- c("cost_needed_for_top_fraction", "min_prop_rem", "Average", "W_prop_rem", "ext.1", "ext.2", "Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7")
dtall12$Species <- factor(dtall12$Species, levels = c("cost_needed_for_top_fraction", "min_prop_rem", "W_prop_rem", "ext.1", "ext.2", "Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7", "Average"))
pplot <- ggplot(dtall12, aes(Proportion, Value, color=exercise)) + 
    geom_line(size=1) + 
    facet_wrap(~ Species, ncol = 4) +
    ggtitle("")  +
    labs(title="", x="Lost landscape", y="Remaining distributions", color="") +
    scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_colour_manual(labels=c("Exercise 1", "Exercise 2"), values=c("#ff0000", "#048c4e")) +
    theme(plot.title=element_text(hjust=0.5),
        axis.title.x=element_text(size=ptextsize+2, family=pfamily),
        axis.title.y=element_text(size=ptextsize+2, family=pfamily),
        axis.text=element_text(size=ptextsize-8 , family=pfamily),
        text=element_text(size=ptextsize+2, family=pfamily),
        legend.spacing=unit(0, "cm"),
        rect=element_blank()) +
    theme(axis.line.x=element_line(color="grey", size=0.5),
        axis.line.y=element_line(color="grey", size=0.5)) +
    theme(panel.grid=element_blank(), panel.border=element_blank())

png(file="./plots/exercise2-1.png", width=1200, height=600)
print(pplot)
dev.off()
remove(pplot)
remove(dtall12)
remove(dtall1)
remove(dtall2)
remove(dmelt1)
remove(dmelt2)
