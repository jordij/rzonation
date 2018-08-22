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
species_labels_wo_avg <- c("Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7")
palette_wo_avg <- c("Species 1" = "#ff0000", "Species 2" = "#ceb600", "Species 3" = "#048c4e", "Species 4" = "#007cff", "Species 5" = "#ffa72d", "Species 6" = "#b05bff", "Species 7" = "#41ff1c")
palette2 <- c("#ff0000", "#ceb600", "#048c4e", "#007cff", "#ffa72d", "#b05bff", "#41ff1c")

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
        labs(title="", x="Lost landscape", y="Remaining distribution", color="") +
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
dset3 <- read.table(file="./data/exercise3.csv", sep=",", header=TRUE)
InfoDataset(dset3, "./plots/exercise3.png")
dset4 <- read.table(file="./data/exercise4.csv", sep=",", header=TRUE)
InfoDataset(dset4, "./plots/exercise4.png")
dset5 <- read.table(file="./data/exercise5.csv", sep=",", header=TRUE)
InfoDataset(dset5, "./plots/exercise5.png")
dset5_nocost <- read.table(file="./data/exercise5_wo_c.csv", sep=",", header=TRUE)
InfoDataset(dset5_nocost, "./plots/exercise5-no_cost.png")
dset6 <- read.table(file="./data/exercise6.csv", sep=",", header=TRUE)
InfoDataset(dset5, "./plots/exercise6.png")
dset6_nomask <- read.table(file="./data/exercise6_wo_m.csv", sep=",", header=TRUE)
InfoDataset(dset6_nomask, "./plots/exercise6-no_mask.png")

species_fish = c("anc","bar","bra","car","cbe","cdo","cuc","egr","ele","ema","eso","fro","gsp","gur","hak","hap","hok","jdo","jgu","jmd","jmm","jmn","kah","kin","lea","lin","lso","mdo","nsd","pco","pil","pop","rbm","rbt","rco","rmu","sbw","scg","sch","sfl","ski","sna","spd","spe","spo","spz","ssh","ssi","sty","swa","tar","tre","war","wit","wra","ybf")
endemic_species_fish = c("car", "cdo", "eso", "gsp", "lso", "nsd", "pco", "pop", "sfl", "spe", "spo", "spz", "ssh", "sty", "wit", "ybf")
non_endemic_species_fish = c("anc","bar","bra","cbe","cuc","egr","ele","ema","fro","gur","hak","hap","hok","jdo","jgu","jmd","jmm","jmn","kah","kin","lea","lin","mdo","pil","rbm","rbt","rco","rmu","scg","sch","ski","sna","spd","ssi","swa","tar","tre","war","wra")
species_fish_labels = c("Average","anc","bar","bra","car","cbe","cdo","cuc","egr","ele","ema","eso","fro","gsp","gur","hak","hap","hok","jdo","jgu","jmd","jmm","jmn","kah","kin","lea","lin","lso","mdo","nsd","pco","pil","pop","rbm","rbt","rco","rmu","sbw","scg","sch","sfl","ski","sna","spd","spe","spo","spz","ssh","ssi","sty","swa","tar","tre","war","wit","wra","ybf")
dset7 <- read.table(file="./data/nzfish.csv", sep=",", header=TRUE)
dmelt <- melt(dset7, id.vars="Prop_landscape_lost")
#dtall <- subset(dmelt, (variable=="ave_prop_rem" | variable %in% species_fish))
dtall <- subset(dmelt, (variable=="ave_prop_rem" | variable=="min_prop_rem"))
colnames(dtall) <- c("Proportion", "Species", "Value")
pplot <- ggplot(dtall, aes(Proportion, Value, color=Species)) + 
    geom_line(size=1) +
    # scale_linetype_manual(guide=FALSE, values=c("dashed", "solid", "solid", "solid", "solid", "solid", "solid", "solid"))+
    ggtitle("")  +
    labs(title="", x="Lost landscape", y="Remaining distribution", color="") +
    scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_color_manual(labels = c("Average", "Minimum"), values = c("red", "darkgreen")) +
    #scale_colour_manual(labels=species_labels, values=palette) +
    # scale_colour_manual(labels=species_fish_labels) +
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

png(file="./plots/exercise7.png", width=800, height=500)
print(pplot)
dev.off()
remove(pplot)
remove(dset7)
remove(dmelt)
remove(dtall)

dset7 <- read.table(file="./data/nzfish_no_sbw.csv", sep=",", header=TRUE)
dmelt <- melt(dset7, id.vars="Prop_landscape_lost")
#dtall <- subset(dmelt, (variable=="ave_prop_rem" | variable %in% species_fish))
dtall <- subset(dmelt, (variable=="ave_prop_rem" | variable=="min_prop_rem"))
colnames(dtall) <- c("Proportion", "Species", "Value")
pplot <- ggplot(dtall, aes(Proportion, Value, color=Species)) + 
    geom_line(size=1) +
    # scale_linetype_manual(guide=FALSE, values=c("dashed", "solid", "solid", "solid", "solid", "solid", "solid", "solid"))+
    ggtitle("")  +
    labs(title="", x="Lost landscape", y="Remaining distribution", color="") +
    scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_color_manual(labels = c("Average", "Minimum"), values = c("red", "darkgreen")) +

    #scale_colour_manual(labels=species_labels, values=palette) +
    # scale_colour_manual(labels=species_fish_labels) +
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

png(file="./plots/exercise7-no_sbw.png", width=800, height=500)
print(pplot)
dev.off()
remove(pplot)
remove(dset7)
remove(dmelt)
remove(dtall)

dset7 <- read.table(file="./data/nzfish_no_sbw.csv", sep=",", header=TRUE)
dmelt <- melt(dset7, id.vars="Prop_landscape_lost")
dtall <- subset(dmelt, (variable %in% species_fish))
colnames(dtall) <- c("Proportion", "Species", "Value")
pplot <- ggplot(dtall, aes(Proportion, Value)) + 
    geom_line(size=1, color="darkgreen") + 
    facet_wrap(~ Species, ncol = 5) +
    # scale_linetype_manual(guide=FALSE, values=c("dashed", "solid", "solid", "solid", "solid", "solid", "solid", "solid"))+
    ggtitle("")  +
    labs(title="", x="Lost landscape", y="Remaining distribution", color="") +
    scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
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

png(file="./plots/exercise7_facets.png", width=1200, height=1200)
print(pplot)
dev.off()
remove(pplot)
remove(dset7)
remove(dmelt)
remove(dtall)

# Comparing exercises
dmelt1 <- melt(dset1, id.vars="Prop_landscape_lost")
dtall1 <- subset(dmelt1, (variable=="ave_prop_rem" | variable %in% species))
colnames(dtall1) <- c("Proportion", "Species", "Value")

dmelt2 <- melt(dset2, id.vars="Prop_landscape_lost")
dtall2 <- subset(dmelt2, (variable=="ave_prop_rem" | variable %in% species))
colnames(dtall2) <- c("Proportion", "Species", "Value")

dmelt3 <- melt(dset3, id.vars="Prop_landscape_lost")
dtall3 <- subset(dmelt3, (variable=="ave_prop_rem" | variable %in% species))
colnames(dtall3) <- c("Proportion", "Species", "Value")

dmelt5 <- melt(dset5, id.vars="Prop_landscape_lost")
dtall5 <- subset(dmelt5, (variable=="ave_prop_rem" | variable %in% species))
colnames(dtall5) <- c("Proportion", "Species", "Value")

dmelt5_nc <- melt(dset5_nocost, id.vars="Prop_landscape_lost")
dtall5_nc <- subset(dmelt5_nc, (variable=="ave_prop_rem" | variable %in% species))
colnames(dtall5_nc) <- c("Proportion", "Species", "Value")

dmelt6 <- melt(dset6, id.vars="Prop_landscape_lost")
dtall6 <- subset(dmelt6, (variable=="ave_prop_rem" | variable %in% species))
colnames(dtall6) <- c("Proportion", "Species", "Value")

dmelt6_nm <- melt(dset6_nomask, id.vars="Prop_landscape_lost")
dtall6_nm <- subset(dmelt6_nm, (variable=="ave_prop_rem" | variable %in% species))
colnames(dtall6_nm) <- c("Proportion", "Species", "Value")

dset7 <- read.table(file="./data/nzfish_no_sbw.csv", sep=",", header=TRUE)

dfishbqp5 <- read.table(file="./data/nzfish_bqp_5.csv", sep=",", header=TRUE)
dfishbqp10 <- read.table(file="./data/nzfish_bqp_10.csv", sep=",", header=TRUE)
dfishbqp20 <- read.table(file="./data/nzfish_bqp_20.csv", sep=",", header=TRUE)

dfishw2 <- read.table(file="./data/nzfish_weight2.csv", sep=",", header=TRUE)
dfishw5 <- read.table(file="./data/nzfish_weight5.csv", sep=",", header=TRUE)

dfishw2$avg_endemic <- rowMeans(dfishw2[names(dfishw2) %in% endemic_species_fish]) 
dfishw5$avg_endemic <- rowMeans(dfishw5[names(dfishw5) %in% endemic_species_fish]) 
dfishw2$avg_non_endemic <- rowMeans(dfishw2[names(dfishw2) %in% non_endemic_species_fish]) 
dfishw5$avg_non_endemic <- rowMeans(dfishw5[names(dfishw5) %in% non_endemic_species_fish]) 


dmelt_fishw2 <- melt(dfishw2, id.vars="Prop_landscape_lost")
dtall_fishw2_end <- subset(dmelt_fishw2, (variable=="avg_endemic"))
dtall_fishw2_non_end <- subset(dmelt_fishw2, (variable=="avg_non_endemic"))
colnames(dtall_fishw2_end) <- c("Proportion", "Species", "Value")
colnames(dtall_fishw2_non_end) <- c("Proportion", "Species", "Value")

dmelt_fishw5 <- melt(dfishw5, id.vars="Prop_landscape_lost")
dtall_fishw5_end <- subset(dmelt_fishw5, (variable=="avg_endemic"))
dtall_fishw5_non_end <- subset(dmelt_fishw5, (variable=="avg_non_endemic"))
colnames(dtall_fishw5_end) <- c("Proportion", "Species", "Value")
colnames(dtall_fishw5_non_end) <- c("Proportion", "Species", "Value")

dmelt_fishbqp5 <- melt(dfishbqp5, id.vars="Prop_landscape_lost")
dtall1_fishbqp5 <- subset(dmelt_fishbqp5, (variable=="ave_prop_rem"))
colnames(dtall1_fishbqp5) <- c("Proportion", "Species", "Value")

dmelt_fishbqp10 <- melt(dfishbqp10, id.vars="Prop_landscape_lost")
dtall1_fishbqp10 <- subset(dmelt_fishbqp10, (variable=="ave_prop_rem"))
colnames(dtall1_fishbqp10) <- c("Proportion", "Species", "Value")

dmelt_fishbqp20 <- melt(dfishbqp20, id.vars="Prop_landscape_lost")
dtall1_fishbqp20 <- subset(dmelt_fishbqp20, (variable=="ave_prop_rem"))
colnames(dtall1_fishbqp20) <- c("Proportion", "Species", "Value")

dtall1_fishbqp5$exercise <- "BQP 5"
dtall1_fishbqp10$exercise <- "BQP 10"
dtall1_fishbqp20$exercise <- "BQP 20"

dtall_fishw2_end$weight <- "Endemic weight 2"
dtall_fishw2_non_end$weight <- "Non-endemic weight 2"
dtall_fishw5_end$weight <- "Endemic weight 5"
dtall_fishw5_non_end$weight <- "Non-endemic weight 5"

dtall1$exercise <- "1"
dtall2$exercise <- "2"
dtall3$exercise <- "3"
dtall5$exercise <- "With cost"
dtall5_nc$exercise <- "Without cost"
dtall6$exercise <- "With mask"
dtall6_nm$exercise <- "Without mask"

dtall12 <- rbind(dtall1, dtall2)
dtall23 <- rbind(dtall2, dtall3)
dtall55 <- rbind(dtall5, dtall5_nc)
dtall66 <- rbind(dtall6, dtall6_nm)
dtallbqp <- rbind(dtall1_fishbqp5, dtall1_fishbqp10, dtall1_fishbqp20)
dtallweighted <- rbind(dtall_fishw2_end, dtall_fishw2_non_end,dtall_fishw5_end,dtall_fishw5_non_end)

pplot <- ggplot(dtallweighted, aes(Proportion, Value, color=weight)) + 
    geom_line(size=1) + 
    ggtitle("")  +
    labs(title="", x="Lost landscape", y="Remaining distribution", color="") +
    scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_colour_manual(values=c("red", "darkgreen", "blue", "green")) +
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

png(file="./plots/nzfish_endemic.png", width=900, height=600)
print(pplot)
dev.off()
remove(pplot)

pplot <- ggplot(dtallbqp, aes(Proportion, Value, color=exercise)) + 
    geom_line(size=1) + 
    ggtitle("")  +
    labs(title="", x="Lost landscape", y="Remaining distribution", color="") +
    scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_colour_manual(values=c("red", "darkgreen", "blue")) +
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

png(file="./plots/nzfish_bqp.png", width=1200, height=600)
print(pplot)
dev.off()
remove(pplot)


# arrange levels
levels(dtall12$Species) <- c("cost_needed_for_top_fraction", "min_prop_rem", "Average", "W_prop_rem", "ext.1", "ext.2", "Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7")
dtall12$Species <- factor(dtall12$Species, levels = c("cost_needed_for_top_fraction", "min_prop_rem", "W_prop_rem", "ext.1", "ext.2", "Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7", "Average"))
pplot <- ggplot(dtall12, aes(Proportion, Value, color=exercise)) + 
    geom_line(size=1) + 
    facet_wrap(~ Species, ncol = 4) +
    ggtitle("")  +
    labs(title="", x="Lost landscape", y="Remaining distribution", color="") +
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

# arrange levels
levels(dtall23$Species) <- c("cost_needed_for_top_fraction", "min_prop_rem", "Average", "W_prop_rem", "ext.1", "ext.2", "Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7", "Planned Urban Areas")
dtall23$Species <- factor(dtall23$Species, levels = c("cost_needed_for_top_fraction", "min_prop_rem", "W_prop_rem", "ext.1", "ext.2", "Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7", "Planned Urban Areas", "Average"))
pplot <- ggplot(dtall23, aes(Proportion, Value, color=exercise)) + 
    geom_line(size=1) + 
    facet_wrap(~ Species, ncol = 4) +
    ggtitle("")  +
    labs(title="", x="Lost landscape", y="Remaining distribution", color="") +
    scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_colour_manual(labels=c("Exercise 2", "Exercise 3"), values=c("#ff0000", "#048c4e")) +
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

png(file="./plots/exercise3-1.png", width=1200, height=600)
print(pplot)
dev.off()

# arrange levels
levels(dtall55$Species) <- c("cost_needed_for_top_fraction", "min_prop_rem", "Average", "W_prop_rem", "ext.1", "ext.2", "Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7")
dtall55$Species <- factor(dtall55$Species, levels = c("cost_needed_for_top_fraction", "min_prop_rem", "W_prop_rem", "ext.1", "ext.2", "Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7", "Average"))
pplot <- ggplot(dtall55, aes(Proportion, Value, color=exercise)) + 
    geom_line(size=1) + 
    facet_wrap(~ Species, ncol = 4) +
    ggtitle("")  +
    labs(title="", x="Lost landscape", y="Remaining distribution", color="") +
    scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_colour_manual(values=c("#ff0000", "#048c4e")) +
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

png(file="./plots/exercise5-1.png", width=1200, height=600)
print(pplot)
dev.off()

levels(dtall66$Species) <- c("cost_needed_for_top_fraction", "min_prop_rem", "Average", "W_prop_rem", "ext.1", "ext.2", "Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7")
dtall66$Species <- factor(dtall66$Species, levels = c("cost_needed_for_top_fraction", "min_prop_rem", "W_prop_rem", "ext.1", "ext.2", "Species 1", "Species 2", "Species 3", "Species 4", "Species 5", "Species 6", "Species 7", "Average"))
pplot <- ggplot(dtall66, aes(Proportion, Value, color=exercise)) + 
    geom_line(size=1) + 
    facet_wrap(~ Species, ncol = 4) +
    ggtitle("")  +
    labs(title="", x="Lost landscape", y="Remaining distribution", color="") +
    scale_x_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_y_continuous(breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_colour_manual(values=c("#ff0000", "#048c4e")) +
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

png(file="./plots/exercise6-1.png", width=1200, height=600)
print(pplot)
dev.off()
remove(pplot)
remove(dtall12)
remove(dtall1)
remove(dtall2)
remove(dtall55)
remove(dtall5_nc)
remove(dtall5)
remove(dtall66)
remove(dtall6_nm)
remove(dtall6)
remove(dmelt1)
remove(dmelt2)
remove(dtall23)
remove(dmelt3)

# BQP Curves Exercise 4
dsetc <- read.table(file="./data/bqp_curves_exercise4.csv", sep=",", header=TRUE)
pplot <- ggplot(dsetc, aes(X, Y, color=Species)) + 
    geom_line(size=1) +
    ggtitle("")  +
    labs(title="", color="") +
    scale_x_continuous(trans = "reverse", name="Proportion of neighboring cells remaining", breaks=c(0, 0.25, 0.5, 0.75, 1), expand=c(0, 0)) +
    scale_y_continuous(name="Local value remaining", breaks=c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2), expand=c(0, 0)) +
    scale_colour_manual(values=palette_wo_avg) +
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

png(file="./plots/exercise4_bqp_curves.png", width=800, height=500)
print(pplot)
dev.off()
