library(dplyr)
library(ggplot2)

lvls <- c("Keine Umsetzung", "Mit der Umsetzung begonnen", "Vollständig umgesetzt")
df1 <- data.frame(status = sample(x = lvls, size = 360, replace = T, prob = c(.3,.5,.2)),
                 funktion = rep(c('Materialwirtschaft', 'Apotheke', 'Essensbestellung'), 3))
df2 <- data.frame(funktion = c('Apotheke', 'Materialwirtschaft', 'Essensbestellung'), y = c(.5,.6,.7))

wcs_colors <- c('#dfe1e7', '#c5d1d7', '#c0c3d0', "#a0a5b8", '#88a1ad')
p <- ggplot(df1, mapping = aes(x = funktion, fill = factor(status))) + 
    geom_bar(position = "fill") +
    geom_bar(data = df2, mapping = aes(x = funktion, y = y, fill = NULL), stat = "identity", width = .2) +
    theme(axis.title  = element_blank()) +
    scale_fill_manual(values = wcs_colors) +
    theme(legend.title = element_blank()) + coord_flip() +
    theme(panel.background = element_blank()) +
    ggtitle("Multiple Indicator Views") +
    theme(legend.position = "bottom") +
    theme(text = element_text(family = ""))

# ggsave("multiple-indicator-views.tiff", height = 4, width = 10)
