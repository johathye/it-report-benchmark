library(dplyr)
library(ggplot2)

# generate sample data 
set.seed(342)
lvl <- c("Laggards", "Late Majority", 'Early Majority', 'Early Adopters', "Innovators")
n = 180
wcs <- data.frame(WCS = rnorm(n, 20, 8), axis = factor(rep("A", n)))
wcs$WCS[wcs$WCS < 0 | wcs$WCS > 40] <- rnorm(1, 12,4)
x <- wcs$WCS

calculate_rogers <- function(x){
    # calcualte roger groups by using quantile function
    qntl <- c(.16, .5, .84, .975, 1)
    qntl <- c(0, quantile(x, probs = qntl))
    qntl[length(qntl)] <- 40
    v <- qntl %>% diff
    names(v) <- NULL
    return(v)
}

qntl <- calculate_rogers(wcs$WCS)
roger_cat <- c('Leggards', 'Late Majority', 'Early Majority', 'Early Adopters', 'Innovators')

rogers <- data.frame(
    value = qntl,
    key = factor(roger_cat, levels = roger_cat, ordered = T))

wcs_colors <- c('#dfe1e7', '#c5d1d7', '#c0c3d0', "#a0a5b8", '#88a1ad')
theme_wcs_innovation <- theme(axis.ticks.y = element_blank(),
                              axis.text.y = element_blank(),
                              axis.title.y = element_blank(),
                              legend.position = "bottom",
                              panel.background = element_blank(),
                              legend.title = element_blank(), 
                              legend.key = element_blank(), 
                              axis.title = element_blank(),
                              axis.text.x = element_text(size = 16),
                              axis.ticks.length=unit(0.25,"cm"))

df <- data.frame(x = c(15, 15), y = c(1.2,1.8))

pos_jtr <- position_jitter(height = 0, width = .05)
performance_bar <- ggplot(data = rogers, mapping = aes(x = 1.5, y = value, fill = key)) + 
    geom_bar(stat = "identity", width = .5) +
    geom_point(data = wcs, mapping = aes(x = 1.5, y = WCS, fill = NULL), size = 2, alpha = .5, shape = 16, position = pos_jtr) + 
    coord_flip() +
    scale_fill_manual(values = wcs_colors) +
    scale_x_continuous(limits = c(1.2, 1.85)) +
    scale_y_continuous(breaks=seq(0,40,5)) +
    geom_bar(stat = "identity", width = .5, alpha = 0) +
    geom_line(data = df, mapping = aes(y = x, x = y, fill = NULL), lwd = 2) +
    geom_text(label = " 25 %", x = 1.83, y = 15) + 
    theme_wcs_innovation +
    guides(fill = guide_legend(override.aes = list(alpha = 1))) +
    ggtitle("Composite-Score-Innovation-View.tiff") +
    theme(title = element_text(face = "bold", size = 18))

performance_bar

# ggsave("composite-score-innovation-view.tiff", height = 4, width = 10)
