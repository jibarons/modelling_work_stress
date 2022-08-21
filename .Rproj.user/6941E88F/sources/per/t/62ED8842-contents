

library(dplyr)
library(tidyr)
library(ggplot2)
library(moments)
library(corrplot)
library(lavaab)

# Report libraries
library(knitr)
library(kableExtra)
library(rmarkdown)


# Evaluating assumptions ####

# Load data variables
df <- foreign::read.spss(
  "data/data1.sav", use.value.labels = FALSE, to.data.frame = TRUE
)

## Explore Univariate distributions ####

# Although data is ordinal and thus non-normal by definition, we check for
# normality assumption. We will apply z rescaling to see how they approximate to 
# mean 0 and sd 1. Will also check histograms and kurtosis and skewnewss. We 
# reject applying saphiro test, as being only 5 poitn scale ordinal is unlikely will
# provide any meaningfull result


# Z-tranform
df_z <- sapply(
  df, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
) %>% as.data.frame()

# Check histogram
df_z_long <- df_z %>% 
  tidyr::pivot_longer(dplyr::everything(), names_to = "variable")

p_hist <- ggplot2::ggplot(df_z_long, ggplot2::aes(value)) + 
  ggplot2::geom_histogram(bins = 7) +
  ggplot2::geom_vline(
    ggplot2::aes(xintercept = mean(value, na.rm = TRUE)),
    color = "red", linetype = "dashed", size = 0.75
  ) +
  ggplot2::facet_wrap(~ variable, ncol = 5) + 
  ggplot2::theme_bw() + 
  ggplot2::theme(
    text = ggplot2::element_text(size = 10)
  )

# Compute descriptive stats to check for normality
norm_stats <- df_z %>%
  dplyr::summarise(dplyr::across(
    dplyr::everything(), 
    list(
      skewness = ~ round(moments::skewness(., na.rm = TRUE), 3),
      kurtosis = ~ round(moments::kurtosis(., na.rm = TRUE), 3)
    )
  )) %>%
  tidyr::pivot_longer(
    dplyr::everything(), names_to = c("variable", ".value"), names_sep = "_"
  )

# Check best performing vars as average of skewness and kurtisis - 3, assuming 
# skewness 0 and kutosis 3 is a normal distribution.

norm_stats$kurtosis_excess <-  norm_stats$kurtosis - 3
norm_stats$avg_skew_kurt <- rowMeans(
  abs(norm_stats[, c("skewness", "kurtosis_excess")])
)


## Explore bivariate distributions ####

# Cretae fun for scatter plots
p_scatter <- function(x, y) {
  ggplot2::ggplot(df, ggplot2::aes(x = {{x}}, y = {{y}})) +
    ggplot2::geom_jitter(width = 0.5, size = 2, alpha = 0.5) +
    ggplot2::geom_smooth(method = "lm", color = "red") +
    ggplot2::theme_bw() +
    ggplot2::labs(caption = "Jitter added at width 0.5. Tendency line base on linear regresion method (lm)")
}

# Two assumed normally distributed
p_norm2 <- p_scatter(v3, v4)
# One assumed normally distributed
p_norm1 <- p_scatter(v4, v16)
# None assumed normally distributed
p_norm0 <- p_scatter(v16, v47)

## Explore correlation matrix ####

# Correlation Structure
mtx_cor <- cor(df, use = "complete.obs")
mtx_cor[upper.tri(mtx_cor, diag = TRUE)] <- NA

# Maximum correlation
max_r <- max(mtx_cor, na.rm = TRUE)
max_r_vars <- which(mtx_cor == max_r, arr.ind = TRUE)
max_r_vars <- paste0(
  rownames(mtx_cor)[max_r_vars[1]], ", ",
  rownames(mtx_cor)[max_r_vars[2]]
)
max_r2 <- summary(lm(v18 ~ v17, data = df))$r.squared

# Mminumum correlation
min_r <- min(mtx_cor, na.rm = TRUE)
min_r_vars <- which(mtx_cor == min_r, arr.ind = TRUE)
min_r_vars <- paste0(
  rownames(mtx_cor)[min_r_vars[1]], ", ",
  rownames(mtx_cor)[min_r_vars[2]]
)
min_r2 <- summary(lm(v45 ~ v7, data = df))$r.squared


corrplot.mixed(
  cor(df, use = "complete.obs"), 
  lower = 'number', upper = 'square', 
  tl.cex = 0.75, number.cex = 0.5, number.digits = 1
)


# Path Analysys ####

# Load data factors
df_f <- foreign::read.spss(
  "data/data1factors.sav", use.value.labels = FALSE, to.data.frame = TRUE,
)
attributes(df_f)$variable.labels

# Model definition
model <- 'ta_val7 ~ kj_enc1 + op_rew3 + tk_inv5 + ts_cla6'
# Fit
fit <- lavaan::sem(model, data = df_f)
# summary measures
lavaan::summary(fit, standardized = FALSE, rsquare = TRUE) # lavaan Rsquare = Square Multiple Correlation (https://paolotoffanin.wordpress.com/2018/06/30/beginning-with-sem-in-lavaan-iii/)

lavaan::parameterestimates(fit, standardized = TRUE)


tidySEM::graph_sem(model = fit)
lavaanPlot::lavaanPlot(model = fit, coefs = TRUE)
semPlot::semPaths(
  fit, what = "std", 
  residuals = FALSE, intercepts = FALSE, 
  style = "ram"
)



rmarkdown::render("R/report_script.rmd")

