

library(dplyr)
library(tidyr)
library(ggplot2)
library(moments)
library(corrplot)
library(lavaan)
library(tidySEM)
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

## Model 1: simple regression ####

# Load data factors
df_f <- foreign::read.spss(
  "data/data1factors.sav", use.value.labels = FALSE, to.data.frame = TRUE,
)
var_labs <- attributes(df_f)$variable.labels


## Model 1: Valuation of work

# Model definition
model <- ' 
  # regressions
  ta_val7 ~ kj_enc1 + op_rew3 + tk_inv5 + ts_cla6
 
  # intercepts
  ta_val7 ~ 1
'

# Fit
fit <- lavaan::sem(model, data = df_f, estimator = "ML", std.lv = TRUE) #std.lv standardize to the latent vars only. std.all std.all uses all path information to determine the standardized estimates for paths 

# summary measures
fit_summary <- lavaan::summary(fit, standardized = TRUE, rsquare = TRUE)
fit_pe <- fit_summary$pe # parameters estimated

# get tidy form of parameter, to use also with graph_sem  
tidy_fit <- fit %>%
  tidySEM::get_edges() %>%
  dplyr::mutate( # remove labels to create path model
    #label = NULL,
    show = dplyr::if_else(from == to, FALSE, TRUE),
    curvature = dplyr::if_else(curvature == 60, 80, curvature)
  ) 
  
# Create path plot  
p_path <- tidySEM::graph_sem(
  tidy_fit, 
  layout = tidySEM::get_layout(
    "kj_enc1", "",
    "op_rew3", "",
    ""       , "ta_val7",
    "tk_inv5", "",
    "ts_cla6", "",
    rows = 5
  ),
  # spacing_x = 1.5,# default 1
  # spacing_y = 1.5, # default 1,
  text_size = 5.5, # default 4
  angle = 0
) 



## Model 2: Mediation effect ####
# Meidation model def: https://lavaan.ugent.be/tutorial/mediation.html

model_med <- ' 
  # direct effect
  ta_val7 ~ c*kj_enc1 + c*op_rew3 + c*ts_cla6

  # mediator
  tk_inv5 ~ a*op_rew3
  ta_val7 ~ b*tk_inv5

  # indirect effect (a*b)
  ab := a*b
  # total effect
  total := c + (a*b)
'

# Fit
fit_med <- lavaan::sem( model_med, data = df_f, estimator = "ML", std.lv = TRUE)

# summary measures
fit_summary_med <- lavaan::summary(fit_med, standardized = TRUE, rsquare = TRUE,)
fit_pe_med <- fit_summary_med$pe # parameters estimated

tidy_fit_med <- fit_med %>%
  tidySEM::get_edges() %>%
  dplyr::mutate( # remove labels to create path model
    #label = NULL,
    show = dplyr::if_else(from == to, FALSE, TRUE),
    curvature = dplyr::if_else(curvature == 60, 80, curvature)
  ) 

# Create path plot  
p_path_med <- tidySEM::graph_sem(
  tidy_fit_med, 
  layout = tidySEM::get_layout(
    "kj_enc1", "",
    "op_rew3", "",
    ""       , "ta_val7",
    "tk_inv5", "",
    "ts_cla6", "",
    rows = 5
  ),
  # spacing_x = 1.5,# default 1
  # spacing_y = 1.5, # default 1,
  text_size = 5.5, # default 4
  angle = 0
)


# The latent model ####

# model_lat <- ' 
#   
#   # measurement model (latent vars definitions)
#   kj_enc1 =~ v5 + v7 + start(1)*v17 # setting a start(1)*x1 This procedure helps the model to become identifiable and indicates which one of the observed variables is the ”number one operationalization” of the latent variable.
#   tv_bui12 =~ start(1)*v42r + v43
#   tr_psy11 =~ start(1)*v44 + v45
# 
#   # regressions
#   tr_psy11 ~ kj_enc1 + tv_bui12 
# 
#   # residual correlations (covariance)
#   kj_enc1 ~~ tv_bui12 # The two latent variables are allowed to correlate in the model. This decision is based on a theoretical assumption.
# 
#   # constraints (fix unique variances (errors) of obs var to 1)
#   # procedure from: https://groups.google.com/g/lavaan/c/fIRj-P8Y_Dc
#   
#   # Zero out default unique variances
#   v5 ~~ 0*v5
#   v7 ~~ 0*v7
#   v17 ~~ 0*v17
#   v42r ~~ 0*v42r
#   v43 ~~ 0*v43
#   v44 ~~ 0*v44
#   v45 ~~ 0*v45
#   
#   
#   # Define new latent variables for error variances
#   e5 =~ 0
#   e7 =~ 0
#   e17 =~ 0
#   e42r =~ 0
#   e43 =~ 0
#   e44 =~ 0
#   e45 =~ 0
#   
#   # error variances fixed to one
#   e5 ~~ 1*e5
#   e7 ~~ 1*e7
#   e17 ~~ 1*e17
#   e42r ~~ 1*e42r
#   e43 ~~ 1*e43
#   e44 ~~ 1*e44
#   e45 ~~ 1*e45
#   
#   # Estimate loadings for error variances
#   v5 ~ start(1)*e5
#   v7 ~ start(1)*e7
#   v17 ~ start(1)*e17
#   v42r ~ start(1)*e42r
#   v43 ~ start(1)*e43
#   v44 ~ start(1)*e44
#   v45 ~ start(1)*e45
#   
#   # Same procedure for error variance of latent tr_psy11
#   epsw =~ 0
#   epsw ~~ 1*epsw
#   tr_psy11 ~ start(1)*epsw
#   
# '


model_lat <- ' 
  
  # measurement model (latent vars definitions)
  kj_enc1 =~ v5 + v7 + 1*v17 # setting a start(1)*x1 This procedure helps the model to become identifiable and indicates which one of the observed variables is the ”number one operationalization” of the latent variable.
  tv_bui12 =~ 1*v42r + v43
  tr_psy11 =~ 1*v44 + v45

  # regressions
  tr_psy11 ~ kj_enc1 + tv_bui12 

  # residual correlations (covariance)
  kj_enc1 ~~ tv_bui12 # The two latent variables are allowed to correlate in the model. This decision is based on a theoretical assumption.

  # constraints (fix unique variances (errors) of obs var to 1)
  # procedure from: https://groups.google.com/g/lavaan/c/fIRj-P8Y_Dc
  
  # Zero out default unique variances
  v5 ~~ 0*v5
  v7 ~~ 0*v7
  v17 ~~ 0*v17
  v42r ~~ 0*v42r
  v43 ~~ 0*v43
  v44 ~~ 0*v44
  v45 ~~ 0*v45
  
  
  # Define new latent variables for error variances
  e5 =~ 0
  e7 =~ 0
  e17 =~ 0
  e42r =~ 0
  e43 =~ 0
  e44 =~ 0
  e45 =~ 0
  
  # error variances fixed to one
  e5 ~~ 1*e5
  e7 ~~ 1*e7
  e17 ~~ 1*e17
  e42r ~~ 1*e42r
  e43 ~~ 1*e43
  e44 ~~ 1*e44
  e45 ~~ 1*e45
  
  # Estimate loadings for error variances
  v5 ~ 1*e5
  v7 ~ 1*e7
  v17 ~ 1*e17
  v42r ~ 1*e42r
  v43 ~ 1*e43
  v44 ~ 1*e44
  v45 ~ 1*e45
  
  # Same procedure for error variance of latent tr_psy11
  tr_psy11 ~~ 0*tr_psy11
  epsw =~ 0
  epsw ~~ 1*epsw
  tr_psy11 ~ 1*epsw
  
'



# Fit
fit_lat <- lavaan::sem(model_lat, data = df, estimator = "ML", std.lv = TRUE)

# summary measures
fit_summary_lat <- lavaan::summary(fit_lat, standardized = TRUE, rsquare = TRUE)
fit_pe_lat <- fit_summary_lat$pe # parameters estimated

# get tidy form of parameter, to use also with graph_sem  
tidy_fit_lat <- fit_lat %>%
  tidySEM::get_edges() %>%
  dplyr::mutate( # remove labels to create path model
    label = est_std,
    show = dplyr::if_else(from == to, FALSE, TRUE),
    curvature = dplyr::if_else(curvature == 60, 80, curvature)
  ) 

# Create path plot  
p_path_lat <- tidySEM::graph_sem(
  tidy_fit_lat, 
  layout = tidySEM::get_layout(
    "e5"   ,"v5"   ,""         ,""          ,""    ,"",
    "e7"   ,"v7"   ,"kj_enc1"  ,""          ,""    ,"",
    "e17"  ,"v17"  ,""         ,"epsw"      ,"v44" ,"e44",
    ""     , ""     ,""         ,"tr_psy11"  ,""   ,"",
    "e42r" ,"v42r" ,""         ,""          ,"v45" ,"e45",
    ""     , ""     ,"tv_bui12" ,""          ,""   ,"",
    "e43"  ,"v43"  ,""         ,""          ,""    ,"",
    rows = 7
  ),
  spacing_x = 1.75,# default 1
  spacing_y = 1.75, # default 1,
  text_size = 5.5, # default 4
  angle = 0
)







rmarkdown::render("R/report_script.rmd")

