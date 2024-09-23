## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning=FALSE
)

## ----setup--------------------------------------------------------------------
# Import packages
library(maxent.ot)

## ---- echo=FALSE, fig.cap="Fig 1: Two OT tableaux.", out.width = '40%'--------
knitr::include_graphics("../man/figures/OT_tableau_simple.jpg")

## ---- echo=FALSE, fig.cap="Fig 2: Two MaxEnt tableaux.", out.width = '85%'----
knitr::include_graphics("../man/figures/maxEnt_tableau.jpg")

## ---- echo=FALSE, fig.cap="Fig 3: How MaxEnt score varies with penalty.", out.width = '85%'----
knitr::include_graphics("../man/figures/MaxEnt_vs_penalty.jpg")

## ---- include=FALSE-----------------------------------------------------------
# This creates the .jpg image for the graph in the chunk above.
# Ideally, use this (i.e. create graph rather than use an image)...
# ...but this currently renders small plot & ugly big spaces for labels

# Hack to set up window
curve(exp(-x), from=0, to=5,
      col="white", xlab="penalty", ylab="MaxEnt score")    

# Add horizontal & vertical lines at y=0 & x=0
abline(h=0, col="gray")
abline(v=0, col="gray")

# Extend curve to to R-edge of window
curve(exp(-x), from=0, to=5.25, col="blue", add=TRUE)      

## -----------------------------------------------------------------------------
# Get paths to sample data file
data_file <- system.file(
  "extdata", "sample_data_frame.csv", package = "maxent.ot"
)

# Take a look at the structure of the sample data
data_table <- read.table(data_file, header=FALSE)
data_table

## -----------------------------------------------------------------------------
data_frame_file <- system.file(
  "extdata", "sample_data_frame.csv", package = "maxent.ot"
)

data_frame <- read.csv(data_frame_file)
data_frame

## ----dataForSimpleCase--------------------------------------------------------
# Fit weights to data using file path
simple_model <- optimize_weights(data_frame)

## -----------------------------------------------------------------------------
simple_model <- optimize_weights(data_frame)

## -----------------------------------------------------------------------------
# View the model we've fit (no biases used)
simple_model

## ----simpleModelWeight--------------------------------------------------------
# Get learned weights of model we've fit (no biases used)
simple_model$weights

## ----simpleModelLL------------------------------------------------------------
# Get log likelihood of model we've fit (no biases used)
simple_model$loglik

## ---- echo=FALSE, fig.cap="Table 1: Log likelihood of M1's & M2's weights.", out.width = '85%'----
knitr::include_graphics("../man/figures/logLike_table_2.jpg")

## -----------------------------------------------------------------------------
# Get paths to toy data and bias files.
data_frame_file <- system.file(
  "extdata", "sample_data_frame.csv", package = "maxent.ot"
)
bias_file <- system.file(
  "extdata", "sample_bias_data_frame.csv", package = "maxent.ot"
)

# Take a look at the structure of the bias_file
data_frame <- read.csv(data_frame_file) 
bias_data_frame <- read.csv(bias_file)
bias_data_frame

## -----------------------------------------------------------------------------
# Fit weights with biases specified in file
optimize_weights(data_frame, bias_data_frame)

## -----------------------------------------------------------------------------
# Fit weights with biases specified as scalars
optimize_weights(
  data_frame, mu = 0, sigma = 1000
)

## -----------------------------------------------------------------------------
# Fit weights with biases specified in vector form
optimize_weights(
  data_frame, mu = c(1, 2), sigma = c(100, 200)
)

## -----------------------------------------------------------------------------
# Fit weights with a mix of scalar and vector biases
optimize_weights(
  data_frame, mu = c(1, 2), sigma = 1000
)

## -----------------------------------------------------------------------------
# Get log likelihood of simple model (no biases)
simple_model$loglik

## -----------------------------------------------------------------------------
# View the data that was used to train `simple_model`
read.table(data_file, header=FALSE, sep='\t')

## ----scalarBiases-------------------------------------------------------------
# Train regularized model with mu=0 & sigma=1
regularized_model <- optimize_weights(
  data_frame, mu=0, sigma=1
)

## -----------------------------------------------------------------------------
# Take a took at the regularized model trained with scalar biases
regularized_model

## -----------------------------------------------------------------------------
# View the bias parameters we used during model training
regularized_model$bias_params

## ----biasModelAttributes------------------------------------------------------
# Get learned weights for the regularized model trained with scalar biases
regularized_model$weights

## ----biasModelObjFn-----------------------------------------------------------
# Get value of objective function for trained regularized model
regularized_model$loglik

## -----------------------------------------------------------------------------
# Get weight of Constraint 1 (simple model)
cons1_noBias <- simple_model$weights[1]

# Get weight of Constraint 1 (regularized model)
cons1_simpleBias <- regularized_model$weights[1]

# Compare learned weights of Constraint 1 in the unbiased model...
# ...and in the regularized one
print(paste("In the unbiased model, Constraint 1's  weight is:",
            sprintf(cons1_noBias, fmt = '%#.3f')))
print(paste("In the regularized model, Constraint 1's  weight is:",
            sprintf(cons1_simpleBias, fmt = '%#.3f')))

## ----echo=FALSE---------------------------------------------------------------
# Create matrix with 3 columns
tab <- matrix(c(95, 90, 73, 20), ncol=2, byrow=TRUE)

# Define column names and row names of matrix
colnames(tab) <- c('Expt cdn', 'Control cdn')
rownames(tab) <- c('trained', 'untrained')

# Convert matrix to table
tab <- as.table(tab)

# View table
tab

## -----------------------------------------------------------------------------
# Get path to data file (experimental condition)
white_salt_data_file <- system.file(
  "extdata", "sample_whiteSalt_data_file.txt", package = "maxent.ot"
)

# File is in OTSoft format, so first convert it to data frame
white_salt_data_frame <- otsoft_tableaux_to_df(white_salt_data_file)

# View training data (experimental condition)
white_salt_data_frame

## -----------------------------------------------------------------------------
# Fit model with preferred weights for experimental condition
white_salt_bias_model = optimize_weights(
  white_salt_data_frame, mu=c(0, 0, 1.3, 3.65), sigma=sqrt(0.6)
)

# View trained weights (expt cdn with bias)
white_salt_bias_model$weights

## -----------------------------------------------------------------------------
# Fit unbiased model for experimental condition
white_salt_noBias_model = optimize_weights(
  white_salt_data_frame, 
  mu=c(0, 0, 2.27, 2.27), sigma=sqrt(0.6)
)

# View trained weights (expt cdn, no bias)
white_salt_noBias_model$weights

## -----------------------------------------------------------------------------
# Get path to test file (experimental condition)
white_salt_test_file <- system.file(
  "extdata", "sample_whiteSalt_test_file.txt", package = "maxent.ot"
)

# File is in OTSoft format, so convert first
white_salt_test_df <- otsoft_tableaux_to_df(white_salt_data_file)

# View test data (experimental condition)
white_salt_test_df

# Predict probabilities with weights trained with bias (expt cdn)
predict_probabilities(
  white_salt_test_df, white_salt_bias_model$weights
)

# Predict probabilities with weights trained without bias (expt cdn)
predict_probabilities(
  white_salt_test_df, white_salt_noBias_model$weights
)

## ----echo=FALSE---------------------------------------------------------------
# Create matrix with 3 columns
tab <- matrix(c(91, 93, 78, 45), ncol=2, byrow=TRUE)

# Define column names and row names of matrix
colnames(tab) <- c('Biased model', 'Unbiased model')
rownames(tab) <- c('Trained (95%)', 'Untrained (73%)')

# Convert matrix to table
tab <- as.table(tab)

# View table
print("Predicted % of alternation during response phase (experimental condition)")
tab

## -----------------------------------------------------------------------------
# Get path to data file (control condition)
white_control_data_file <- system.file(
  "extdata", "sample_whiteCtrl_data_file.txt", package = "maxent.ot"
)

white_control_df <- otsoft_tableaux_to_df(white_control_data_file)

# View training data (control condition)
white_control_df

# Fit model with preferred weights for control condition
white_control_bias_model = optimize_weights(
  white_control_df,
  mu=c(0, 0, 1.3, 3.65), sigma=sqrt(0.6)
)

# View trained weights (control cdn with bias)
white_control_bias_model$weights

# Fit unbiased model for control condition
white_control_noBias_model = optimize_weights(
  white_control_df,
  mu=c(0, 0, 2.27, 2.27), sigma=sqrt(0.6)
)

# View trained weights (control cdn, no bias)
white_control_noBias_model$weights

## -----------------------------------------------------------------------------
# Get path to test file (control condition)
white_control_test_file <- system.file(
  "extdata", "sample_whiteCtrl_test_file.txt", package = "maxent.ot"
)

# File is in OTSoft format, so convert first
white_control_test_df <- otsoft_tableaux_to_df(white_control_test_file)

# View test data (experimental condition)
white_control_test_df

# View test data (control condition)
read.table(white_control_test_file, header=FALSE, sep='\t')

# Predict probabilities with weights trained with bias (control cdn)
predict_probabilities(
  white_control_test_df, white_control_bias_model$weights
)

# Predict probabilities with weights trained with bias (control cdn)
predict_probabilities(
  white_control_test_df, white_control_noBias_model$weights
)

## ----echo=FALSE---------------------------------------------------------------
# Create matrix with 3 columns
tab <- matrix(c(87, 87, 26, 61), ncol=2, byrow=TRUE)

# Define column names and row names of matrix
colnames(tab) <- c('Biased model', 'Unbiased model')
rownames(tab) <- c('Trained (90%)', 'Untrained (20%)')

# Convert matrix to table
tab <- as.table(tab)

# View table
print("Predicted % of alternation during response phase (control condition)")
tab

## -----------------------------------------------------------------------------
# Get paths to toy data file
data_file_a <- system.file(
  "extdata", "sample_data_frame.csv", package="maxent.ot"
)

df_a <- read.csv(data_file_a)

# Fit weights to data (no biases)
fit_model_a <- optimize_weights(df_a)

# Predict probabilities for the same input 
predict_probabilities(df_a, fit_model_a$weights)

## -----------------------------------------------------------------------------
# Data has repeated URs (absolute frequency)
# Get paths to toy data file
data_file_b <- system.file(
  "extdata", "sample_data_file_double_aFreq.txt", package="maxent.ot"
)

df_b <- otsoft_tableaux_to_df(data_file_b)

# Take a look at the structure of the sample data with duplicate URs
df_b

# Here's the structure of the same data without duplicate URs
df_a

## -----------------------------------------------------------------------------
# Fit weights to data (no biases)
fit_model_b <- optimize_weights(df_b)

# Predict probabilities for the same input (duplicate URs)
predict_probabilities(df_b, fit_model_b$weights)

## -----------------------------------------------------------------------------
# Get predictions with User-chosen constraint weights
# Make a list of weights
# Be sure to order the weights in exactly the same order...
# ...in which their respective constraints appear in the input file!
my_wt_ls <- list(1.5, 2.5)

# Convert to double object
my_wts <- as.double(my_wt_ls)

## -----------------------------------------------------------------------------
# Get predictions
predict_probabilities(df_a, my_wts)

## -----------------------------------------------------------------------------
# Save predicted result to file
# predict_probabilities(
#   data_file_a, my_wts,
#   output_path = "C:\\path\\to\\dir\\your_file_name.txt",
#   out_sep = "\t"
# )

## -----------------------------------------------------------------------------
# Get paths to toy data file
data_file_c <- system.file(
   "extdata", "sample_data_file_2.txt", package="maxent.ot"
)

# Get predictions T=1
# We'll let temperature default to 1
t1_pred <- predict_probabilities(data_file_c, my_wts)

# Get predictions T=3
t3_pred <- predict_probabilities(data_file_c, my_wts, temperature=3)

# Get predictions T=5
t5_pred <- predict_probabilities(data_file_c, my_wts, temperature=5)

## -----------------------------------------------------------------------------
# View predicted probability of t1_pred
t1_pred$predictions[, 'Predicted']

## -----------------------------------------------------------------------------
# Select columns we want, and store them in lettered variables
a <- t1_pred$predictions[, 'Input']
b <- t1_pred$predictions[, 'Output']
c <- t1_pred$predictions[, 'Predicted']
d <- t3_pred$predictions[, 'Predicted']
e <- t5_pred$predictions[, 'Predicted']

# Join variables to create data frame
temperature_df <- data.frame(a, b, c, d, e)

# Rename columns
names(temperature_df) <- c('Input', 'Output', 'T=1', 'T=3', 'T=5')

# View the data frame
temperature_df

## ----echo=FALSE---------------------------------------------------------------
# Create matrix with 3 columns
tab <- matrix(c(73, 27, 58, 42, 55, 45), ncol=2, byrow=TRUE)

# Define column names and row names of matrix
colnames(tab) <- c('Output1-1 (%)', 'Output1-2 (%)')
rownames(tab) <- c('T=1', 'T=3', 'T-5')

# Convert matrix to table
tab <- as.table(tab)

# View table
tab

## ---- echo=FALSE--------------------------------------------------------------
# Create separate data frames for Input1 and Input2
ur1_df <- temperature_df[1:2, 3:5]
ur2_df <- temperature_df[3:5, 3:5]

# save old par
oldpar <- par(mar=c(5, 4, 4, 4), xpd=TRUE)

# Plot distribution of outputs for Input1
barplot(as.matrix(ur1_df),
        main = "Input 1",
        xlab = "Temperature",
        ylab = "% output",
        axes = TRUE,
        legend.text = c("Out1-1", "Out1-2"),
        args.legend = list(x = "topright",
                           inset = c(-0.55, 0),    # Move legend outside plot
                           cex = .7)               # Legend font size
        )

# Plot distribution of outputs for Input2
par(mar=c(5, 4, 4, 4), xpd=TRUE)
barplot(as.matrix(ur2_df),
        main = "Input 2",
        xlab = "Temperature",
        ylab = "% output",
        axes = TRUE,
        legend.text = c("Out2-1", "Out2-2", "Out2-3"),
        args.legend = list(x = "topright",
                           inset = c(-0.55, 0),
                           cex = .7)
        )

# Reset par
par(oldpar)

## -----------------------------------------------------------------------------
# Get paths to sample Hungarian wug data
hu_file <- system.file(
  "extdata", "sample_hu_wug.txt", package = "maxent.ot"
)

hu_data <- otsoft_tableaux_to_df(hu_file)

# We'll use the weights learned by training on the lexicon as reported by HZSL on (p.848).
lex_wts_ls <- list(5.39, 1.69, 1.46, 3.00, 4.04, 2.45, 1.08, .91, 1.73, 2.42)

# Make a named list (optional)
names(lex_wts_ls) <- list(
  'AGREE(back,nonlocal)',	'AGREE(front,local)',
  'AGREE(nonhigh_front,local)',	'AGREE(low_front,local)',
  'AGREE(double_front,local)',	'USE_FRONT/bilabial__',
  'USE_FRONT/[+cor,+son]__', 'USE_FRONT/sibilant__',
  'USE_FRONT/CC__',	'USE_BACK/[C0i:C0]__'
)

# Convert to double object (required type as argument of predict_probabilities())
lex_wts <- as.double(lex_wts_ls)

# Predict probabilities with temperature set to 1.5
# Notice also that "UTF-8" is an option for encoding, which comes in useful here
hu_pred <- predict_probabilities(
  hu_data, lex_wts, temperature = 1.5, encoding = "UTF-8"
)

## -----------------------------------------------------------------------------
# Let's view some of the predicted probabilities at T=1.5
# We've dropped the constraint violations for easier viewing
# Both predicted and observed probability are conditioned over URs
# e.g. Observed probability in rows 3 & 4 don't sum to 1
# because the stem [étt] appears twice in this data set.
head(hu_pred$predictions[, -4:-(ncol(hu_pred$predictions)-3)])

# For curiosity's sake, let's compare:
# With default temperature=1...
# ... predicted probability is more polarized
hu_pred_t1 <- predict_probabilities(
  hu_data, lex_wts, encoding = "UTF-8"
)
head(hu_pred_t1$predictions[, -4:-(ncol(hu_pred_t1$predictions)-3)])

## -----------------------------------------------------------------------------
# Learn weights for 500 simulated wug tests
# monte_carlo_weights(hu_pred, 500)

## -----------------------------------------------------------------------------
# Get path to the saved constraint weights trained on the 500 simulated wug tests
hu_500simul_wts_path <- system.file(
  "extdata", "hu_500simuls_wts.txt", package = "maxent.ot"
)

# Store constraint weights in object `hu_500simul_wts`
hu_500simul_wts <- read.table(hu_500simul_wts_path, header=TRUE, sep='\t')
hu_500simul_wts <- data.frame(hu_500simul_wts)

# Rename columns
names(hu_500simul_wts) <- c(
  'AGREE(back,nonlocal)',	'AGREE(front,local)',
  'AGREE(nonhigh_front,local)',	'AGREE(low_front,local)',
  'AGREE(double_front,local)',	'USE_FRONT/bilabial__',
  'USE_FRONT/[+cor,+son]__', 'USE_FRONT/sibilant__',
  'USE_FRONT/CC__',	'USE_BACK/[C0i:C0]__'
)

# View the first 6 rows of hu_500simul_wts
# i.e. the first 6 Grammar S's
head(hu_500simul_wts)

## -----------------------------------------------------------------------------
# Learn weights for 5 simulated wug tests
#hu_simul_wts <- monte_carlo_weights(hu_pred, 5)
#hu_simul_wts

## -----------------------------------------------------------------------------
# Fit model for human wug response
human_wug_model <- optimize_weights(hu_data)

## -----------------------------------------------------------------------------
# Weights trained on human wug response
human_wt <- human_wug_model$weights

# Average idealized wug weights
ideal_wt <- colMeans(hu_500simul_wts)

# Create data frame
wt_df <- data.frame(human_wt, ideal_wt)

# View weights trained on human wug responses & average idealized wug weights
print(wt_df, digits = 3)

## -----------------------------------------------------------------------------
# Plot weights for the five unnatural constraints by looping through them
# For i-th constraint

for (i in 6:10) {
  hist(hu_500simul_wts[[i]], breaks=15, xlab="weight", main = colnames(hu_500simul_wts[i]))
  abline(v=human_wug_model$weights[i], col="red", lwd=3, lty=2)
}

## -----------------------------------------------------------------------------
# Get paths to toy data files
# This file has two constraints
small_file <- system.file(
  "extdata", "sample_data_file_small.txt", package = "maxent.ot"
)
small_df <- otsoft_tableaux_to_df(small_file)

# This file has three constraints
large_file <- system.file(
  "extdata", "sample_data_file_large_otsoft.txt", package = "maxent.ot"
)
large_df <- otsoft_tableaux_to_df(large_file)

# View small_data
small_df

# View large_data
large_df

## -----------------------------------------------------------------------------
# Fit weights to both data sets
small_model <- optimize_weights(small_df)
large_model <- optimize_weights(large_df)

## -----------------------------------------------------------------------------
# Compare models using the likelihood ratio test
compare_models(small_model, large_model, method='lrt')

## -----------------------------------------------------------------------------
# Get paths to toy data files
# This file has three constraints
large_file_b <- system.file(
  "extdata", "sample_data_file_large_b.txt", package = "maxent.ot"
)
large_df_b <- otsoft_tableaux_to_df(large_file_b)

# View small_data
small_df

# View large_data_b
large_df_b

# Fit weights to data set large_data_b 
large_model_b <- optimize_weights(large_df_b)

## ---- error=TRUE--------------------------------------------------------------
# Compare models using the likelihood ratio test
compare_models(small_model, large_model_b, method='lrt')

## -----------------------------------------------------------------------------
# Compare models using AIC
compare_models(small_model, large_model, method='aic')

## ---- echo=FALSE, fig.cap="Fig 5: Calculating AIC weights: Intermediate steps", out.width = '70%'----
knitr::include_graphics("../man/figures/aicWeight_table.jpg")

## -----------------------------------------------------------------------------
# Compare models using BIC
compare_models(small_model, large_model, method='bic')

## -----------------------------------------------------------------------------
# Get paths to toy data files
# This file has 2 constraints
small_file_c <- system.file(
  "extdata", "sample_data_file_small_c.txt", package = "maxent.ot"
)
small_df_c <- otsoft_tableaux_to_df(small_file_c)

# This file has 3 constraints
large_file_c <- system.file(
  "extdata", "sample_data_file_large_c.txt", package = "maxent.ot"
)
large_df_c <- otsoft_tableaux_to_df(large_file_c)

# Fit weights to both data sets 
small_model_c <- optimize_weights(small_df_c) 
large_model_c <- optimize_weights(large_df_c)

# Compare models using AIC-C
compare_models(small_model_c, large_model_c, method='aic_c')

## -----------------------------------------------------------------------------
# Problematic data set: all models have n-k <= 1

# Compare models using AIC-C
compare_models(small_model, large_model, method='aic_c')

## -----------------------------------------------------------------------------
# View problematic data set
# sample size: n = 3
# maximum number of constraints for valid aic_c score: 3-2 = 1
# actual maximum number of constraints used: 3
large_df

# View non-problematic data set
# sample size: n = 6
# maximum number of constraints for valid aic_c score: 6-2 = 4
# actual maximum number of constraints used: 3
large_df_c

## -----------------------------------------------------------------------------
# 1 Problematic data set: large_data_d has n-k <= 1
# 2 Non-problematic data sets

# sample size: n = 5
# maximum number of constraints for valid aic_c score: 5-2 = 3

# Get paths to toy data files
# This file has 2 constraints: Good
small_file_d <- system.file(
  "extdata", "sample_data_file_small_d.txt", package = "maxent.ot"
)
small_df_d <- otsoft_tableaux_to_df(small_file_d)

# This file has 3 constraints: Good
med_file_d <- system.file(
  "extdata", "sample_data_file_med_d.txt", package = "maxent.ot"
)
med_df_d <- otsoft_tableaux_to_df(med_file_d)

# This file has 4 constraints: Problematic
large_file_d <- system.file(
  "extdata", "sample_data_file_large_d.txt", package = "maxent.ot"
)
large_df_d <- otsoft_tableaux_to_df(large_file_d)

# Fit weights for all data sets 
small_model_d <- optimize_weights(small_df_d)
med_model_d <- optimize_weights(med_df_d)
large_model_d <- optimize_weights(large_df_d)

# Compare models using AIC-C
compare_models(small_model_d, med_model_d, large_model_d, method='aic_c')

