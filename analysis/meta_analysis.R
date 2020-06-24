# performs meta-analysis of present study and Hardwicke et al. (2018)
# the code below will perform two meta-analyses for the article-level reproducibility outcomes.
# one meta-analysis is for reproducibility outcomes before contacting authors
# the other meta-analysis is for reproducibility outcomes after contacting authors
# we will synthesize data from Hardwicke et al. (2018) with data from the present study
# because both studies had similar goals and designs.

# prepare Hardwicke et al. data

## get the total number of articles
hardwicke2018_totalN <- data_hardwicke2018 %>%
  nrow()

## get the number of successful cases prior to author assistance
hardwicke2018_preAuthorSuccess_n <- data_hardwicke2018 %>%
  count(outcome) %>%
  filter(outcome == "Success") %>%
  pull(n)

## get the number of successful cases after author assistance
hardwicke2018_postAuthorSuccess_n <- data_hardwicke2018 %>%
  count(outcome) %>%
  filter(outcome %in% c("Success", "Success with author assistance")) %>%
  pull(n) %>%
  sum()

# prepare present study data

## get the total number of articles
presentStudy_totalN <- articleLevelSummary %>%
  pull(n) %>%
  sum()

## get the number of successful cases prior to author assistance
presentStudy_preAuthorSuccess_n <- articleLevelSummary %>% 
  filter(finalOutcome == "Reproducible\nwithout author\ninvolvement") %>%
  pull(n)

## get the number of successful cases after author assistance
presentStudy_postAuthorSuccess_n <- articleLevelSummary %>% 
  filter(finalOutcome %in% 
           c("Reproducible\nwithout author\ninvolvement",
             "Reproducible\nwith author\ninvolvement")) %>%
  pull(n) %>%
  sum()

# combine the data from the two studies into data frames...

## ...one for pre-author assistance
ma_data_pre <- data.frame(
  type = c("Before author contact", "Before author contact"),
  study = c("Hardwicke et al. (2018)", "Present study"),
  cases = c(hardwicke2018_preAuthorSuccess_n, presentStudy_preAuthorSuccess_n), 
  total = c(hardwicke2018_totalN,presentStudy_totalN))

## ...one for post-author assistance
ma_data_post <- data.frame(
  type = c("After author contact", "After author contact"),
  study = c("Hardwicke et al. (2018)", "Present study"),
  cases = c(hardwicke2018_postAuthorSuccess_n, presentStudy_postAuthorSuccess_n), 
  total = c(hardwicke2018_totalN, presentStudy_totalN))

## ...and one for both together
ma_data_combined <- rbind(ma_data_pre, ma_data_post)

## run meta-analysis
m <- metaprop(
  data = ma_data_combined,
  event = cases, 
  n = total,
  studlab = study,
  method.ci = 'WSCC', # WSCC
  comb.fixed = F,
  comb.random = T,
  sm = "PRAW", # specify summary measure as raw proportions
  method = "Inverse",
  byvar = type,
  overall = F, # turn off overall summary statistic
  overall.hetstat = F, # turn of heterogeneity statistics
  print.byvar = F
)


## build forest plot

pdf(file="forestPlot.pdf", width = 12, height = 6)

forestPlot <- forest(m, 
       leftlabs = c("Study", "Reproducible\ncases", "Total\ncases"),
       rightlabs = c("Proportion", "[95% CI]"),
       xlab = 'Proportion',
       xlim = c(0,0.8),
       col.square = 'black',
       col.diamond = 'black',
       col.by = 'black',
       hetstat = F, # turn off heterogeneity stats
       spacing = 1.25,
       fs.heading = 12.5,
       fontsize = 11,
       weight.study = 'random',
       weight.subgroup = 'weight',
       colgap.forest.left = unit(0.5, 'cm')
)

dev.off()

# OLD CODE using metafor package

# conduct meta-analysis

## compute effect sizes 

# ### we use "PR" because we are not transforming the data (i.e., we are using raw proportions)
# effect_sizes_pre <- escalc(xi=cases, ni=total, data=ma_data_pre, measure="PR")
# effect_sizes_post <- escalc(xi=cases, ni=total, data=ma_data_post, measure="PR")
# effect_sizes_combined <- escalc(xi=cases, ni=total, data=ma_data_combined, measure="PR")
# 
# ##  perform data synthesis
# ## we are using a random effects model, specifically the REML (restricted maximum likelihood estimator) method
# 
# ma_model_pre <- rma(yi, vi, data=effect_sizes_pre, method="REML") 
# ma_model_post <- rma(yi, vi, data=effect_sizes_post, method="REML") 

## build forest plot
## we first plot all of the study effects and CIs. 
## then we plot the separate random effects models for before and after author contact.

#pdf(file = 'manuscript/figs/forestPlot.pdf') 

### forest plot of all studies
# forestPlot <- forest(effect_sizes_combined$yi, # plot study effects
#                      effect_sizes_combined$vi, # plot study CIs
#                      slab = ma_data_combined$study, # plot study labels
#                      xlim=c(-0.5, 1.25), # set horizontal limits
#                      ylim=c(-0.5,11), # set vertical limits
#                      rows=c(6:7, 1:2), # specify rows in which effects are plotted
#                      refline = NULL, # do not show reference line
#                      header= c("Study", "Proportion reproducible [95% CI]")) # show header
# 
# ### add summary estimate to the bottom
# forestPlot <- forestPlot + addpoly(ma_model_pre, row=5, mlab="RE Model") # plot the pre- summary model
# forestPlot <- forestPlot + addpoly(ma_model_post, row=0, mlab="RE Model") # plot the post- summary model
# 
# ### plot subheadings
# par(font=4) # set text style to bold for the below
# text(-0.5, c(3,8), pos=4, # specify position of text
#      c("After contacting authors", # specify subheadings
#        "Before contacting authors"))
# 
# #dev.off() 
# 
