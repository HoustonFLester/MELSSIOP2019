#### Description: Cross-sectional Data Example
#### Authors: Anonymous
#### Installing the latest versions of the packages as of 1/14/2019. The sessionInfo() code will present 
####  the exact version information.
######################################################################

# Note that lines ending in four dashes (or more) is used to demark sections
# Setup ---------------

  ## Install packages
      # install.packages("brms")
      # install.packages("mvtnorm")
      # install.packages("lme4")
      # install.packages("devtools")
      # install.packages("ggplot2")
      # install.packages("processx")
      # install.packages("bayesplot")
      # devtools::install_github("mvuorre/brmstools")

  ## Loads the packages
      library(brms)
      library(mvtnorm)
      library(lme4)
      library(bayesplot)
      library(ggplot2)
      library(brmstools)
      sessionInfo() #Presents version information 

# Cross-sectional data generation, GenSetup1 ---------------
  ## Level 2 information - Allowing for unequal work group sizes

      set.seed(1919) # This is making the random data generation reproducible. You should get the same mean, min, and max described below. 
      
    ### Generating the level two work group sizes. Maximum is 10 minimum is 2 with a
    ### mean of 6.732. You should match this mean. 
      Lev2N <- 250
      with_sample_cross <- rbinom(n = Lev2N, prob = 0.7, size = 10)
      sum(with_sample_cross) ####Total number of people (followers) = 1735
      
      mean(with_sample_cross) # 6.94
      min(with_sample_cross) # 3
      max(with_sample_cross) # 10

    ### Creating a group id variable to correspond to the number of individuals on each team. 
      group_id <- rep(1:Lev2N, times = with_sample_cross)
  
  # Cross-sectional data generation location side, GenSetup2 ---------------
    ## Setting the parameter values to generate the data    
    ### Location side 
      #### All exogenous predictor variables were generated to have a total variance of one and are assumed to
      #### be uncorrelated with one another. This was done so the effect sizes (correlations)
      #### from the meta-analysis can be best incorporated into the simulation. 
      #### Similarty (i.e., a Level 1 predictor) is generated to have 20% of its variability between groups
      #### and 80% of it within group. The correlation was converted into an r squared and it is assumed
      #### that the proportion of variability that similarity explains is the same at both levels (i.e., .5^2).
      #### Transformational leadership is assumed to explain an additional 53.29% of the variability in 
      #### between group LMX. The cross-level interaction was specified in such a way that the effect 
      #### for level-one similarity is 0 for those in a group with a highly transformational leader 
      #### (score of 2 standard deviations above the mean). Overall Level 1 variability is 1. Overall between variability is .25
      #### for an ICC of .25/(1.25) = .2. However, it is important to note that this ICC is for a 
      #### group that is in a group with an average similarity, an individual with that particuar similarity value, 
      #### and an average level of transformational leadership. In other words, when all predictor variables are zero,
      #### because there is also the scale side of the model. 
   
          similar_L1_var <- .8
          similar_L2_var <- .2
          trans_tot_var <- 1
          
          gamma_00 = 0 #Location intercept
          gamma_10 = sqrt(5)/4 #Location - simple main effect for similarity within-group effect (value converted from meta-analysis)
          gamma_01 = sqrt(5)/4 #Location main effect for Level-two similarity (value converted from meta-analysis)
          gamma_02 = sqrt(.73^2/4) #Location transformational leadership simple main effect
          gamma_11 = -gamma_10/2 #Location interaction term which is created to "shut off" the similarity level one effect for those with high transformational leadership 
          
          avg_res_L1var = 1 - gamma_10^2*similar_L1_var #Explains 25% of the level 1 variance
          res_RIvar = .25 - gamma_01^2*similar_L2_var - gamma_02^2*trans_tot_var #Explains 78.29% of random intercept variance
          res_RSvar = .125 - (-gamma_10/2)^2 #Explains 62.5% of the random slope variance.
          
    # LMX data generation scale side, GenSetup3 ---------------            
      #### There is not much to go on as far as prior research regarding these data generating values.
      #### Thus, they were chosen to align with our hypothetical substantive scenario and to provide
      #### an unconditional ICC that is aligned with what is typically found in prior research
      #### (i.e., somewhere between .15 and .30, Mathieu, J. E., Aguinis, H., Culpepper, S. A., & Chen, 2012)
  
          tau_00 = log(sqrt(avg_res_L1var)) #scale fixed intercept log(sqrt(avg_resid_VAR))
          tau_10 = -.15 #scale - fixed Level-one similarity simple main effect 
          tau_01 = -.15 #scale main effect for Level-two similarity (value converted from meta-analysis)
          tau_02 = -.10 #scale transformative leadership simple main effect
          tau_11 = .075 #Scale interaction term which is created to "shut off" the similarity level one effect for those with high transformational leadership 
          
          res_RIvar_sigma <- .01
          res_RSvar_sigma <- .01


      #### Used to fill in the covariance matrix
          off_diag_b0_t0 <- -.3*sqrt(res_RIvar)*sqrt(res_RIvar_sigma)
          off_diag_b0_b1 <- -.3*sqrt(res_RIvar)*sqrt(res_RSvar)
          off_diag_t0_t1 <- -.3*sqrt(res_RIvar_sigma)*sqrt(res_RSvar_sigma)
          off_diag_t0_b1 <- .3*sqrt(res_RIvar_sigma)*sqrt(res_RSvar)
          off_diag_t1_b1 <- -.3*sqrt(res_RSvar_sigma)*sqrt(res_RSvar)
          off_diag_b0_t1 <- 0

      #### G matrix
          mus <- c(0, 0, 0, 0)
          sigma <- matrix(data = c(res_RIvar, off_diag_b0_t0 , off_diag_b0_t1, off_diag_b0_b1,
                                   off_diag_b0_t0, res_RIvar_sigma, off_diag_t0_t1, off_diag_t0_b1,
                                   off_diag_b0_t1, off_diag_t0_t1, res_RSvar_sigma, off_diag_t1_b1,
                                   off_diag_b0_b1, off_diag_t0_b1, off_diag_t1_b1, res_RSvar), 
                          nrow = 4, ncol = 4)
      
          G_corr <- cov2cor(sigma)
          colnames(G_corr) <- c("u0j", "v0j", "v1j", "u1j")
          row.names(G_corr) <- c("u0j", "v0j", "v1j", "u1j")
           
# Actually generating the data, GenData ---------------
    # Generating Level-two data. 
          set.seed(5189)
          lev2_rans <- rmvnorm(n = Lev2N, mus, sigma) # u's and v's from equations 1 and 2
          trans <- rnorm(Lev2N,mean = 0,sd = 1) #Transformative leadership
          L2_similar <- rnorm(Lev2N,mean = 0,sd = sqrt(.2)) # Level 2 similarity uncentered
          
          ### Accounting for unbalanced sample size. 
          trans_unbal <- rep(trans, with_sample_cross)
          
          # Transformative leadership grand mean centered. 
          trans_gmc <- trans - mean(trans_unbal)
          
          #Level two similarity unbalanced
          similar_L2_unbal <- rep(L2_similar, with_sample_cross)
          
          
          
          # Creating an empty list of lists to hold the generated data for each group
          data_holder_cross_list <-  rep( list(list()), Lev2N )
          # Using a for loop to generate the level-one data for each team and  
          #then bind all 250 groups of the data together
      
    for (GroupID in 1:Lev2N){
        # Generate Standard Normal Variates with Specified Correlation
        
        u0j <- lev2_rans[GroupID, 1]
        v0j <- lev2_rans[GroupID, 2]
        v1j <- lev2_rans[GroupID, 3]
        u1j <- lev2_rans[GroupID, 4]
        
        # Creating the level one similarity variable with the specified correlation.
        similar <- L2_similar[GroupID] + rnorm(n = with_sample_cross[GroupID],mean = 0, sd = sqrt(.8))
        
        
        IndID <- 1:with_sample_cross[GroupID]
        # Similar level 1 group mean centered
        similar_l1_grmc <- similar - L2_similar[GroupID]
        #-# or 
        # similar_l1_grmc <- similar - mean(similar)
        
        
        log_sigma_i = (tau_00 + v0j) + tau_01*L2_similar[GroupID] + tau_02*trans_gmc[GroupID] + 
          (tau_10 + tau_11*trans_gmc[GroupID] + v1j)*similar_l1_grmc
        sigma_i = exp(log_sigma_i)
        e_ij = rnorm(with_sample_cross[GroupID],mean = 0,sd = sigma_i)
        # Create Outcome Variable
        Y = (gamma_00 + u0j) + gamma_01*L2_similar[GroupID] + gamma_02*trans_gmc[GroupID] + 
            (gamma_10 + gamma_11*trans_gmc[GroupID] + u1j)*similar_l1_grmc + e_ij
        
        data_holder_cross_list[[GroupID]] = data.frame(GroupID, IndID, group_size = with_sample_cross[GroupID], 
                                                        transform_grandmc = trans_gmc[GroupID], similar, similarL1_groupmc = similar_l1_grmc, 
                                                       similarL2 = L2_similar[GroupID],
                                                       tau_00, v0j,
                                                        tau_01, tau_10, tau_11, v1j, gamma_00, u0j, 
                                                        gamma_01, gamma_10, gamma_11, u1j,LMX = Y, sigma_i, e_ij)
      }

    ### Binding all 250 groups of the data together
      data_holder_cross_centered <- do.call(rbind, data_holder_cross_list)

      data_holder_cross_centered <- as.data.frame(data_holder_cross_centered) #Converting from matrix to a dataframe
      
      #Centering the tranformational leadership variable at two standard deviations above the mean to get the 
      #effect of similiarity specifically for very high transformational leaders. 
      
      
      data_holder_cross_centered$transform_c2 <-    
        data_holder_cross_centered$transform_grandmc - 2
      
#################################################################################################
# Note: Data generation and manipulation complete. The final data are data_holder_cross_centered. 
# From now until the end of this script, we are
# performing analyses. All models that are estimated using brms use the default
# priors for more information see Bürkner (2017)
#################################################################################################

# Estimating manuscript models, EstManuscriptmodels ---------------
  # Unconditional ICC
      unc_ICC <- lmer(LMX  ~ 1 + (1 | GroupID), data = data_holder_cross_centered) 
      summary(unc_ICC) 
      
  # Full Model that matches the data generation model
      cross_LMX_full <- bf(LMX ~ 1  + similarL1_groupmc + similarL2 + transform_grandmc +
                                similarL1_groupmc:transform_grandmc + (1 + similarL1_groupmc|ID1|GroupID),
                          sigma ~ 1 + similarL1_groupmc + similarL2 + transform_grandmc +
                                  similarL1_groupmc:transform_grandmc + (1 + similarL1_groupmc|ID1|GroupID))
      
  # Estimating the model with brms
      cross_LMX_full_out <- brm(cross_LMX_full,
                                data = data_holder_cross_centered, chains = 4, cores = 6, iter = 5000,
                                control = list(adapt_delta = .85, max_treedepth = 15), seed = 1848, save_all_pars = T)
      
      summary(cross_LMX_full_out)
      
  
      
      color_scheme_set("darkgray")
      colors <- theme_bw()
      # plot(cross_inform_prior_res, pars = "location Intercept", theme = colors)
      to_alter_LMX <- cross_LMX_full_out
      
      better_LMX_names <- c("Intercept", "sigma_Intercept", "Similar-L1", "Similar-L2",
                            "Transformative", "Interaction", 
                            "sigma_Similar-L1", "sigma_Similar-L2", "sigma_Transformative",
                            "sigma_interaction")
      
      
      names(to_alter_LMX$fit)[1:10] <- c("Intercept", "sigma_Intercept", "Similar-L1", "Similar-L2",
                                         "Transformative", "Interaction", 
                                         "sigma_Similar-L1", "sigma_Similar-L2", "sigma_Transformative",
                                         "sigma_interaction")
      
      plot(to_alter_LMX, pars = better_LMX_names, theme = colors, exact_match = TRUE)
      
     
      
      set.seed(157)
      
      lz<- c("LMX", expression('LMX'['rep']))
      
      pp_check(cross_LMX_full_out, nsamples = 1000, size = 1) +
        scale_color_grey(name = "", labels=lz) +
        theme_bw() +
        ggplot2::xlab("LMX")
      
      
  # Full Model that matches the data generation model with an informative prior 
      informative <- c(prior(normal(0, 1), class = "b"), 
                       prior(normal(0, 1), class = "b", dpar = "sigma"),
                       prior(lkj(10), class = "cor"))
      
      #No need to redefine the same equation.
      
      cross_def_full_info_out <- brm(cross_LMX_full, data = data_holder_cross_centered, 
                                     chains = 4, cores = 6, iter = 5000,
                                     control = list(adapt_delta = .85, max_treedepth = 15), 
                                     prior = informative,
                                     seed = 14, save_all_pars = T)
      summary(cross_def_full_info_out)
      
      
  # Transformative leadership centered at two standard deviations above the mean
      
      cross_LMX_full_transc2 <- bf(LMX ~ 1  + similarL1_groupmc + similarL2 + transform_c2 +
                                         similarL1_groupmc:transform_c2 + (1 + similarL1_groupmc|ID1|GroupID),
                                  sigma ~ 1 + similarL1_groupmc + similarL2 + transform_c2 +
                                          similarL1_groupmc:transform_c2 + (1 + similarL1_groupmc|ID1|GroupID))
      
  # Estimating the model with brms
      cross_LMX_full_transc2_out <- brm(cross_LMX_full_transc2,
                                        data = data_holder_cross_centered, chains = 4, cores = 6, iter = 5000,
                                        control = list(adapt_delta = .85, max_treedepth = 15), seed = 1954, save_all_pars = T)
      
      summary(cross_LMX_full_transc2_out)
      
        
# Other models ---------------   
    ##Empty model 

    empty_mod <- bf(Y ~ 1 ,
                    sigma ~ 1 )

    empty_mod_out <- brm(empty_mod, data = data_holder_cross_centered, chains = 4, cores = 7, iter = 5000,
                           control = list(adapt_delta = .85, max_treedepth = 15), seed = 8154, save_all_pars = T)

    print(summary(empty_mod_out), digits = 4)
    
    empty_mod_freq <- lm(Y ~ 1, data = data_holder_cross_centered)
    summary(empty_mod_freq)


    ##Random Intercept 
    
    random_int <- bf(Y ~ 1 + (1 | GroupID),
                    sigma ~ 1 )
    
    random_int_out <- brm(random_int, data = data_holder_cross_centered, chains = 4, cores = 7, iter = 5000,
                         control = list(adapt_delta = .85, max_treedepth = 15), seed = 186, save_all_pars = T)
    
    summary(random_int_out)
    
    unc_ICC <- lmer(Y  ~ 1 + (1 | GroupID), data = data_holder_cross_centered) 
    summary(unc_ICC)
    
    ## Random Intercept for location and scale ----
      ###From now on only the Bayesian approach will be estimated.
      ###The extra ID1 variable is a generic variable that is needed to accomodate random intercepts on the scale side
    random_ints_loc_scale <- bf(Y ~ 1   + (1 |ID1|GroupID),
                          sigma ~ 1 +  (1 |ID1|GroupID))
    
    random_ints_loc_scale_out <- brm(random_ints_loc_scale, data = data_holder_cross_centered, 
                                     chains = 4, cores = 7, iter = 5000,
                                     control = list(adapt_delta = .85, 
                                                    max_treedepth = 15), seed = 4789, save_all_pars = T)
    
    ## Random slope for location and random intercept for scale 
      ### 
    
    randomSlopeLoc_RIscale <- bf(Y ~ 1 + similarL1_groupmc + (1 + similarL1_groupmc|ID1|GroupID),
                                sigma ~ 1 +  (1 |ID1|GroupID))
    
    randomSlopeLoc_RIscale_out <- brm(randomSlopeLoc_RIscale, data = data_holder_cross_centered, 
                                     chains = 4, cores = 7, iter = 5000,
                                     control = list(adapt_delta = .85, 
                                                    max_treedepth = 15), seed = 4, save_all_pars = T)
    
    
    ## Random slopes on both sides 
    
    randomSlope_both <- bf(Y ~ 1  + similarL1_groupmc + (1 + similarL1_groupmc|ID1|GroupID),
                         sigma ~ 1 + similarL1_groupmc + (1 + similarL1_groupmc|ID1|GroupID))
    
    ### Estimating the model with brms
    randomSlope_both_out <- brm(randomSlope_both, data = data_holder_cross_centered, chains = 4, cores = 6, iter = 5000,
                              control = list(adapt_delta = .85, max_treedepth = 15), seed = 8712, save_all_pars = T)
    
    
    ## Full model Same as above  
    ### Defining the full model that corresponds to the data generation model for brms.
    
    cross_def_full <- bf(Y ~ 1  + similarL1_groupmc + similarL2_grandmc + transform_grandmc + similarL1_groupmc:transform_grandmc + (1 + similarL1_groupmc|ID1|GroupID),
                         sigma ~ 1 + similarL1_groupmc + similarL2_grandmc + transform_grandmc + similarL1_groupmc:transform_grandmc + (1 + similarL1_groupmc|ID1|GroupID))
    
    ### Estimating the model with brms
    cross_def_full_out <- brm(cross_def_full, data = data_holder_cross_centered, chains = 4, cores = 6, iter = 5000,
                              control = list(adapt_delta = .85, max_treedepth = 15), seed = 1848, save_all_pars = T)
    
 



