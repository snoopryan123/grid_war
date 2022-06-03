

# ################
# ### Method 2 ###
# ################
# 
# ### get OQ, DQ
# lm_oq = glm(y1 ~ factor(OFF_TEAM_ID) + factor(PARK) , data=X1_, family="poisson")
# lm_dq = glm(y2 ~ factor(DEF_TEAM_ID) + factor(PARK) , data=X2_, family="poisson")
# coeffs_oq = coefficients(lm_oq)[str_detect(names(coefficients(lm_oq)), "OFF")]
# coeffs_dq = coefficients(lm_dq)[str_detect(names(coefficients(lm_dq)), "DEF")]
# coeffs_p1 = coefficients(lm_oq)[str_detect(names(coefficients(lm_oq)), "PARK")]
# coeffs_p2 = coefficients(lm_dq)[str_detect(names(coefficients(lm_dq)), "PARK")]
# se_oq = summary(lm_oq)$coefficients[,2][str_detect(names(coefficients(lm_oq)), "OFF")]
# se_dq = summary(lm_dq)$coefficients[,2][str_detect(names(coefficients(lm_dq)), "DEF")]
# oqdq_df2 = data.frame(
#   TEAM = str_sub(names(coeffs_p1), -3, -1),
#   beta_hat_OQ = unname(coeffs_oq),
#   beta.oq,
#   beta_hat_DQ = unname(coeffs_dq),
#   beta.dq, se_oq, se_dq
#   # beta_hat_p1 = unname(coeffs_p1),
#   # beta_hat_p2 = unname(coeffs_p2),
#   # beta.pk
# ) %>% mutate(beta_hat_OQ_L = beta_hat_OQ - 2*se_oq, beta_hat_OQ_U = beta_hat_OQ + 2*se_oq,
#              beta_hat_DQ_L = beta_hat_DQ - 2*se_dq, beta_hat_DQ_U = beta_hat_DQ + 2*se_dq)
# oqdq_df2
# ### OQ, DQ coefficients are recovered!
# oqdq_df2 %>% ggplot(aes(x=beta.oq, y=beta_hat_OQ)) +
#   geom_point(color="dodgerblue2") +
#   geom_errorbar(aes(ymin = beta_hat_OQ_L, ymax = beta_hat_OQ_U), width=0.005, size=0.5, color="dodgerblue2") +
#   geom_abline(intercept = 0, slope = 1) 
# oqdq_df2 %>% ggplot(aes(x=beta.dq, y=beta_hat_DQ)) +
#   geom_point(color="dodgerblue2") +
#   geom_errorbar(aes(ymin = beta_hat_DQ_L, ymax = beta_hat_DQ_U), width=0.005, size=0.5, color="dodgerblue2") +
#   geom_abline(intercept = 0, slope = 1) 
# 
# 
# 
# ### get PARK
# curr_oq = data.frame(
#   OFF_TEAM_ID = str_sub(names(coeffs_oq), -3, -1),
#   beta_hat_OQ = unname(coeffs_oq)
# ) %>% bind_rows(tibble(OFF_TEAM_ID = "ANA", beta_hat_OQ = 0))
# curr_dq = data.frame(
#   DEF_TEAM_ID = str_sub(names(coeffs_dq), -3, -1),
#   beta_hat_DQ = unname(coeffs_dq)
# ) %>% bind_rows(tibble(DEF_TEAM_ID = "ANA", beta_hat_DQ = 0))
# X_pk = X_ %>% left_join(curr_oq) %>% left_join(curr_dq) 
# 
# lm_park = glm(y ~ beta_hat_OQ + beta_hat_DQ + factor(PARK) , data=X_pk, family="poisson")
# # lm_park
# coeffs_pk = coefficients(lm_park)[str_detect(names(coefficients(lm_park)), "PARK")]
# se_pk = summary(lm_park)$coefficients[,2][str_detect(names(coefficients(lm_park)), "PARK")]
# 
# PARK_df2 = data.frame(
#   PARK = str_sub(names(coeffs_pk), -3, -1),
#   beta_hat_PARK = unname(coeffs_pk),
#   beta.pk,
#   se = unname(se_pk)
# ) %>% mutate(
#   beta_hat_PARK_L = beta_hat_PARK - 2*se,
#   beta_hat_PARK_U = beta_hat_PARK + 2*se
# ) %>% relocate(beta_hat_PARK_L, .before = beta_hat_PARK) %>%
#   relocate(beta_hat_PARK_U, .after = beta_hat_PARK) 
# PARK_df2
# 
# ### evaluate how well we recovered the coeffs
# 
# 
# 
# 
# mean(abs(PARK_df2$beta_hat_PARK - PARK_df2$beta.pk)) 
# 
# PARK_df2 %>% ggplot(aes(x=beta.pk, y=beta_hat_PARK )) +
#   geom_point(color="dodgerblue2") +
#   geom_errorbar(aes(ymin = beta_hat_PARK_L, ymax = beta_hat_PARK_U), 
#                 width=0.005, size=0.5, color="dodgerblue2") +
#   geom_abline(intercept = 0, slope = 1) 
# 
# 
# 
# 
# #####################
# ### Method 1: OLS ###
# #####################
# 
# lm1 = glm(y ~ factor(OFF_TEAM_ID) + factor(DEF_TEAM_ID) + factor(PARK), data=X_, family="poisson")
# 
# coeffs_pk1 = coefficients(lm1)[str_detect(names(coefficients(lm1)), "PARK")]
# se_pk1 = summary(lm1)$coefficients[,2][str_detect(names(coefficients(lm1)), "PARK")]
# 
# PARK_df1 = data.frame(
#   PARK = str_sub(names(coeffs_pk1), -3, -1),
#   beta_hat_PARK = unname(coeffs_pk1),
#   beta.pk,
#   se = unname(se_pk1)
# ) %>% mutate(
#   beta_hat_PARK_L = beta_hat_PARK - 2*se,
#   beta_hat_PARK_U = beta_hat_PARK + 2*se
# ) %>% relocate(beta_hat_PARK_L, .before = beta_hat_PARK) %>%
#   relocate(beta_hat_PARK_U, .after = beta_hat_PARK) 
# PARK_df1
# 
# mean(abs(PARK_df1$beta_hat_PARK - PARK_df1$beta.pk)) 
# PARK_df1$method = "ols"
# PARK_df2$method = "method 2"
# 
# # plot_2017_parkEffectResults = bind_rows(PARK_df1, PARK_df2) %>% 
# #   ggplot(aes(x=beta.pk, y=beta_hat_PARK, color=method)) +
# #   geom_point(size=1) +
# #   geom_errorbar(aes(ymin = beta_hat_PARK_L, ymax = beta_hat_PARK_U), width=0.01) +
# #   geom_abline(intercept = 0, slope = 1) +
# #   scale_color_manual(values=c("dodgerblue2", "firebrick")) +
# #   labs(x="true park effect", y="fitted park effect")
# # plot_2017_parkEffectResults
# 
# plot_2017_parkEffectResults = bind_rows(PARK_df1, PARK_df2) %>% 
#   ggplot(aes(x=exp(beta.pk), y=exp(beta_hat_PARK), color=method)) +
#   geom_point(size=1) +
#   geom_errorbar(aes(ymin = exp(beta_hat_PARK_L), ymax = exp(beta_hat_PARK_U)), width=0.01) +
#   geom_abline(intercept = 0, slope = 1) +
#   scale_color_manual(values=c("dodgerblue2", "firebrick")) +
#   labs(x="true park effect", y="fitted park effect")
# plot_2017_parkEffectResults
# 
# ggsave("plot_2017_poisson_parkEffectResults.png", plot_2017_parkEffectResults, width=8, height=6)
# 
# #####################
# ### Method 3: ridge ###
# #####################
# 
# ridge3 = glmnet(
#   x = model.matrix(~ factor(OFF_TEAM_ID) + factor(DEF_TEAM_ID) + factor(PARK), data=X_),
#   y = X_$y, alpha = 0, lambda = 0.035, family="poisson")
# 
# coeffs_ridge3_all = coef(ridge3)[,1]
# coeffs_ridge3 = coeffs_ridge3_all[str_detect(names(coeffs_ridge3_all), "PARK")]
# 
# PARK_df3 = data.frame(
#   PARK = str_sub(names(coeffs_ridge3), -3, -1),
#   beta_hat_PARK = unname(coeffs_ridge3),
#   beta.pk,
#   method="ridge.3"
#   # se = unname(se_pk3)
# ) 
# # %>% mutate(
# #   beta_hat_PARK_L = beta_hat_PARK - 2*se,
# #   beta_hat_PARK_U = beta_hat_PARK + 2*se
# # ) %>% relocate(beta_hat_PARK_L, .before = beta_hat_PARK) %>%
# #   relocate(beta_hat_PARK_U, .after = beta_hat_PARK) 
# PARK_df3
# 
# norm(PARK_df1$beta.pk - PARK_df1$beta_hat_PARK, "2")
# norm(PARK_df2$beta.pk - PARK_df2$beta_hat_PARK, "2")
# norm(PARK_df3$beta.pk - PARK_df3$beta_hat_PARK, "2")
# 
# plot_2017_parkEffectResults = bind_rows(PARK_df1, PARK_df2, PARK_df3) %>% 
#   ggplot(aes(x=exp(beta.pk), y=exp(beta_hat_PARK), color=method)) +
#   geom_point(size=1) +
#   # geom_errorbar(aes(ymin = exp(beta_hat_PARK_L), ymax = exp(beta_hat_PARK_U)), width=0.01) +
#   geom_abline(intercept = 0, slope = 1) +
#   scale_color_manual(values=c("dodgerblue2", "firebrick", "black")) +
#   labs(x="true park effect", y="fitted park effect")
# plot_2017_parkEffectResults
# 
# # #####################
# # ### Method 3: JS ###
# # #####################
# # 
# # PARK_df3 = PARK_df2 %>% mutate(
# #   mu_hat = sum(beta_hat_PARK)/sum(se),
# #   denom =  sum( (mu_hat - beta_hat_PARK)^2/sd(beta_hat_PARK)^2  ),
# #   beta_hat_PARK = mu_hat + (beta_hat_PARK - mu_hat)*(1 - (n()-2)/denom ),
# #   beta_hat_PARK_L = mu_hat + (beta_hat_PARK_L - mu_hat)*(1 - (n()-2)/denom ),
# #   beta_hat_PARK_U = mu_hat + (beta_hat_PARK_U - mu_hat)*(1 - (n()-2)/denom ),
# #   method = "JS.3"
# # )
# # 
# # plot_2017_parkEffectResults = bind_rows(PARK_df1, PARK_df2, PARK_df3) %>% 
# #   ggplot(aes(x=exp(beta.pk), y=exp(beta_hat_PARK), color=method)) +
# #   geom_point(size=1) +
# #   geom_errorbar(aes(ymin = exp(beta_hat_PARK_L), ymax = exp(beta_hat_PARK_U)), width=0.01) +
# #   geom_abline(intercept = 0, slope = 1) +
# #   # scale_color_manual(values=c("dodgerblue2", "firebrick", "black")) +
# #   labs(x="true park effect", y="fitted park effect")
# # plot_2017_parkEffectResults
# # 
# # norm(PARK_df1$beta.pk - PARK_df1$beta_hat_PARK, "2")
# # norm(PARK_df2$beta.pk - PARK_df2$beta_hat_PARK, "2")
# # norm(PARK_df3$beta.pk - PARK_df3$beta_hat_PARK, "2")

