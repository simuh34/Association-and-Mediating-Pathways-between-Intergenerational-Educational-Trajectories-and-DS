gee_model_ipcw1 <- geeglm(depression ~ educational_mobility_pr + age + gender + Race + urbanicity+ stress + health + financial + warmth + height,
                       data = df_long, 
                       id = hhidpn, 
                       family = binomial(link = "logit"), 
                       corstr = "exchangeable")
summary(gee_model_ipcw1)
tab_model(gee_model_ipcw1)

gee_model_weight <- geeglm(depression ~ educational_mobility_pr + age + gender + Race + urbanicity+ stress + health + financial + warmth + height,
                          data = df_long, 
                          id = hhidpn, 
                          weights = WEIGHT,
                          family = binomial(link = "logit"), 
                          corstr = "exchangeable")
summary(gee_model_weight)
tab_model(gee_model_weight)
