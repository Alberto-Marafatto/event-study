# Function

function_V1 <- function(WD1, # start window form WD1 days after the event
                        WD2, # end window from WD2 days after the event
                        announcements, # dataframe with the announcements
                        data_check # which event to consider
                        ){
  window1 <- -5
  window2 <- 5
  TOT <- matrix(ncol = 16) |> 
    as.data.frame()
  names(TOT) <- c("date", "daily.returns", "Mkt-RF", "RF", "daily.returns.rf",
                  "day_count" ,"event_date", "event_type", "event_name",
                  "prediction", "AR", "sd_AR", "standardized_AR", 
                  "CAR", "sd_CAR", "standardized_CAR")
  TOT <- TOT |> 
    mutate(date = as_date(date),
           event_date = as_date(event_date))
  returns_mkt_XXX_RF <- returns_mkt_XXX_RF |> 
    arrange(date)
  announcements <- announcements |> 
    arrange(Declared_Date)
  for (i in as.character(announcements$Declared_Date)) {
    # for each event define the window we consider to estimate the AR and estimated sd of the AR
    w1 <- which(returns_mkt_XXX_RF$date == as_date(i)) - 220
    w2 <- which(returns_mkt_XXX_RF$date == as_date(i)) - 21
    tmp <- returns_mkt_XXX_RF[w1:w2, ]
    lm_tmp <- lm(daily.returns ~ `Mkt-RF`, tmp)
    # for each event define the event window
    w11 <- which(returns_mkt_XXX_RF$date == as_date(i)) - 5 
    w22 <- which(returns_mkt_XXX_RF$date == as_date(i)) + 5
    tmp1 <- returns_mkt_XXX_RF[w11:w22, ] 
    tmp2 <- tmp1 |> 
      ungroup() |> 
      arrange(date) |> 
      mutate(event_date = as_date(i),
             day_count =  row_number() - 6,
             event_type = announcements[announcements$Declared_Date == as_date(i), ]$NEWS,
             event_name = announcements[announcements$Declared_Date == as_date(i), ]$action_type,
             prediction = predict(lm_tmp, tmp1),
             AR = daily.returns - prediction,
             sd_AR = sigma(lm_tmp),
             standardized_AR = AR / sd_AR)
    tmp3 <- tmp2 |> 
      ungroup() |> 
      filter(day_count >= WD1,
             day_count <= WD2) |> 
      arrange(day_count) |> 
      mutate( 
        # compute the CAR
        CAR = cumsum(AR),
        sd_CAR = sqrt(cumsum(sd_AR^2)),
        standardized_CAR = CAR / sd_CAR
      )
    TOT <- rbind(TOT, tmp3)
  }
  TOT <- TOT[-1,]
  
  # significance test 
  
  tmp_55 <- data.frame(significance = c("x > 0.975", 
                                        "0.025 < x < 0.975", 
                                        "x < 0.025"))
  # outputs if we want to check for dividends announcement effects
  if (data_check == "Dividends"){
    
    TOT_AR <- TOT |> 
      mutate(event_date = as.factor(event_date)) |> 
      group_by(event_date) |> 
      arrange(day_count, .by_group = T) |> 
      ggplot() +
      geom_line(aes(y = AR, x = day_count, 
                    group = event_date, 
                    col = event_type)) +
      colScale
    
    TOT_CAR <- TOT |> 
      mutate(event_date = as.factor(event_date)) |> 
      group_by(event_date) |> 
      arrange(day_count, .by_group = T) |> 
      ggplot() +
      geom_line(aes(y = CAR, x = day_count, 
                    group = event_date, 
                    col = event_type)) +
      colScale
    # table which shows the significance test results
    significance <- TOT |> select(event_date, event_type, 
                                  date, day_count, 
                                  standardized_AR, standardized_CAR) |> 
      mutate(significance_AR = ifelse(standardized_AR < qnorm(0.025),
                                      "x < 0.025",
                                      NA),
             significance_AR_U = ifelse(standardized_AR > qnorm(0.975),
                                        "x > 0.975",
                                        NA),
             significance_AR = ifelse(is.na(significance_AR),
                                      significance_AR_U,
                                      significance_AR),
             significance_AR = ifelse(is.na(significance_AR),
                                      "0.025 < x < 0.975",
                                      significance_AR),
             significance_CAR = ifelse(standardized_CAR < qnorm(0.025),
                                       "x < 0.025",
                                       NA),
             significance_CAR_U = ifelse(standardized_CAR > qnorm(0.975),
                                         "x > 0.975",
                                         NA),
             significance_CAR = ifelse(is.na(significance_CAR),
                                       significance_CAR_U,
                                       significance_CAR),
             significance_CAR = ifelse(is.na(significance_CAR),
                                       "0.025 < x < 0.975",
                                       significance_CAR)) |> 
      select(-significance_AR_U,
             -significance_CAR_U)
    
    sig_NO_AR <- significance |> 
      filter(event_type == "NEUTRAL") |> 
      select(significance_AR, day_count) |> 
      mutate(count = 1) |> 
      group_by(significance_AR, day_count) |> 
      mutate(count = sum(count)) |> 
      distinct() |> 
      ungroup() |> 
      arrange(day_count) |> 
      pivot_wider(names_from = day_count,
                  values_from = count)
    
    sig_NO_AR <- tmp_55 |> 
      full_join(sig_NO_AR, by = join_by("significance" == "significance_AR")) 
    sig_NO_AR[is.na(sig_NO_AR)] <- 0
    
    sig_GOOD_AR <- significance |> 
      filter(event_type == "GOOD") |> 
      select(significance_AR, day_count) |> 
      mutate(count = 1) |> 
      group_by(significance_AR, day_count) |> 
      mutate(count = sum(count)) |> 
      distinct() |> 
      ungroup() |> 
      arrange(day_count) |> 
      pivot_wider(names_from = day_count,
                  values_from = count)
    
    sig_GOOD_AR <- tmp_55 |> 
      full_join(sig_GOOD_AR, by = join_by("significance" == "significance_AR")) 
    sig_GOOD_AR[is.na(sig_GOOD_AR)] <- 0
    sig_GOOD_AR <- sig_GOOD_AR |> 
      kbl() |> 
      kable_classic(full_width = F, html_font = "Cambria")
    
    
    sig_BAD_AR <- significance |> 
      filter(event_type == "BAD")|> 
      select(significance_AR, day_count) |> 
      mutate(count = 1) |> 
      group_by(significance_AR, day_count) |> 
      mutate(count = sum(count)) |> 
      distinct() |> 
      ungroup() |> 
      arrange(day_count) |> 
      pivot_wider(names_from = day_count,
                  values_from = count)
    
    sig_BAD_AR <- tmp_55 |> 
      full_join(sig_BAD_AR, by = join_by("significance" == "significance_AR")) 
    sig_BAD_AR[is.na(sig_BAD_AR)] <- 0
    sig_BAD_AR <- sig_BAD_AR |> 
      kbl() |> 
      kable_classic(full_width = F, html_font = "Cambria")
    
    
    sig_NO_CAR <- significance |> 
      filter(event_type == "NEUTRAL") |> 
      select(significance_CAR, day_count) |> 
      mutate(count = 1) |> 
      group_by(significance_CAR, day_count) |> 
      mutate(count = sum(count)) |> 
      distinct() |> 
      ungroup() |> 
      arrange(day_count) |> 
      pivot_wider(names_from = day_count,
                  values_from = count)
    
    sig_NO_CAR <- tmp_55 |> 
      full_join(sig_NO_CAR, by = join_by("significance" == "significance_CAR")) 
    sig_NO_CAR[is.na(sig_NO_CAR)] <- 0
    sig_NO_CAR <- sig_NO_CAR |> 
      kbl() |> 
      kable_classic(full_width = F, html_font = "Cambria")
    
    sig_GOOD_CAR <- significance |> 
      filter(event_type == "GOOD") |> 
      select(significance_CAR, day_count) |> 
      mutate(count = 1) |> 
      group_by(significance_CAR, day_count) |> 
      mutate(count = sum(count)) |> 
      distinct() |> 
      ungroup() |> 
      arrange(day_count) |> 
      pivot_wider(names_from = day_count,
                  values_from = count)
    
    sig_GOOD_CAR <- tmp_55 |> 
      full_join(sig_GOOD_CAR, by = join_by("significance" == "significance_CAR")) 
    sig_GOOD_CAR[is.na(sig_GOOD_CAR)] <- 0
    sig_GOOD_CAR <- sig_GOOD_CAR |> 
      kbl() |> 
      kable_classic(full_width = F, html_font = "Cambria")
    
    
    sig_BAD_CAR <- significance |> 
      filter(event_type == "BAD")|> 
      select(significance_CAR, day_count) |> 
      mutate(count = 1) |> 
      group_by(significance_CAR, day_count) |> 
      mutate(count = sum(count)) |> 
      distinct() |> 
      ungroup() |> 
      arrange(day_count) |> 
      pivot_wider(names_from = day_count,
                  values_from = count)
    
    sig_BAD_CAR <- tmp_55 |> 
      full_join(sig_BAD_CAR, by = join_by("significance" == "significance_CAR")) 
    sig_BAD_CAR[is.na(sig_BAD_CAR)] <- 0
    sig_BAD_CAR <- sig_BAD_CAR |> 
      kbl() |> 
      kable_classic(full_width = F, html_font = "Cambria")
    
    return(list(TOT = TOT, 
                TOT_AR = TOT_AR, TOT_CAR = TOT_CAR, 
                sig_BAD_AR = sig_BAD_AR, sig_GOOD_AR = sig_GOOD_AR, sig_NO_AR = sig_NO_AR,
                sig_BAD_CAR = sig_BAD_CAR, sig_GOOD_CAR = sig_GOOD_CAR, sig_NO_CAR = sig_NO_CAR))
    
  } else {
  # if not dividend announcements
    TOT_AR <- TOT |> 
      mutate(event_name = as.factor(event_name)) |> 
      group_by(event_date) |> 
      arrange(day_count, .by_group = T) |> 
      ggplot() +
      geom_line(aes(y = AR, x = day_count, 
                    group = event_date, 
                    col = event_name))
    
    TOT_CAR <- TOT |> 
      mutate(event_name = as.factor(event_name)) |> 
      group_by(event_date) |> 
      arrange(day_count, .by_group = T) |> 
      ggplot() +
      geom_line(aes(y = CAR, x = day_count, 
                    group = event_date, 
                    col = event_name)) 
    
    significance <- TOT |> select(event_date, event_type, event_name,
                                  date, day_count, 
                                  standardized_AR, standardized_CAR) |> 
      mutate(significance_AR = ifelse(standardized_AR < qnorm(0.025),
                                      "x < 0.025",
                                      NA),
             significance_AR_U = ifelse(standardized_AR > qnorm(0.975),
                                        "x > 0.975",
                                        NA),
             significance_AR = ifelse(is.na(significance_AR),
                                      significance_AR_U,
                                      significance_AR),
             significance_AR = ifelse(is.na(significance_AR),
                                      "0.025 < x < 0.975",
                                      significance_AR),
             significance_CAR = ifelse(standardized_CAR < qnorm(0.025),
                                       "x < 0.025",
                                       NA),
             significance_CAR_U = ifelse(standardized_CAR > qnorm(0.975),
                                         "x > 0.975",
                                         NA),
             significance_CAR = ifelse(is.na(significance_CAR),
                                       significance_CAR_U,
                                       significance_CAR),
             significance_CAR = ifelse(is.na(significance_CAR),
                                       "0.025 < x < 0.975",
                                       significance_CAR)) |> 
      select(-significance_AR_U,
             -significance_CAR_U)
    
    sig_AR <- significance |> 
      select(significance_AR, day_count) |> 
      mutate(count = 1) |> 
      group_by(significance_AR, day_count) |> 
      mutate(count = sum(count)) |> 
      distinct() |> 
      ungroup() |> 
      arrange(day_count) |> 
      pivot_wider(names_from = day_count,
                  values_from = count)
    
    sig_AR <- tmp_55 |> 
      full_join(sig_AR, by = join_by("significance" == "significance_AR")) 
    sig_AR[is.na(sig_AR)] <- 0
    sig_AR <- sig_AR |> 
      kbl() |> 
      kable_classic(full_width = F, html_font = "Cambria")
    
    sig_CAR <- significance |> 
      select(significance_CAR, day_count) |> 
      mutate(count = 1) |> 
      group_by(significance_CAR, day_count) |> 
      mutate(count = sum(count)) |> 
      distinct() |> 
      ungroup() |> 
      arrange(day_count) |> 
      pivot_wider(names_from = day_count,
                  values_from = count)
    
    sig_CAR <- tmp_55 |> 
      full_join(sig_CAR, by = join_by("significance" == "significance_CAR")) 
    sig_CAR[is.na(sig_CAR)] <- 0
    
    sig_CAR <- sig_CAR |> 
      kbl() |> 
      kable_classic(full_width = F, html_font = "Cambria")
    
    return(list(TOT = TOT, TOT_AR = TOT_AR, 
                TOT_CAR = TOT_CAR, sig_AR = sig_AR, sig_CAR = sig_CAR))
  }
}

saveRDS(function_V1, "function_V1.rds")
