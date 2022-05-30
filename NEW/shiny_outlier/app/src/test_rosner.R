smoothing <- function(df_value){
    smp_size <- floor(0.70 * length(df_value))
    df.train <-  window(ts(df_value), end = smp_size) 
    df.test <-  window(ts(df_value), start = smp_size+1) 
    alpha <- seq(.01, .99, by = .01)
    RMSE <- NA
    for(i in seq_along(alpha)) {
        fit <- ses(df.train, alpha = alpha[i], h = 100)
        RMSE[i] <- accuracy(fit, df.test)[2,2]
    }
    # convert to a data frame and idenitify min alpha value
    alpha.fit <- data_frame(alpha, RMSE)
    alpha.min <- filter(alpha.fit, RMSE == min(RMSE))
    #alpha.mean <- filter(alpha.fit, RMSE == mean(RMSE))
    fit_ses_opt = ses(df_value, alpha = alpha.min$alpha)
    return(fit_ses_opt)
}

test_rosner <- function(df){
    # outlier_var = as.numeric(as.character(df[,"value"]))[which(as.numeric(as.character(df[,"value"])) %in% 
    #                                                              boxplot.stats(as.numeric(as.character(df[,"value"])))$out)]
    # test = rosnerTest(as.numeric(as.character(df[,"value"])), k = ifelse(length(outlier_var)==0,1,length(outlier_var)), warn = F)
    # a = test$all.stats$Obs.Num
    # a = as.integer(a)
    # b = test$all.stats$Outlier
    # (list(a,b,test))
    diff = as.numeric(df[,"value"]) - as.numeric(fitted(smoothing(df[,"value"])))
    vr = abs(diff) > ifelse(nrow(df_sel()) >= 50000, 6*mean(df[,"value"]), 1*mean(df[,"value"]))
    # vr = case_when(
    #   height < 200 | mass > 200      ~ "large",
    #   height > 200 | mass > 200      ~ "large",
    #   height > 200 | mass > 200      ~ "large"
    # )
    return(vr)
}