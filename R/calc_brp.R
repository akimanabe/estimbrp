calc_brp <- function(res_vpa_MSY){

  cat("M = ", res_vpa_MSY$input$dat$M[1,1])

  #+ echo=FALSE, test_description ----
  # テスト用の記述　test_modeが存在してTRUEだったら、tests/overwrite_seeting.rをsourceして上記までの設定を上書きする
  if(sum(ls()=="test_mode")==1 && test_mode==TRUE) source("tests/overwrite_setting.r")

  ## 出力ファイルの名前
  if(!file.exists(output_folder)) dir.create(output_folder, recursive=TRUE)

  old.warning <- options()$warn
  options(warn=warning_option)
  options(tibble.width=Inf)

  ## define graph object
  graph_MSY <- list()

  #' ### 読み込んだVPA結果

  #+ fig.height=10
  plot_vpa(res_vpa_MSY)

  #' ### 生物パラメータ&選択率などの設定
  #' waa_fun, maa_funがTRUEの場合にはここでの設定が将来予測では上書きされることに注意

  # set biological parameter
  if(select_waa_in_MSY==1) waa_in_MSY <- apply_year_colum(res_vpa_MSY$input$dat$waa, waa_year_in_MSY)
  if(select_maa_in_MSY==1) maa_in_MSY <- apply_year_colum(res_vpa_MSY$input$dat$maa, maa_year_in_MSY)
  if(select_M_in_MSY  ==1) M_in_MSY   <- apply_year_colum(res_vpa_MSY$input$dat$M  , M_year_in_MSY  )
  if(select_waa.catch_in_MSY==0) waa_catch_in_MSY <- waa_in_MSY
  if(select_waa.catch_in_MSY==1) waa_catch_in_MSY <- apply_year_colum(res_vpa_MSY$input$dat$waa.catch, waa_year_in_MSY)

  biopar_MSY <- tibble(waa=waa_in_MSY, maa=maa_in_MSY, M=M_in_MSY, waa.catch=waa_catch_in_MSY) %>%
    filter(!is.na(waa))
  # knitr::kable(biopar_MSY)

  #' ### SR関係のフィット

  #+ R_code_for_SRfit ----

  if(given_SR_object==0){

    # より頑健な（でも時間がかかる）関数を使う場合、fit.SRやfit.SRregimeと置き換える
    if(do_check_SRfit==TRUE){
      if(use_regime_model==FALSE) fit.SR <- function(...) fit.SR_tol(..., is_regime=FALSE,n_check=n_check, fun_when_check5_replace=fun_when_check5_replace)
      if(use_regime_model==TRUE) fit.SRregime <- function(...) fit.SR_tol(..., is_regime=TRUE,n_check=n_check, fun_when_check5_replace=fun_when_check5_replace)
    }

    data_SR <- get.SRdata(res_vpa_MSY, return.df=TRUE, weight.year=year_for_srfit, weight.data=weight.data)
    data_SR <- data_SR %>% dplyr::filter(R>0)
    SR_weight <- data_SR$weight
    if(all(year_for_srfit)<0) stop("year_for_srfitにマイナス値を与えるオプションは廃止しました.. (バグが誘発されるため)")

    if(use_regime_model==0){
      res_SR_MSY <- fit.SR(data_SR,
                           SR      = function_srfit,
                           method  = L1L2_srfit,
                           AR      = AR_srfit,
                           out.AR  = AR_estimation,
                           hessian = FALSE,
                           bio_par = biopar_MSY,
                           plus_group = res_vpa_MSY$input$plus.group,
                           HS_fix_b = {if(rule_type=="1B") HS_breakpoint_per_minSSB*min(data_SR$SSB) else NULL}
      )

    }

    if(use_regime_model==1){
      res_SR_MSY <- fit.SRregime(data_SR,
                                 SR     = function_srfit,
                                 method = L1L2_srfit,
                                 regime.year = regime_year_MSY,
                                 regime.key  = regime_key_MSY,
                                 regime.par  = regime_independent_pars_MSY,
                                 bio_par = biopar_MSY,
                                 plus_group = res_vpa_MSY$input$plus.group)

    }

    if(make_SRmodel_table==1 && use_regime_model==0){
      # 網羅的なパラメータ推定
      SR_table <- tryall_SR(data_SR = data_SR, plus_group = res_vpa_MSY$input$plus.group, bio_par=biopar_MSY, tol=do_check_SRfit)
      select_type <- ifelse(res_SR_MSY$input$AR==0,"non",
                            ifelse(res_SR_MSY$input$out.AR==TRUE,"outer","inner"))

      SR_table <- SR_table %>%
        mutate(selection=ifelse(L.type==res_SR_MSY$input$method &
                                  SR.rel==res_SR_MSY$input$SR &
                                  AR.type==select_type,"selected",0)) %>%
        select(-id)

      cat("## 再生産関係テーブル \n")
      knitr::kable(SR_table)
      readr::write_csv(SR_table,path=str_c(output_folder,"/",SR_compared_file_path))
    }


    # 置き換えたfit.SRとfit.SRregimeを消す
    if(do_check_SRfit==TRUE){
      if(use_regime_model==FALSE) rm(fit.SR)
      if(use_regime_model==TRUE) rm(fit.SRregime)
    }
  }

  if(given_SR_object==1){
    res_SR_MSY <- get(load(given_SR_file_name))
    function_srfit <- res_SR_MSY$input$SR
    L1L2_srfit     <- res_SR_MSY$input$method
    AR_srfit       <- res_SR_MSY$input$AR
    out.AR         <- res_SR_MSY$input$AR_estimation
    SR_weight      <- res_SR_MSY$input$SRdata$weight

    use_regime_model <- ifelse(class(res_SR_MSY)=="fit.SRregime",1,0)
    if(use_regime_model==1){
      regime_year_MSY <- res_SR_MSY$input$regime.year
      regime_key_MSY  <- res_SR_MSY$input$regime.key
      regime_independent_pars_MSY <- res_SR_MSY$input$regime.par
    }
  }

  #' ### MSY推定のための将来予測の設定

  #+ MSY_estimation ----

  # 将来予測計算用のデータを構成する
  # set future projection year
  if(select_nyear_MSY==1){
    if(select_GT==0){
      select_GT <- Generation.Time(res_vpa_MSY,
                                   maa=rowMeans(res_vpa_MSY$input$dat$maa,na.rm=T),
                                   M=rowMeans(res_vpa_MSY$input$dat$M,na.rm=T),
                                   Plus=ifelse(res_vpa_MSY$input$plus.group==TRUE,19,0))
      # cat("推定世代時間: ", select_GT, "\n")
    }
    nyear_MSY <- round(select_GT * GT_multi)
  }

  vpa_years <- colnames(res_vpa_MSY$naa)
  future_MSY_year <- vpa_years[apply(res_vpa_MSY$input$dat$caa,2,sum, na.rm=T)>0] %>%
    as.numeric() %>% max()

  # cat("MSY推定するときの将来予測年数: ", nyear_MSY, "(", future_MSY_year+nyear_MSY, "年)\n")

  # set future F
  if(select_Fcurrent_MSY==1){
    Fcurrent_MSY <- apply_year_colum(res_vpa_MSY$faa,target_year=Fcurrent_MSY_year)
  }
  if(select_Fcurrent_MSY==3){
    Fcurrent_MSY <- convert_Fvector(res_vpa    = res_vpa_MSY,
                                    res_future = NULL,
                                    faa_vector      = Fimpact_MSY ,
                                    faa_vector_year = Fimpact_MSY_year,
                                    faa_bio_year    = Fimpact_MSY_bioyear,
                                    saa_vector      = Fsel_MSY,
                                    saa_vector_year = Fsel_MSY_year,
                                    saa_bio_year    = Fsel_MSY_bioyear)$Fvector
  }

  # set pope
  is_pope_logical <- dplyr::case_when(is_pope==1 ~ res_vpa_MSY$input$Pope,
                                      is_pope==2 ~ TRUE,
                                      is_pope==3 ~ FALSE)
  if(is.na(is_pope_logical)) stop("Set appropriate number (1-3) in is_pope")


  # if averaging model parameter
  {if(average_SRmodel==1){
    res_SR_MSY <- purrr::pmap(set_average_SRmodel[,-5],
                              function(SR.rel,L.type,AR,out.AR)
                                fit.SR(data_SR,SR=SR.rel,method=L.type,AR=AR,out.AR=out.AR))
    class(res_SR_MSY) <- "SRfit.average"
    average_option <- list(SR_list=res_SR_MSY,weight=set_average_SRmodel$weight)
  }
    else{
      average_option <- NULL
    }}

  {if(use_regime_model==1){
    regime_shift_option <- list(future_regime=future_regime_MSY)
  }
    else{
      regime_shift_option <- NULL
    }}

  # if given_SR_pars is used
  if(given_SR_pars==1){
    res_SR_MSY$pars[] <- SR_specific_par_MSY
  }

  ## 将来予測で用いる再生産パラメータ
  bind_cols(res_SR_MSY$pars,res_SR_MSY$steepness) %>% mutate(AICc   = res_SR_MSY$AICc,
                                                             method = res_SR_MSY$input$method,
                                                             type   = res_SR_MSY$input$SR) %>%
    knitr::kable()

  #' ### 将来予測のためのパラメータの統合

  #+ make_future_data ----
  data_future_MSY <- make_future_data(res_vpa = res_vpa_MSY,
                                      nsim = MSY_nsim, # number of simulation
                                      nyear = nyear_MSY, # number of future year
                                      future_initial_year_name = future_MSY_year,
                                      start_F_year_name = future_MSY_year+1,
                                      start_biopar_year_name=future_MSY_year+1,
                                      start_random_rec_year_name = future_MSY_year+1,
                                      # biopar setting
                                      waa_year=NULL, waa=waa_in_MSY,
                                      waa_catch_year=NULL, waa_catch=waa_catch_in_MSY,
                                      maa_year=NULL, maa=maa_in_MSY,
                                      M_year=NULL, M=M_in_MSY,
                                      waa_fun=waa_fun_MSY,
                                      waa_fun_name=ifelse(use_specific_waa_fun==TRUE, waa_fun_name_MSY, NA),
                                      waa_catch_fun=waa_catch_fun_MSY,
                                      waa_catch_fun_name=ifelse(use_specific_waa_catch_fun==TRUE, waa_catch_fun_name_MSY, NA),
                                      maa_fun=maa_fun_MSY,
                                      start_waafun_year_name=future_MSY_year+1,
                                      start_waacatchfun_year_name=future_MSY_year+1,
                                      # faa setting
                                      faa_year=NULL,
                                      currentF=Fcurrent_MSY,
                                      futureF=Fcurrent_MSY,
                                      # HCR setting (not work when using TMB)
                                      start_ABC_year_name=future_MSY_year+1,
                                      HCR_beta=1,
                                      HCR_Blimit=-1,
                                      HCR_Bban=-1,
                                      HCR_year_lag=0,
                                      # SR setting
                                      res_SR=res_SR_MSY,
                                      seed_number=MSY_seed,
                                      bias_correction=bias_correction_MSY,
                                      resid_type=SR_error_MSY,
                                      resample_year_range={if(SR_error_MSY%in%c("resample","backward")) select_resample_year else 0},
                                      backward_duration={if(SR_error_MSY%in%c("backward")) backward_duration_MSY else NULL},
                                      recruit_intercept=recruit_intercept_MSY,
                                      model_average_option=average_option,
                                      regime_shift_option =regime_shift_option,
                                      # others
                                      Pope=is_pope_logical,
                                      plus_group=res_vpa_MSY$input$plus.group
  )

  if(do_sd0_check==TRUE) x <- test_sd0_future(data_future_MSY)

  #' ### 実際に将来予測をして管理基準値を計算する

  #+ estimate_MSY_Refence_points ----
  if(rule_type=="1A"){
    res_MSY <- est_MSYRP(data_future=data_future_MSY, ncore=ncore,
                         optim_method=optim_method_msy, compile_tmb=compile_tmb, candidate_PGY=candidate_PGY,
                         only_lowerPGY="lower", candidate_B0=candidate_B0,
                         candidate_Babs=candidate_Babs, calc_yieldcurve=calc_yieldcurve,
                         select_Btarget=select_Btarget, select_Blimit=select_Blimit, select_Bban=select_Bban)
  }
  if(rule_type=="1B"){
    res_MSY <- est_MSYRP_proxy(data_future=data_future_MSY,
                               Fmsy_proxy_candidate=Fmsy_proxy,
                               msy_SPR_candidate=candidate_msy_SPR,
                               Blimit_candidate=candidate_Blimit,
                               Babs_value = Babs_value,
                               Bban_candidate=candidate_Bban, # not calculate all statistics
                               select_Btarget=select_Btarget,
                               select_Blimit=select_Blimit,
                               select_Bban=select_Bban,
                               F.range = seq(from=0,to=10,length=101))
  }

  return(list(res_MSY$summary, res_MSY$res_SR))
}

calc_brp_safe <- purrr::safely(calc_brp, otherwise = NULL)
