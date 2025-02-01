#' <style type="text/css"> .main-container {max-width: 1500px;} </style>
#'
#' ##  [1] MSY管理基準値を計算する
#'
#' - 必要なファイルはVPA結果（csv or rda）です
#' - プラスグループを考慮するかどうかについては自動的にVPAと同じ設定を使います。%SPRの計算などもすべて。
#' - 年齢は rownames(res_vpa$naa) のラベルを読んで、加入年齢を判別します。0歳よりも大きい場合には自動的に再生産関係をずらします。
#'

#+
#

knitr::opts_chunk$set(echo = TRUE, fig.width=6.5,fig.height=3.3, cache=FALSE)

# 実行したときの警告を表示するかどうか (-1: 表示しない, 0: 表示する)
warning_option <- -1

#'
#' ## 1) 入出力ファイルの設定
#' ### 1-1) 読み込みファイル
#'
#' - VPA結果が保存されているファイルの名前 (load_dataで自動的に判別されるのでcsv形式、rda形式どちらでもOK)
#'
#' - rdaファイルはvpa関数の実行結果をsaveしたもの
#' - csvファイルの例はこちら https://github.com/ichimomo/frasyr/blob/dev/inst/extdata/res_vpa_dummy.csv
#' - csvファイルで放流データを含む場合 https://github.com/ichimomo/frasyr/blob/dev/inst/extdata/res_vpa_dummy_release_fish_new.csv (YR2022用の新しいフォーマット)
#'

#+ input_output setting ----

res_vpa_MSY <- res_vpa_example # csvファイルもOKです

#' 1-2) 出力ファイルを置くフォルダの名前

output_folder <- "example_run_RI"

# もし手で走らせるなら以下をTRUE
hand <- FALSE

#' ## 2) 再生産関係のフィット
#' ここの結果とあわせて、再生産関係のモデル診断 (https://github.com/ichimomo/frasyr_tool/SRcheck) を実施し、パラメータの収束の確認と交互に結果を見て再生産関係を決めていきます
#' 詳細についてはここで記述しているリンク先を見てください
#' https://github.com/ichimomo/frasyr_tool/blob/YR2023/SRcheck/README.md

#+ stock_recruitment ----

rule_type <- "1A" # "1B" # 1Bの場合には管理基準値は再生産関係から決めずにF%SPRなどから決めていくことになります

# 1Aの場合にはレジームシフトの設定など再生産関係についていろいろ決めることがあります
if(rule_type=="1A"){
  # 再生産関係をあてはめる年の範囲 0: 全部の年のデータを使う, 正の数字: 指定された数字に一致する年のデータを用いる (最新年の加入は不確実性が多いため、使わないことよくあり。その場合はフィットする年を明示的に指定してください)
  year_for_srfit <- 1988:2016
  # 再生産関係の式 ("HS" | "BH" | "RI")
  function_srfit <- "RI"
  # 最小化手法 ( "L1": 最小絶対値法 | "L2": 最小二乗法)
  L1L2_srfit <- "L2"
  # 自己相関の考慮 (0: なし, 1: あり)
  AR_srfit <- 1
  # 自己相関を計算するときに、自己相関を外側で計算する(TRUE)か、尤度に組み込んで内側で計算する(FALSE)か。推奨は外側(TRUE)。
  AR_estimation <- TRUE
  # 任意の重みをdataに与えたいとき
  # tibbleで与えるdataのyearは実際のdata(data_SR_MSY)のyearと同じ長さである必要があります
  # これが与えられる場合year_for_srfitは無視されます
  weight.data <- NULL #tibble(year=1985:2020, weight=c(rep(0.5,18),rep(1,18)))
  # レジームシフトを考慮する (0: 考慮しない, 1: 考慮する)
  #   考慮する場合、上で指定された再生産関係の式と最小化手法が使われますが、自己相関は考慮されません
  #   また、レジームシフトを考慮する場合には、モデル診断と網羅的な再生産関係のフィット、モデル平均は
  #   使えません
  use_regime_model <- 0
  if(use_regime_model==1){
    regime_year_MSY <- c(1976,1988) # 新しいレジームがスタートする年
    regime_key_MSY  <- c(0, 1, 0) # どの期間を同じレジームと判断するか
    regime_independent_pars_MSY <- c("a","b") # レジーム間で独立と仮定するパラメータ: "a", "b", "sd"の３つから選ぶ
    future_regime_MSY <- 0 # regime_key_MSYで定義したどのレジームが将来おこると仮定するか
    future_regime_scenario <- NULL # 将来の期間にregimeが変わると仮定した場合のシナリオ (未実装)
  }

  # 再生産関係推定の方法を頑健な方法（tol）によっておこなうか。特にL1&HSの場合にはここをTRUEにしてください。初期値を変えて何回もパラメータ推定するため時間かかります。make_SRmodel_tableで網羅的なパラメータ推定する場合やレジーム資源の場合、ちょっと時間かかりすぎるかもしれないです）
  # https://github.com/ichimomo/frasyr_tool/blob/YR2021/SRcheck/SRdiagnostics.md#%E6%8E%A8%E5%AE%9A%E5%80%A4%E3%81%AE%E5%8F%8E%E6%9D%9F%E3%81%AE%E6%9C%89%E7%84%A1%E3%82%84%E6%9C%80%E9%81%A9%E8%A7%A3%E3%81%AB%E9%81%94%E3%81%97%E3%81%A6%E3%81%84%E3%82%8B%E3%81%8B%E3%82%92%E3%83%81%E3%82%A7%E3%83%83%E3%82%AF を実行してみて、1〜5でOKが出ない場合にはTRUEにしたほうが良いかも
  do_check_SRfit <- FALSE
  if(do_check_SRfit==TRUE){
    n_check <- 300
    fun_when_check5_replace <- max
  }

  # 再生産関係を網羅的にフィットしてその結果を表にするかどうか(0: しない, 1: する(時間がちょっとかかります))
  # 表の見方の解説はこちら https://github.com/ichimomo/frasyr_tool/wiki/%E5%86%8D%E7%94%9F%E7%94%A3%E9%96%A2%E4%BF%82%E3%81%AE%E8%A1%A8%E3%81%AE%E8%A6%8B%E6%96%B9
  make_SRmodel_table <- 0
  if(make_SRmodel_table==1){
    # AICを網羅的に検討したその結果を保存するファイルの名前(csvファイル)
    SR_compared_file_path <- "model_selection.csv"
  }
  # 再生産関係を平均する(0: 平均しない、1: 平均する)
  average_SRmodel <- 0
  if(average_SRmodel==1){
    set_average_SRmodel  <- tibble(SR.rel=c("RI","BH"),
                                   L.type=c("L2","L2"),
                                   AR=c(1,1),
                                   out.AR=c(FALSE,FALSE),
                                   weight=c(0.71,0.29))
  }
  # データから推定するのではなく、外部からパラメータを指定して与える（暫定的） (0; 与えない、1; 与える)
  # 関数型は上で設定されたものが用いられます
  given_SR_pars <- 0  # 有効にする場合は1
  if(given_SR_pars==1){
    # a, b, sd, rhoの順番です
    SR_specific_par_MSY <- c(0.030, 1477000, 0.60, 0)
  }

  # SR関係をRのオブジェクトとして呼び出してそれを用いる (0; 用いない、1; 用いる)
  # 関数型などはすべてここで与えたRのオブジェクトで設定されているものに置き換えられます
  # 網羅的な再生産関係のフィットは行いません。モデル平均には非対応。
  # check.SRfitなどを実行して、別のパラメータが推定されたときなどに使います。
  given_SR_object <- 0  # 有効にする場合は1
  if(given_SR_object==1){
    given_SR_file_name <- "output_TMI/HSL2A1/res_SR.rda"
  }
}

# 1Bの場合: 基本的に以下の設定を使うことになります。
if(rule_type=="1B"){
  # フィットする年数（加入尾数を平均する期間）は状況に応じて設定してください
  year_for_srfit <- 1960:2017
  # 再生産関係の式="HS"で固定
  function_srfit <- "HS"
  # HSを用いる場合の折れ点の位置＝過去最低親魚量*x倍とする（作業部会ではx=1と説明しましたが、それだとHSと同じってコメント言われたのでx=0.01をデフォルトとします。0.01よりも小さい値は、計算の都合上、設定できません）
  HS_breakpoint_per_minSSB <- 0.01
  # 最小化手法="L2" (L1でも良いと思うけどあまりそのような状況は想定していないので)
  L1L2_srfit <- "L2"
  # 自己相関の考慮=0 (なし)。かわりに、短期的将来予測ではバックワードリサンプリングを使います
  AR_srfit <- 0
  # 自己相関を計算するときに、自己相関を外側で計算する(TRUE)か、尤度に組み込んで内側で計算する(FALSE)か。推奨は外側(TRUE)。

  # 任意の重みをdataに与えたいとき
  # tibbleで与えるdataのyearは実際のdata(data_SR_MSY)のyearと同じ長さである必要があります
  # これが与えられる場合year_for_srfitは無視されます
  weight.data <- NULL #tibble(year=1985:2020, weight=c(rep(0.5,18),rep(1,18)))

  # 以下、使わない設定
  AR_estimation <- FALSE
  use_regime_model <- 0
  make_SRmodel_table <- 0
  do_check_SRfit <- FALSE
  average_SRmodel <- 0
  given_SR_object <- 0
  given_SR_pars <- 0
}

#' ## 3) MSY推定の設定（F一定の条件下での将来予測をもとにする）
#' 3-1) 選択率とFcurrentの設定
#' MSY推定で用いる選択率はFcurrentの選択率を前提に計算する。詳しい説明はこちらを参照 https://github.com/ichimomo/frasyr_tool/wiki/selectivity
#'

#+ set_selectivity ----
#   1: vpaのF at ageに対して年を指定し、その平均を使う,
#   2: 手動でFcurrentを設定する
#   3: 選択率を参照する年と漁獲圧を参照する年を別にする
select_Fcurrent_MSY <- 1
# 上で1を選んだ場合:Fを平均したい年数
if(select_Fcurrent_MSY==1){
  # 実際の年を指定する場合
  Fcurrent_MSY_year <- 2015:2017
}
# 上で2を選んだ場合:FcurrentとしたいFをベクトルで入力
if(select_Fcurrent_MSY==2){
  Fcurrent_MSY <- c(0.1, 0.2, 0.3, 0.3)
}
# 上で3を選んだ場合:選択率参照年と漁獲の強さの参照年を分ける
if(select_Fcurrent_MSY==3){
  # 漁獲圧の強さとしてFcurrentを代表とする年(実際の年を指定する)
  Fimpact_MSY_year <- 2015:2017
  # 漁獲圧の強さ、直接ベクトルで入れる場合(Fimpact_year_MSYとどちらかだけ指定すること)
  Fimpact_MSY <- NULL # c(0.1, 0.3, 0.3, 0.5)
  # 漁獲圧=>SPR換算するときに参照する生物パラメータの年の範囲(通常はFimpact_year_MSYと同じで良い）
  Fimpact_MSY_bioyear <- 2015:2017
  # 選択率としてFcurrentを代表とする年
  Fsel_MSY_year <- 2008:2017
  # 選択率を直接入れる場合（Fsel_year_MSYとどちらかだけ指定する）
  Fsel_MSY <- NULL # c(0.1, 0.3, 0.3, 0.5)
  # 選択率からFcurrentに換算するときに参照する生物パラメータの年の範囲(通常はFimpact_year_MSYと同じ=Fcurrentと言える範囲の年で良い）
  Fsel_MSY_bioyear <- 2015:2017
}

#' ## 3-2) 各種計算方法
#'

#+ calc_condition ----
# 漁獲量の計算方法（1:VPAと同じ設定を使う, 2:Popeの近似式を使う, 3:Bavanovの式を使う）
is_pope <- 1
# 乱数のシード
MSY_seed <- 1
# MSY計算時のシミュレーション回数(1000回以上推奨)
MSY_nsim <- 1000
# MSYの推定方法（"R" or "tmb"）# tmbはいろいろ対応していない機能があるので使わないでください
# *1の場合TMBをインストールしてください
optim_method_msy <- "R"
# MSYの推定方法としてTMBを選んだ場合、初回の計算時にコンパイルをおこなうか(FALSE: おこなわない、TRUE: おこなう)
compile_tmb <- FALSE
# 漁獲量曲線の計算などをする場合に並列計算をするか(0: しない, 1以上: 用いるコアの数)(windowsでは使えません)
ncore <- 0
# 平衡状態にあると仮定する年の決め方（1: 世代時間の20倍を自動で計算する, 2: 具体的な年数を与える）
select_nyear_MSY <- 1
# *世代時間から計算する場合
if(select_nyear_MSY==1){
  # 世代時間の推定方法（0: 自動的に計算, 1以上の数：ここで指定された値を世代時間（年）とする）
  select_GT <- 0
  # 世代時間の何倍を平衡状態に置くか（デフォルトは20)
  # デフォルトは20ですが、20では平衡状態にならない場合（一定のFで漁獲してまだ親魚資源量の平均値が変化するような場合）、平衡状態になるまで数を大きくしてください
  GT_multi <- 20
}
# *具体的な年数を与える場合
if(select_nyear_MSY==2){
  nyear_MSY <- 400
}
# 複数シミュレーションの結果をあわせて漁獲量を最大化するときの統計量
# *算定ルールでは1（平均）を使うことになっている。 1: 平均（デフォルト）, 2: 中央値
stat_maximize <- 1
# *確率的将来予測の平均と決定論的将来予測の結果が一致するかをテストする（よけいに時間がかかります）
do_sd0_check <- FALSE

#' ## 3-3) 生物パラメータの設定
#'

#+ set_biological_paramter ----
# MSY計算時の年齢別体重(資源量計算用)の設定(1:年で指定する、2:直接指定する)
select_waa_in_MSY <- 1
if(select_waa_in_MSY==1){ # 1の場合にはこちらを設定
  waa_year_in_MSY <- 2015:2017
}
if(select_waa_in_MSY==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
  waa_in_MSY <- c(100,200,300,400)
}

# MSY計算時の年齢別体重(漁獲量計算用)の設定(0:資源量計算用と同じ、1:年数で指定、2:直接指定)
select_waa.catch_in_MSY <- 0
if(select_waa.catch_in_MSY==1){ # 1の場合にはこちらを設定
  waa.catch_year_in_MSY <- 2016:2018
}
if(select_waa.catch_in_MSY==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
  waa.catch_in_MSY <- c(100,200,300,400)
}

# MSY計算時の年齢別成熟率の設定(1:年数で指定、2:直接指定)
select_maa_in_MSY <- 1
if(select_maa_in_MSY==1){ # 1の場合にはこちらを設定
  maa_year_in_MSY <- 2015:2017
}
if(select_maa_in_MSY==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
  maa_in_MSY <- c(0,0,0.5,1)
}

# MSY計算時の自然死亡係数の設定(1:年数で指定、2:直接指定)
select_M_in_MSY <- 1
if(select_M_in_MSY==1){ # 1の場合にはこちらを設定
  M_year_in_MSY <- 2015:2017
}
if(select_M_in_MSY==2){ # 2の場合にはこちらを設定。年毎に異る場合は年齢×年の行列を入れる
  M_in_MSY <- c(0,0,0.5,1)
}

# 体重が密度依存で変化するオプションを利用する (FALSE:利用しない, TRUE:利用する)
waa_fun_MSY <- FALSE
# 体重と密度の関係において、独自の関数を使用する場合
use_specific_waa_fun <- FALSE
if(use_specific_waa_fun==TRUE){
  # 適当に関数を定義して..
  #waa_specific_fun <- function(t,waa,rand,naa,pars_b0,pars_b1){
  #   waa[,1,]*2
  #}
  # 関数名を与える
  # waa_fun_name_MSY <- "waa_specific_fun"
  ## 以下、ホッケの場合(２つ上の階層にあるhokke_waaフォルダにプログラムと推定結果があるとする)
  source("../../hokke_waa/waa_est.R")
  load("../../hokke_waa/hokke_d_mod.rda") # waa_hokkeで推定されたパラメータ推定結果
  # future関数で使う関数の定義(11月=翌年の１月と読み替える)
  waa_specific_fun <- function(t,waa,rand,naa,pars_b0,pars_b1){
    pred_waa <- predict_d_waa(hokke_d_mod, colSums(naa[,t-1,]), month=11)
    pred_waa <- rbind(rep(0.1, ncol(pred_waa)), pred_waa) %>% as.matrix() # 0歳のwaa=0.1（ほぼない）
    pred_waa
  }
  waa_fun_name_MSY <- "waa_specific_fun"
  # test
  #matplot(t(res_vpa_MSY$input$dat$waa[-1,-1]),col=1)
  #matpoints(t(predict_d_waa(hokke_d_mod, colSums(res_vpa_MSY$naa), month=11)),col=2)

}
# 漁獲量計算用の体重が密度で変化するオプションを利用する(FALSE:利用しない, TRUE:利用する)
waa_catch_fun_MSY <- FALSE
# 体重と密度の関係において、独自の関数を使用する場合
use_specific_waa_catch_fun <- FALSE
if(use_specific_waa_catch_fun==TRUE){
  # 適当に関数を定義して..
  #waa_catch_specific_fun <- function(t,waa,rand,naa,pars_b0,pars_b1){
  #waa[,1,]*2
  #}
  # 関数名を与える
  #waa_catch_fun_name_MSY <- "waa_catch_specific_fun"

  ## 以下、ホッケの場合(必要ファイルはwaa_funのほうで読み込まれているとする
  # future関数で使う関数の定義(7月)
  waa_catch_specific_fun <- function(t,waa,rand,naa,pars_b0,pars_b1){
    pred_waa <- predict_d_waa(hokke_d_mod, colSums(naa[,t,]), month=7)
    pred_waa <- rbind(pred_waa, pred_waa[5,]) %>% as.matrix()# 5歳のwaa=4歳のwaaとする
    pred_waa
  }
  waa_catch_fun_name_MSY <- "waa_catch_specific_fun"
  # test
  matplot(t(predict_d_waa(hokke_d_mod, colSums(res_vpa_MSY$naa), month=7)),col=1)
  matpoints(t(res_vpa_MSY$input$dat$waa.catch),col=2)
}

# 成熟率が密度依存で変化するオプションを利用する(FALSE:利用しない, TRUE:利用する) # 太平洋マダラ用（暫定的なものです）
maa_fun_MSY <- FALSE

#' ## 3-4) 再生産の仮定

#+ SR_assumption ----
#  1Bの場合には「対数正規分布」＆「バイアス補正あり」を選択することで、過去の平均加入尾数を仮定したときの管理基準値が計算されます
#  バイアス補正（シミュレーションの平均と決定論的予測が一致するようにする）
#  (TRUE:する(デフォルト)、FALSE: しない)
bias_correction_MSY <- TRUE # FALSE
# 加入変動の誤差分布("lognormal": 対数正規誤差, "resample": 残差リサンプリング)
# 残差の分布がモデル診断の結果表示されるので、その分布を見て正規分布に近ければ"lognormal"を選んで
# 問題ないです。あからさまに変な分布だったら残差リサンプリングも検討してみてください。平均値は同じになるはずです。
# "backward"は、過去をブロックに区切って、段階的にリサンプリングできる期間を増やしていく方法です
SR_error_MSY <- "lognormal" # or "resample" # or "backward"
# SR_error_MSYが"resample"の場合の設定
if(SR_error_MSY=="resample"){
  select_resample_year <- 1960:2018 # または as.numeric(colnames(res_vpa_MSY$naa)) というRコマンドも有効
}
# SR_error_MSYが"backward"の場合の設定
if(SR_error_MSY=="backward"){
  # リサンプリングの年の範囲
  select_resample_year <- 1960:2018 # または as.numeric(colnames(res_vpa_MSY$naa)) というRコマンドも有効
  backward_duration_MSY <- 5 # 1ブロックに入る年数
}
# コンスタントに再生産関係とは独立の加入がある場合(別系群からの移入や放流)の加入尾数
# *通常は0 (放流がある場合でもMSY計算では放流があることを前提にはしない)
recruit_intercept_MSY <- 0

#' ## 3-5) どのような管理基準値を計算するか
#'

#+ RP_definition ----
# 1Aの場合
if(rule_type=="1A"){
  # 管理基準値を計算するPGYレベル（-1: 計算しない, 0から1までの数字のベクトル: MSYx割合に対応する親魚レベル）
  candidate_PGY <- c(0.6,0.1)
  # PGYの下側のみの管理基準値を計算する（"lower": 下側のみ（計算時間短縮）, "both":両側）
  only_lowerPGY <- "lower"
  # 管理基準値を計算するB0レベル（-1: 計算しない, 0から1までの数字のベクトル: B0x割合に対応する親魚レベル）
  candidate_B0 <- c(0.2)
  # 特定の親魚量レベルを直接指定する（-1: 計算しない, 親魚量のベクトル: その親魚量に維持するときの管理基準値）
  # 過去最低の親魚資源量をvpaの結果から持ってくる
  candidate_Babs <- -1 # c(12200, 20000)
  # 目標管理基準値の選択 (0: MSY,
  #                   1以上の数字: MSY_res$summaryの行数,
  #                   負の数字: インタラクティブに決定(エラー出るので修正))
  select_Btarget <- 0
  # 限界管理基準値の選択 (0: 60%MSY,
  #                   1以上の数字: MSY_res$summaryの行数,
  #                   負の数字: インタラクティブに決定 )
  select_Blimit  <- 0
  # 禁漁水準の選択      (0: 10%MSY,
  #                   1以上の数字: MSY_res$summaryの行数,
  #                   負の数字: インタラクティブに決定  )
  select_Bban  <- 0

  #最適化時のオプション①:　PGYを探索するときの上限のF. BlimitやBbanがうまく推定できないとき、この数字を大きくしてみてください
  set_upper_PGY <- 10
  #最適化時のオプション②:　漁獲量曲線を書くときのグリッドの初期値. BlimitやBbanがうまく推定できないとき、漁獲量曲線がスムーズでない場合、ここの値を増やしたりしてみてください。
  set_trace_multi <- sort(c(0.001,seq(from=0,to=2,by=0.1),10,100))
}

# 1Bの場合
if(rule_type=="1B"){
  # 計算するFベースの管理基準値の種類　c("Fmax","F0.1","F%spr")　の３種類をベクトルで指定
  Fmsy_proxy <- c("Fmax","F0.1","F%spr")
  # F%sprを計算する場合、何％の値を計算するか（-1: 計算しない, 0から1までの数字のベクトル: B0x割合に対応する親魚レベル）
  candidate_msy_SPR <- c(30,40)
  # Blimitの候補として計算する管理基準値の種類("Bmin"は過去最低親魚量、"10%B0"はB0の10%、"Babs"は任意の親魚量）
  candidate_Blimit <- c("Bmin","10%B0","Babs")
  # "Babs"を選んだ場合、設定する親ウオ量
  Babs_value <- 10000
  # Bbanの候補として計算する管理基準値の種類（"0"は0,"0.1Blimit"はBlimitの10％,"0.2Blimit"はBlimitの20%）
  #   Bbanについては、対応する親魚量だけを出力し、Fなどの値は出力されません
  candidate_Bban <- c("0","0.1Blimit","0.2Blimit")
  # 目標管理基準値の選択 c("Fmax","F0.1","F%spr")から一つ選択。F%sprを選ぶ場合はF%spr30など、選択する%値をあとにつける
  select_Btarget <- "F%spr30"
  # 限界管理基準値の選択 c("Bmin","10%B0","Babs")から一つ選択。Babsを選ぶ場合はBabs_valueで与えたベクトルの何番目の値かを"Babs1"のように指定する
  select_Blimit  <- "Bmin"
  # 禁漁水準の選択  c("0","0.1Blimit","0.2Blimit")から一つ選択。
  select_Bban  <- "0"
  # ダイナミックBmsyを計算するか
  calc_dynBmsy <- TRUE
}

#' ## 4) 出力の調整
#'

#+ output_setting ----
# Kobe plot
# 管理基準値にラベルをつけるか？(0: つけない, 1:つける)
put_label_kobe <- 1
# Btarget, Blimit, Bbanのラベルを指定(kobe chart以外でも共通のラベルとなる）
label_name_kobe <- c("目標管理基準値案","限界管理基準値案","禁漁水準案")
# kobe plot を書く年の範囲 (0: 特に指定しない, 年数(1990:2000とか): その年のデータだけでKobe plotを書く
plot_year_kobe <- 0 #1989:2018
# 漁獲量曲線に過去の漁獲量を重ね書きする場合、過去の漁獲量をとる範囲 (0: 全年を指定)
past_year_range_yieldcurve <- 0 # c(1990:2000, 2005:2010) # 左のように、漁獲量を書く年をすべて指定する
# 再生産関係の点にラベルをつける年
SRplot_label_year <- c(1988, 2000, 2010, 2017)
# 再生産関係の図で示す予測区間の広さ(モデル平均の場合、現状では0.9で固定で調整できず）
predict_interval_SRplot <- 0.9
# HCRを重ね書きする場合のbeta (デフォルトは0.8, HCRを重ね書きしない場合は負の値を入れる)
# * HCRを入れる場合と入れない場合を2つ作る場合はベクトルで入力
# * ylabel_kobeとのすべての組み合わせのkobe chartが出力される
beta_kobe <- c(-1,0.8)
# HCRの図を書くときに設定するベータの値
beta_default <- 0.8

# バイオマスの単位(出力をこの値で割る)。すべてのバイオマスのデフォルトの単位は「トン」と考えている
biomass.unit_MSY <- 1000
# バイオマスの単位(日本語)。
biomass.name_MSY <- "千トン"
# 加入尾数の単位(出力結果をこの値で割る)
number.unit_MSY <- 1000
# 加入尾数の名前(VPA結果そのものの尾数の単位が系群によって異なる。それぞれの系群で適切な単位を入力すること
number.name_MSY <- "万尾"
# 漁獲量曲線を細かく書くか（FALSE: 書かない（計算時間短縮), TRUE: 書く）
calc_yieldcurve <- TRUE # or FALSE
# いろんなグラフを最後に出力する（少し時間かかるので、グラフ必要ない場合にはFALSE）
make_summary_graphs <- TRUE

if(hand==TRUE){

#'
#' ## Rコードと結果
#' この下のコードは基本的に編集する必要はありません。
#' htmlへの出力としては一部省略されています

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
knitr::kable(biopar_MSY)

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
    cat("推定世代時間: ", select_GT, "\n")
  }
  nyear_MSY <- round(select_GT * GT_multi)
}

vpa_years <- colnames(res_vpa_MSY$naa)
future_MSY_year <- vpa_years[apply(res_vpa_MSY$input$dat$caa,2,sum, na.rm=T)>0] %>%
  as.numeric() %>% max()

cat("MSY推定するときの将来予測年数: ", nyear_MSY, "(", future_MSY_year+nyear_MSY, "年)\n")

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
                       select_Btarget=select_Btarget, select_Blimit=select_Blimit, select_Bban=select_Bban,
                       multi_upper_PGY=set_upper_PGY,
                       trace.multi=set_trace_multi)
                       #trace_multi=set_trace_multi)
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

res_future_MSY <- res_MSY$res_future_MSY
data_future_MSY <- res_MSY$data_future_MSY

# 将来予測結果；概観
par(mfrow=c(2,2), mar=c(3,3,1,1))
plot_future_simple(res_future_MSY)
par(mfrow=c(1,1))

tail(res_future_MSY$summary) %>% arrange(year) %>%
  knitr::kable()

Btarget0 <- derive_RP_value(res_MSY$summary,"Btarget0")$SSB
Blimit0  <- derive_RP_value(res_MSY$summary,"Blimit0")$SSB
Bban0    <- derive_RP_value(res_MSY$summary,"Bban0")$SSB
SPR_MSY0 <- derive_RP_value(res_MSY$summary,"Btarget0")$perSPR
Fmsy0 <- res_MSY$Fvector %>%
  slice(which(res_MSY$summary$RP.definition=="Btarget0")) %>%
  as.numeric()
refs <- tibble(Bmsy  = Btarget0, Blimit= Blimit0, Bban  = Bban0)

# 推定結果
knitr::kable(res_MSY$summary)

# 結果の保存
options(tibble.width=NULL)
save(data_future_MSY,file=str_c(output_folder,"/data_future_MSY.rda"))
save(res_MSY,file=str_c(output_folder,"/res_MSY.rda"))
save(res_SR_MSY,file=str_c(output_folder,"/res_SR.rda"))

#+ output_of_setting ----
knitr::kable(setting_msy <- tibble(age                =dimnames(res_future_MSY$naa)$age,
                                   currentF           =Fcurrent_MSY,
                                   futureF            =Fcurrent_MSY,
                                   maturity_init_year =data_future_MSY$data$maa_mat[,1,1],
                                   bweight_init_year  =data_future_MSY$data$waa_mat[,1,1],
                                   #             cweight_init_year  =data_future_MSY$data$waa.catch_mat[,1,1],
                                   natural_mortality  =data_future_MSY$data$M[,1,1]))

# Fcurrentのグラフ
(graph_fcurrent <- plot_Fcurrent(vpares=res_vpa_MSY,Fcurrent=Fcurrent_MSY,
                                 year.range=rev((future_MSY_year)-0:6)))
ggsave_SH(str_c(output_folder,"/graph_fcurrent.png"), graph_fcurrent)

# waa_fun=TRUEの場合の作図
if(waa_fun_MSY==TRUE && use_specific_waa_fun==FALSE){
  png(str_c(output_folder,"/waa_fun_diagnostics.png"))
  par(mfrow=c(data_future_MSY$data$nage,2),mar=c(2,2,1,1))
  for(i in 1:data_future_MSY$data$nage){
    matplot(log(res_future_MSY$naa[i,,]),
            log(res_future_MSY$waa[i,,]),
            col="gray",pch=20,xlab="log(numbers)",ylab="log(weight)")
    matpoints(log(res_vpa_MSY$naa[i,,]),
              log(res_vpa_MSY$input$dat$waa[i,,]),col="red",pch=20)
    abline(a=mean(data_future_MSY$data$waa_par_mat[i,,"b0"]),
           b=mean(data_future_MSY$data$waa_par_mat[i,,"b1"]),col=2)
    title(str_c("Log scale, Age",rownames(res_vpa_MSY$naa)[i]),line=-1)

    matplot(res_future_MSY$naa[i,,],res_future_MSY$waa[i,,],
            col="gray",pch=20,xlab="numbers",ylab="weight")
    matpoints(res_vpa_MSY$naa[i,,],res_vpa_MSY$input$dat$waa[i,,],
              col="red",pch=20)
    title(str_c("Normal scale, Age",rownames(res_vpa_MSY$naa)[i]),line=-1)
    if(i==1) legend("topright",pch=c(20,20,NA),lty=c(NA,NA,1),
                    col=c("red","gray","red"),
                    legend=c("observed data","predicted data","regression line"))
  }
  dev.off()
}

# waa_catch_fun=TRUEの場合の作図
if(waa_catch_fun_MSY==TRUE && use_specific_waa_catch_fun==FALSE){

  png(str_c(output_folder,"/waa_catch_fun_diagnostics.png"))
  par(mfrow=c(data_future_MSY$data$nage,2),mar=c(2,2,1,1))
  for(i in 1:data_future_MSY$data$nage){
    if(is.null(res_vpa_MSY$input$dat$waa.catch)) res_vpa_MSY$input$dat$waa.catch <- res_vpa_MSY$input$dat$waa
    matplot(log(res_future_MSY$naa[i,,]),
            log(res_future_MSY$waa_catch[i,,]),
            col="gray",pch=20,xlab="log(numbers)",ylab="log(weight)")
    matpoints(log(res_vpa_MSY$naa[i,,]),
              log(res_vpa_MSY$input$dat$waa.catch[i,,]),col="red",pch=20)
    abline(a=mean(data_future_MSY$data$waa_catch_par_mat[i,,"b0"]),
           b=mean(data_future_MSY$data$waa_catch_par_mat[i,,"b1"]),col=2)
    title(str_c("Log scale, Age",rownames(res_vpa_MSY$naa)[i]),line=-1)

    matplot(res_future_MSY$naa[i,,],res_future_MSY$waa_catch[i,,],
            col="gray",pch=20,xlab="numbers",ylab="weight")
    matpoints(res_vpa_MSY$naa[i,,],res_vpa_MSY$input$dat$waa.catch[i,,],
              col="red",pch=20)
    title(str_c("Normal scale, Age",rownames(res_vpa_MSY$naa)[i]),line=-1)
    if(i==1) legend("topright",pch=c(20,20,NA),lty=c(NA,NA,1),
                    col=c("red","gray","red"),
                    legend=c("observed data","predicted data","regression line"))
  }
  dev.off()
}

# ホッケ(use_specific_waa_fun)の場合
if(waa_fun_MSY==TRUE && use_specific_waa_fun==TRUE){
  png(str_c(output_folder,"/waa_fun_diagnostics.png"))
  sum_naa <- apply(res_future_MSY$naa,c(2,3),sum)
  par(mfrow=c(2,3),mar=c(2,2,1,1))
  for(i in 1:5){
    plot(sum_naa[-dim(sum_naa)[[1]],], res_future_MSY$waa[i+1,-1,])
    title(str_c("November, age ",i-1),line=-1)
  }
  dev.off()
}

if(waa_catch_fun_MSY==TRUE && use_specific_waa_catch_fun==TRUE){
  png(str_c(output_folder,"/waa_catch_fun_diagnostics.png"))
  sum_naa <- apply(res_future_MSY$naa,c(2,3),sum)
  par(mfrow=c(2,3),mar=c(2,2,1,1))
  for(i in 1:6){
    plot(sum_naa, res_future_MSY$waa_catch[i,,])
    title(str_c("July, age ",i),line=-1)
  }
  dev.off()
}

# maa_fun=TRUEの場合の作図
if(maa_fun_MSY==TRUE){
  png(str_c(output_folder,"/maa_fun_diagnostics.png"))
  par(mfrow=c(ceiling(data_future_MSY$data$nage/2),2),mar=c(2,2,1,1))
  for(i in 1:data_future_MSY$data$nage){
    matplot(res_future_MSY$naa[i,,],
            res_future_MSY$maa[i,,],
            col="gray",pch=20,xlab="Numbers",ylab="Maturity rate")
    matpoints(res_vpa_MSY$naa[i,,],
              res_vpa_MSY$input$dat$maa[i,,],col="red",pch=20)
    abline(a=mean(data_future_MSY$data$maa_par_mat[i,,"b0"]),
           b=mean(data_future_MSY$data$maa_par_mat[i,,"b1"]),col=2)
    title(str_c("Age",rownames(res_vpa_MSY$naa)[i]),line=-1)
  }
  dev.off()
}

#+ output_of_setting2 ----
if(average_SRmodel==0){
  if(use_regime_model==0){
    (graph_MSY$SRplot <- SRplot_gg(res_SR_MSY,
                                   xscale=biomass.unit_MSY,xlabel=biomass.name_MSY,
                                   yscale=number.unit_MSY,ylabel=number.name_MSY,
                                   labeling.year=SRplot_label_year,
                                   ## 90%信頼区間を表示していることに注意！
                                   refs=NULL,plot_CI=TRUE,CI=predict_interval_SRplot,
                                   recruit_intercept=recruit_intercept_MSY,
                                   add.info=TRUE) + theme_SH()+ scale_x_continuous(labels = scales::comma)+ scale_y_continuous(labels = scales::comma))
  }
  if(use_regime_model==1){

    (graph_MSY$SRplot <- qplot(data_SR$SSB,data_SR$R,xlab="SSB",ylab="R",
                               color=factor(res_SR_MSY$regime_resid$regime),
                               xlim=c(0,max(data_SR$SSB)),ylim=c(0,max(data_SR$R))) +
       geom_line(data=res_SR_MSY$pred, mapping=aes(x=SSB,y=R,color=Regime)) +
       labs(caption=str_c("関数形: ",res_SR_MSY$input$SR,", 自己相関: ", 0 ,
                          ", 最適化法:",res_SR_MSY$input$method,", AICc: ",
                          round(res_SR_MSY$AICc,2))) +
       theme_SH()+theme(legend.position="top")+ scale_x_continuous(labels = scales::comma)+ scale_y_continuous(labels = scales::comma))

  }
}

if(average_SRmodel==1){
  graph_MSY$SRplot <- compare_SRfit(res_SR_MSY)+ggtitle("SR functions averaged")
}

graph_MSY$SRplot
ggsave_SH(str_c(output_folder,"/graph_SRplot.png"), graph_MSY$SRplot)

#' ### 神戸プロット

#+ kobe_plot ----

# SPR.msyを目標としたとき、それぞれのF at age by yearを何倍すればSPR.msyを達成できるか計算
# 今後はkobe.ratioを一貫して使っていく
SPR.history <- get.SPR(res_vpa_MSY,
                       target.SPR=SPR_MSY0*100,Fmax=8)$ysdata
(kobe.ratio <- tibble(year   = as.numeric(colnames(res_vpa_MSY$ssb)),
                      Fratio = SPR.history$"F/Ftarget",
                      Uratio = get_U(res_vpa_MSY)/derive_RP_value(res_MSY$summary,"Btarget0")$U,
                      Bratio = get_ssb(res_vpa_MSY)/Btarget0,
                      DBratio = NA) %>%
    dplyr::filter(!is.na(Bratio)))

if(rule_type=="1B" && calc_dynBmsy==TRUE){

  fmsy <- res_MSY$Fvector[which(res_MSY$summary$RP.definition=="Btarget0"),] %>%
    as.numeric()
  dyn_SB_msy <- dyn.msy(res_vpa_MSY$naa, fmsy=fmsy, a=res_SR_MSY$pars$a, b=res_SR_MSY$pars$b, resid=res_SR_MSY$resid,
                        resid.year=res_SR_MSY$input$SRdata$year, waa=res_vpa_MSY$input$dat$waa, maa=res_vpa_MSY$input$dat$maa,
                        M=res_vpa_MSY$input$dat$M, assume_SR=FALSE)
  kobe.ratio$DBratio_old <- DBratio_old <- get_ssb(res_vpa_MSY)/dyn_SB_msy$ssb

  # future_vpaを使った別の方法
  # 初期の漁獲量はFmsyでの年齢組成＋過去平均５年の加入尾数を仮定
  iniyear <- vpa_years[1] %>% as.numeric()
  init_biopar <- derive_biopar(res_vpa_MSY,derive_year=iniyear)
  init_naa <- calc.rel.abund(Fmsy0,1,nrow(res_vpa_MSY$naa),
                             M  =init_biopar$M,
                             waa=init_biopar$waa,
                             waa.catch=init_biopar$waa,
                             maa=init_biopar$maa)$rel.abund * mean(as.numeric(res_vpa_MSY$naa[1,1:5]))
  res_vpa_DRP <- res_vpa_MSY
  res_vpa_DRP$naa[,1] <- init_naa
  res_future_DRP <- redo_future(data_future_MSY,
                                list(res_vpa           = res_vpa_DRP,
                                     nyear             = nyear_MSY + length(vpa_years),
                                     future_initial_year_name = iniyear,
                                     nsim               = 1000,
                                     start_F_year_name = iniyear,
                                     currentF          = Fmsy0,
                                     futureF           = Fmsy0,
                                     resid_type        = "backward",
                                     bias_correction   = FALSE,
                                     backward_duration = 5))

  # バイアス補正をするかどうか？がけっこう問題。管理基準値のほうもmedianで出してしまうという方法もあるか？
  # とりあえずバイアス補正しないで計算してみる
  graph_MSY$future_DRP <- plot_futures(vpares=res_vpa_MSY, future.list=list(res_future_DRP),
                                       maxyear=2035, what.plot=c("Recruitment","SSB","catch","U"),
                                       Btarget=Btarget0,CI_range=c(0.5,0.5),
                                       ncol=2, n_example=0)

  # データの整理
  vpa_ssb <- get_ssb(res_vpa_MSY) %>%  as.data.frame() %>%
    rownames_to_column() %>% rename(ssb=".", year=rowname)
  dyn_ssb_new <- get_ssb(res_future_DRP) %>% rowMeans() %>% as.data.frame() %>%
    rownames_to_column() %>% rename(SBmsy_dyn=".", year=rowname) %>% left_join(vpa_ssb) %>%
    mutate(DBratio=ssb/SBmsy_dyn, year=as.numeric(year)) %>% as_tibble()

  kobe.ratio <- kobe.ratio %>% select(-DBratio) %>% right_join(dyn_ssb_new)

}

if(put_label_kobe==0) label_name_kobe <- c("","","")

kobe_setting <- expand.grid(beta=beta_kobe) %>%
  as_tibble() %>%
  mutate(graph=rep(list(0),nrow(.)))
names(kobe_setting$graph) <- str_c("kobe_S",1:nrow(kobe_setting))

for(kk in 1:nrow(kobe_setting)){
  kobe_setting$graph[[kk]] <- plot_kobe_gg(FBdata=kobe.ratio,
                                           refs_base=res_MSY$summary,
                                           roll_mean=1,
                                           Btarget="Btarget0",
                                           beta =kobe_setting$beta[kk],
                                           refs.color=rep("black",3),
                                           yscale=1.2,
                                           HCR.label.position=c(1,1),
                                           RP.label=label_name_kobe,
                                           Fratio=kobe.ratio$Fratio,
                                           plot.year=ifelse(plot_year_kobe==0, "all", plot_year_kobe))
  ggsave_SH(str_c(output_folder,"/graph_kobe",kk,".png"),
            kobe_setting$graph[[kk]])
}

kobe_setting$graph[[kk]]
graph_MSY <- c(graph_MSY, kobe_setting$graph)

if(rule_type=="1B" && calc_dynBmsy==TRUE){
  kobe_setting <- kobe_setting %>%
    mutate(DBgraph=rep(list(0),nrow(.)))
  names(kobe_setting$DBgraph) <- str_c("kobe_D",1:nrow(kobe_setting))

  for(kk in 1:nrow(kobe_setting)){
    kobe_setting$DBgraph[[kk]] <- plot_kobe_gg(FBdata=kobe.ratio,
                                               refs_base=res_MSY$summary,
                                               xlab_name="DBratio",
                                               roll_mean=1,
                                               Btarget="Btarget0",
                                               beta =kobe_setting$beta[kk],
                                               refs.color=rep("black",3),
                                               yscale=1.2,
                                               HCR.label.position=c(1,1),
                                               RP.label=label_name_kobe,
                                               Fratio=kobe.ratio$Fratio,
                                               plot.year=ifelse(plot_year_kobe==0, "all", plot_year_kobe))
    ggsave_SH(str_c(output_folder,"/graph_kobe_DB",kk,".png"),
              kobe_setting$DBgraph[[kk]])
  }
  kobe_setting$DBgraph[[kk]]
  graph_MSY <- c(graph_MSY, kobe_setting$DBgraph)
}


#' 漁獲量曲線
# yield curve ----
# 将来予測を重ね書きしているが、この時点では将来予測結果は得られていないためここではシンプルな結果のみを出す？
refs.plot <- dplyr::filter(res_MSY$summary,RP.definition%in%c("Btarget0","Blimit0","Bban0")) %>%
  arrange(desc(SSB))

if(rule_type=="1B"){
  trace_plot <- res_MSY$res_refF$ypr.spr %>%
    rename(ssb.mean=pspr,fmulti=F.range) %>%
    # mutate(ssb.mean=F.range,fmulti=F.range) %>%    #F-base
    mutate(catch.CV=NA)# %>%
  #      dplyr::filter(ssb.mean<3)
  biomass.unit_MSY <- 1
  refs.plot <- refs.plot %>% mutate(SSB=perSPR*100)
  # refs.plot <- refs.plot %>% mutate(SSB=`Fref/Fcur`)   #F-base
}
if(rule_type=="1A"){
  trace_plot <- res_MSY$trace
}

(graph_MSY$yield_curve_simple <- plot_yield(trace_plot,
                                            refs.plot,
                                            refs.label=label_name_kobe,
                                            future=NULL,
                                            past=NULL,labeling=FALSE,
                                            refs.color=rep("black",3),
                                            biomass.unit=biomass.unit_MSY,
                                            AR_select=FALSE,
                                            xlim.scale=0.7,ylim.scale=1.1,
                                            plus_group = res_vpa_MSY$input$plus.group
) + theme_SH(legend.position="right") + scale_x_continuous(labels = scales::comma)+ scale_y_continuous(labels = scales::comma))

if(rule_type=="1B"){

  max.ypr <- max(res_MSY$res_refF$ypr.spr$ypr)
  bhpars <- purrr::map(c(0.6,0.7, 0.8, 0.9, 0.99), function(x) get.ab.bh(x, max.ypr*1.05, biopar_MSY))
  bhvalue <- purrr::map_dfr(bhpars, function(x) tibble(ssb=0:100, R=SRF_BH(x$S0*(0:100)/100, x$a, x$b)), .id="id") %>%
    pivot_wider(names_from=id, values_from=R, names_prefix="h")


  (graph_MSY$yield_curve_simple <- graph_MSY$yield_curve_simple +
      xlab("加入1尾あたりの親魚量（漁獲なしのときの値との相対値, %SPR）") +
      ylab("加入1尾あたりの漁獲量, YPR") +
      geom_ribbon(data=bhvalue, aes(x=ssb,ymax=h5, ymin=h1), alpha=0.05) +
      geom_ribbon(data=bhvalue, aes(x=ssb,ymax=h5, ymin=h2), alpha=0.05) +
      geom_ribbon(data=bhvalue, aes(x=ssb,ymax=h5, ymin=h3), alpha=0.05) +
      geom_ribbon(data=bhvalue, aes(x=ssb,ymax=h5, ymin=h4), alpha=0.05)
  )
  #, lwd=1, col="lightgreen", lty="31"))

  # F-baseの場合
  # (graph_MSY$yield_curve_simple <- graph_MSY$yield_curve_simple +
  #     xlab("漁獲の強さ") +
  #     ylab("加入1尾あたりの漁獲量, YPR"))

}
ggsave_SH(str_c(output_folder,"/graph_yield_curve_simple.png"),
          graph_MSY$yield_curve_simple)

# いろいろな数値結果はallresults_1doMSY.csvで出てきます
out.vpa(res=res_vpa_MSY,
        srres=res_SR_MSY,
        msyres= res_MSY,
        fres_current=res_future_MSY,
        fres_HCR=NULL,
        kobe.ratio=kobe.ratio,
        kobeII=NULL,filename=NULL,
        csvname=str_c(output_folder,"/allresults_1doMSY.csv"),
        pdfname=str_c(output_folder,"/simple_plot.pdf"))

#' ## 最後、いろいろな図の出力
#'

# write final table ----
(final_stat <- list(SBmsy = Btarget0,
                    MSY   = derive_RP_value(res_MSY$summary,"Btarget0")$Catch,
                    Fmsy  = Fmsy0,
                    Umsy  = derive_RP_value(res_MSY$summary,"Btarget0")$U,
                    per_SPR_MSY_biolpar_eq = derive_RP_value(res_MSY$summary,"Btarget0")$perSPR,
                    SBlimit = Blimit0,
                    SBban   = Bban0,
                    "FBratio_last_year"=tail(kobe.ratio,n=1),
                    SR_par_for_future=apply(res_MSY$input$tmb_data$SR_mat[,,c("a","b","rho")],3,mean)))

res_vpa_tibble <- convert_vpa_tibble(res_vpa_MSY)

(graph_MSY$ssb_history <- res_vpa_tibble %>%
    dplyr::filter(stat=="SSB") %>%
    ggplot() +
    geom_path(aes(x=year,y=value/biomass.unit_MSY),color=rgb(73,99,173,maxColorValue=255),lwd=1)+
    xlab("漁期年")+ylab(biomass.name_MSY)+
    #coord_cartesian(ylim=c(0,1.05*max(value/1000)),expand=0)+
    #     scale_y_continuous(expand=expand_scale(mult=c(0,0.05)))+
    theme_SH(base_size=11)+theme(legend.position="top")+
    geom_hline(yintercept=Btarget0/biomass.unit_MSY,col=unlist(format_type()[1,2]),lty=unlist(format_type()[1,3]),lwd=1)+
    geom_hline(yintercept=Blimit0/biomass.unit_MSY, col=unlist(format_type()[2,2]),lty=unlist(format_type()[2,3]),lwd=1)+
    geom_hline(yintercept=Bban0/biomass.unit_MSY,   col=unlist(format_type()[3,2]),lty=unlist(format_type()[3,3]),lwd=1)+
    geom_hline(yintercept=0,            col="black") + scale_y_continuous(labels = scales::comma)
)

if(rule_type=="1B" && calc_dynBmsy==TRUE){
  #  ssbdata <- tibble(year=as.numeric(colnames(dyn_SB_msy$naa)),
  #                    ssb=dyn_SB_msy$ssb/biomass.unit_MSY)
  graph_MSY$ssb_history <- graph_MSY$ssb_history +
    geom_path(data=dplyr::filter(kobe.ratio, year<as.numeric(max(vpa_years))+15), mapping=aes(x=year, y=SBmsy_dyn),
              col="#00533E",lty="51",lwd=1) + scale_y_continuous(labels = scales::comma)
}

(graph_MSY$catch_history <- res_vpa_tibble %>%
    dplyr::filter(stat=="catch") %>%
    ggplot() +
    geom_path(aes(x=year,y=value/biomass.unit_MSY),color=rgb(126,171,117,maxColorValue=255),lwd=1)+
    xlab("漁期年")+ylab(biomass.name_MSY)+
    scale_y_continuous(expand=expand_scale(mult=c(0,0.05)),labels = scales::comma)+
    theme_SH(base_size=11)+theme(legend.position="top")+
    geom_hline(yintercept=0,            col="black"))

(graph_MSY$Fratio_history <- kobe.ratio %>%
    ggplot() +
    geom_path(aes(x=as.numeric(year),y=Fratio),color=rgb(122,78,155,maxColorValue=255),lwd=1)+
    xlab("漁期年")+ylab("漁獲圧の比(F/Fmsy)")+
    scale_y_continuous(expand=expand_scale(mult=c(0,0.05)))+
    theme_SH(base_size=11)+theme(legend.position="top")+
    geom_hline(yintercept=1, col="black",lty=2,lwd=1)+
    geom_hline(yintercept=0, col="black"))

(graph_MSY$HCR <- plot_HCR(Btarget0,Blimit0,Bban0,Ftarget=1,
                           biomass.unit=biomass.unit_MSY,beta=beta_default,
                           Fcurrent=-1, #1/derive_RP_value(res_MSY$summary,"Btarget0")$"Fref2Fcurrent",
                           RP.label=label_name_kobe) + ylim(0,1.3) + scale_x_continuous(labels = scales::comma))

(graph_MSY$HCR_catch <- plot_HCR_by_catch(trace=res_MSY$trace,
                                          fout0.8=res_future_0.8HCR,
                                          SBtarget=Btarget0,SBlim=Blimit0,SBban=Bban0,
                                          Fmsy_vector=Fmsy0,
                                          MSY=derive_RP_value(res_MSY$summary,"Btarget0")$Catch,
                                          M_vector=as.numeric(M_in_MSY),
                                          biomass.unit=biomass.unit_MSY,beta=beta_default,
                                          Pope=is_pope_logical,
                                          RP.label=label_name_kobe) + scale_x_continuous(labels = scales::comma)+ scale_y_continuous(labels = scales::comma))

inch <- 2.54
ggsave(str_c(output_folder,"/graph_ssb_history.png"  ), graph_MSY$ssb_history,
       height=6/inch,width=9/inch)
ggsave(str_c(output_folder,"/graph_catch_history.png"), graph_MSY$catch_history,
       height=6/inch,width=9/inch)
ggsave(str_c(output_folder,"/graph_Fratio_histry.png"), graph_MSY$Fratio_history,
       height=6/inch,width=9/inch)

ggsave(str_c(output_folder,"/graph_HCR.png"), graph_MSY$HCR,
       height=8/inch,width=15/inch)
ggsave(str_c(output_folder,"/graph_HCR_catch.png"), graph_MSY$HCR_catch,
       height=8/inch,width=15/inch)

save(graph_MSY,file=str_c(output_folder,"/graph_all.rda"))

options(warn=old.warning)
options(tibble.width=NULL,tibble.print_max=20)

}

