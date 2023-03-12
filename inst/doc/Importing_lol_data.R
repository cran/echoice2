## ----v02setup_vignette, include=FALSE-------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", out.width = "100%",
  fig.width = 7, fig.height = 4, dev = "CairoPNG", dpi = 150, fig.path = "",
  message = FALSE, warning = FALSE, error = FALSE
)
library(echoice2)


## ----2install_from_cran, eval=FALSE, include=TRUE-------------------------------------------------------------------------------------------------------------
##   install.packages("echoice2")


## ----v02install_from_github, eval=FALSE, include=TRUE---------------------------------------------------------------------------------------------------------
##   remotes::install_github("ninohardt/echoice2")


## ----v02load_echoice2, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------
  library(echoice2)


## ----v02v02show_data------------------------------------------------------------------------------------------------------------------------------------------
  library(bayesm)
  data(camera)
  head(camera[[1]]$X)
  head(camera[[1]]$y)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
  data_tidier <- ec_lol_tidy1(camera) %>% rename(`p`='price')
  data_tidier %>% head


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
  data_tidier_2 <- data_tidier %>% filter(p>0)
  data_tidier_2 %>% head


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
  data_tidier_2 %>% ec_util_dummy_mutualeclusive() %>% filter(mut_ex)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
  data_tidier_3 <- 
    data_tidier_2 %>% ec_undummy(c('canon','sony','nikon','panasonic'), 'brand')
  data_tidier_3 %>% head


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
  data_tidied<-
    data_tidier_3 %>% 
      mutate(across(c(pixels,zoom), ec_undummy_lowhigh))%>% 
      mutate(across(c(swivel,video,wifi), ec_undummy_yesno))
  data_tidied %>% head()


## ----v02summarize_attributes----------------------------------------------------------------------------------------------------------------------------------
  data_tidied %>% ec_summarize_attrlvls


## ----v02total_by_task-----------------------------------------------------------------------------------------------------------------------------------------
  data_tidied %>%
    group_by(id, task) %>%
      summarise(x_total = sum(x), .groups = "keep") %>%
      group_by(x_total) %>% count(x_total)


## ----v02holdout_split-----------------------------------------------------------------------------------------------------------------------------------------
  #randomly assign hold-out group, use seed for reproducible plot
  set.seed(555) 
    data_ho_tasks=
    data_tidied %>%
      distinct(id,task) %>%
      mutate(id=as.integer(id))%>%
      group_by(id) %>%
      summarise(task=sample(task,1), .groups = "keep")
  set.seed(NULL)

  #calibration data
    data_cal= data_tidied %>% mutate(id=as.integer(id)) %>%
      anti_join(data_ho_tasks, by=c('id','task'))
    
  #'hold-out' data
    data_ho= data_tidied %>% mutate(id=as.integer(id)) %>%
      semi_join(data_ho_tasks, by=c('id','task'))


## ----v02estimating all final models, eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------------------------------------------------------------
##   #compensatory
##   out_camera_compensatory <- dd_est_hmnl(data_tidied)
##   dir.create("draws")
##   save(out_camera_compensatory,file='draws/out_camera_cal.rdata')
## 
##   #conjunctive screening
##   out_camera_screening <- dd_est_hmnl_screen(data_tidied)
##   save(out_camera_screening,file='draws/out_camera_screening.rdata')


## ----v02loading draws from previous run, message=FALSE, warning=FALSE, eval=TRUE, include=FALSE---------------------------------------------------------------
  load('draws/out_camera_cal.rdata')
  load('draws/out_camera_screening.rdata')


## ----v02traceplot_vd, fig.height=4, fig.width=7---------------------------------------------------------------------------------------------------------------
  out_camera_compensatory %>% ec_trace_MU(burnin = 100)


## ----v02traceplot_vd-screen, fig.height=4, fig.width=7--------------------------------------------------------------------------------------------------------
  out_camera_screening %>% ec_trace_MU(burnin = 100)


## ----v02outlier_check, fig.height=4, fig.width=6--------------------------------------------------------------------------------------------------------------
  dd_LL(out_camera_compensatory ,data_cal, fromdraw = 3000) %>% 
    apply(1,mean) %>% tibble(LL=.) %>% 
    ggplot(aes(x=LL)) + 
      geom_density(fill="lightgrey", linewidth=1) + theme_minimal() +
      xlab("Individual average Log-Likelihood")


## ----v02LMD---------------------------------------------------------------------------------------------------------------------------------------------------
  list(compensatory = out_camera_compensatory,
       conjunctive  = out_camera_screening) %>%
    purrr::map_dfr(ec_lmd_NR, .id = 'model') %>%
    filter(part==1) %>% select(-part)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
ho_prob_dd=
    data_ho %>%
      prep_newprediction(data_cal) %>%
        dd_dem(out_camera_compensatory, prob=TRUE)

ho_prob_ddscreen=
    data_ho %>%
      prep_newprediction(data_cal) %>%
        dd_dem_sr(out_camera_screening, prob=TRUE)

hit_probabilities=c()

hit_probabilities$compensatory=
ho_prob_dd %>%
  vd_dem_summarise() %>%
  select(id,task,alt,x,p,`E(demand)`) %>%
  group_by(id,task) %>% group_split() %>%
  purrr::map_dbl(. %>%  select(c(x,`E(demand)`)) %>% 
                        add_row(x=1-sum(.$x),`E(demand)`=1-sum(.$`E(demand)`)) %>%
                        summarise(hp=sum(x*`E(demand)`))%>% unlist) %>% mean()

hit_probabilities$screening=
ho_prob_ddscreen %>%
  vd_dem_summarise() %>%
  select(id,task,alt,x,p,`E(demand)`) %>%
  group_by(id,task) %>% group_split() %>%
  purrr::map_dbl(. %>%  select(c(x,`E(demand)`)) %>% 
                        add_row(x=1-sum(.$x),`E(demand)`=1-sum(.$`E(demand)`)) %>%
                        summarise(hp=sum(x*`E(demand)`))%>% unlist) %>% mean()

hit_probabilities


## ----v02vd_mu, fig.height=4, fig.width=6----------------------------------------------------------------------------------------------------------------------
  out_camera_compensatory %>% ec_estimates_MU()


## ----v02vdscreen_mu, fig.height=4, fig.width=6----------------------------------------------------------------------------------------------------------------
  out_camera_screening %>% ec_estimates_MU()


## ----v02estimates_screening, fig.height=4, fig.width=6--------------------------------------------------------------------------------------------------------
  out_camera_screening %>% ec_estimates_screen()

