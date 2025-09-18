library(RPostgres)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(fixest)
library(broom)
library(stargazer)


# PART I


# generate wrds connection 
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='bing_han')


# get hodings data 
res <- dbSendQuery(wrds, "
        WITH eps_raw AS (
            SELECT *,
                   ROW_NUMBER() OVER (
                       PARTITION BY ticker, cusip, fpedats
                       ORDER BY ABS(fpedats - statpers)
                   ) AS rn,
                   curcode as market
            FROM tr_ibes.nstatsum_epsus
            WHERE measure = 'EPS'
              AND estflag = 'P'
              AND fiscalp = 'QTR'
              AND fpi IN ('6','7')
              AND fpedats > statpers
              AND fpedats BETWEEN '2015-01-01' AND '2024-12-31'
        ),
        eps_ranked AS (
            SELECT *,
                   LAG(meanest) OVER (PARTITION BY cusip ORDER BY fpedats) AS prev_meanest
            FROM eps_raw
            WHERE rn = 1
        ),
        EPS AS (
            SELECT cusip, fpedats, curcode, meanest - prev_meanest AS eps_diff, market
            FROM eps_ranked
            WHERE prev_meanest IS NOT NULL
        ),
        XRATE as (
        	SELECT 
        	    a.datadate,
        	    a.tocurd,
        	    CASE 
        	        WHEN a.tocurd = 'USD' THEN 1
        	        ELSE ROUND(a.exratd / b.gbp_usd_rate, 6)
        	    END AS usd_to_currency_rate
        		FROM 
        		    comp_global_daily.g_exrt_dly a
        		JOIN (
        		    SELECT datadate, exratd AS gbp_usd_rate
        		    FROM comp_global_daily.g_exrt_dly
        		    WHERE tocurd = 'USD' AND fromcurd = 'GBP'
        		) b ON a.datadate = b.datadate
        		WHERE 
        		    a.fromcurd = 'GBP'
        		    AND TO_CHAR(a.datadate, 'MM-DD') IN ('03-31', '06-30', '09-30', '12-31')
        		    AND a.datadate BETWEEN '2015-01-01' AND '2024-12-31'
        ),
        HOLDING_ALL as (
        	select rdate
        	--, prdate
        	, fundno
        	, cusip
        	, assets
        	, COALESCE(round(shares*prc), 0) as amt
        	, COALESCE(round(change*prc), 0) as chg_amt
        	from tr_mutualfunds.s12 where prc is not null 
        	and fdate between '2015-01-01' and '2024-12-31'
        	and country = 'UNITED STATES'
        ),
        TARGET as (
        	select distinct rdate, fundno 
        	from HOLDING_ALL inner join EPS 
        	on EPS.cusip = HOLDING_ALL.cusip
        	and EPS.fpedats = HOLDING_ALL.rdate
        	where EPS.market <> 'US'
        ),
        HOLDING as (
        	select HOLDING_ALL.* 
        	from HOLDING_ALL
        	inner join TARGET
        	on TARGET.rdate = HOLDING_ALL.rdate
        	and TARGET.fundno = HOLDING_ALL.fundno
        )
        select HOLDING.fundno
        	, HOLDING.rdate
        	, EPS.market
        	, ROUND(avg(EPS.eps_diff/XRATE.usd_to_currency_rate)::numeric,3) as eps_diff
        	, ROUND(avg(HOLDING.amt/XRATE.usd_to_currency_rate)) as amt
        	, ROUND(avg(HOLDING.chg_amt/XRATE.usd_to_currency_rate)) as chg_amt
        	, ROUND(avg(HOLDING.assets)) as asset
        	from HOLDING
        	inner join EPS 
        	on EPS.cusip = HOLDING.cusip
        	and EPS.fpedats = HOLDING.rdate
        	INNER join XRATE
        	on XRATE.tocurd = EPS.curcode
        	and XRATE.datadate = EPS.fpedats
        	group by HOLDING.fundno, HOLDING.rdate, EPS.market
      ")
fund_hold <- dbFetch(res, n = -1)
dbClearResult(res)
head(fund_hold)

# backup data 
fund_hold_bk<-fund_hold


# data cleaning for invalid asset values 
fund_hold<- fund_hold %>%
  filter(!is.na(asset), is.finite(asset), asset != 0)

# eliminate outliers
fund_hold %>%
  filter(market == "USD") %>%
  ggplot(aes(x = eps_diff)) +
  geom_density(fill = "lightgreen", alpha = 0.6) +
  theme_minimal() +
  labs(title = "EPS Diff Density (Market = USD)",
       x = "eps_diff",
       y = "Density")
q_low <- quantile(fund_hold$eps_diff, 0.01, na.rm = TRUE)
q_high <- quantile(fund_hold$eps_diff, 0.99, na.rm = TRUE)
fund_hold <- fund_hold %>%
  #filter(market == "USD") %>%
  filter(eps_diff >= q_low & eps_diff <= q_high)

# to see which markets should be included
fund_hold %>%
  count(market) %>%
  arrange(desc(n))

# filter data by market and adjust values by log 
fund_hold <- fund_hold %>%
  filter(market %in% c('USD','EUR','CNY','JPY','KRW','CAD','DKK'))%>%
  mutate(
    amt=ifelse(chg_amt==0,0,sign(amt)*log10(abs(amt)+1)),
    chg_amt=ifelse(chg_amt==0,0,sign(chg_amt)*log10(abs(chg_amt)+1)),
    asset = log(asset),
    year = year(ymd(rdate)),
    is_us = if_else(market == 'USD', 1, 0)
    )

#fund_hold <- fund_hold %>% select(-year)

# check the correlation coefficient
fund_hold %>%
  filter(is_us == 0) %>%
  group_by(year) %>%
  summarise(
    cor_eps_logchg = cor(eps_diff, chg_amt, 
                         use = "complete.obs", method = "pearson")
  )

# summaery data by rdate+market group
summary_data <- fund_hold %>%
  group_by(rdate, market, is_us) %>%
  summarise(
    mean_eps_diff = mean(eps_diff, na.rm = TRUE),
    mean_log_chg = mean(chg_amt, na.rm = TRUE),
    count = n()
  ) %>%
  ungroup()

# to see the relations between log chg_amt and mean eps_diff 
ggplot(summary_data,aes(x=mean_eps_diff,y=mean_log_chg,color=market,size=count)) +
  geom_point() +
  labs(
    x = "Mean EPS Diff",
    y = "Mean Log Change Amount",
    color = "Market"
  ) +
  theme_minimal()

# to see the time trend of mean eps_diff among different markets
ggplot(summary_data, aes(x = rdate, y = mean_eps_diff, color = market)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Mean EPS Diff",
    color = "Market"
  ) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "12 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # facet_wrap(~ is_us,labeller = as_labeller(c(`1` = "US", `0` = "Non-US")))
  facet_wrap(~ market)

# regress our model in part one (holding changes ~ eps changes, asset as size for control) 
model_fe<-feols(chg_amt~eps_diff*is_us+asset|rdate+fundno,data=fund_hold) #FE:rdate+fundno
etable(model_fe)



# PART II


# Four-factor data 
res_factor <- dbSendQuery(wrds, "select dateff, mktrf, smb, hml, rf, umd 
                  from ff_all.factors_monthly 
                  where date between '2015-01-01' and '2024-12-31'
      ")
data_factor <- dbFetch(res_factor, n = -1)
dbClearResult(res)
head(data_factor)

# monthly fund retrun data 
res_ret <- dbSendQuery(wrds, "
                  select distinct fundno,caldt, t3.mret
                  from mflinks_all.mflink1 t1 
                  inner join 
                  (select distinct wficn, fundno 
                    from mflinks_all.mflink2 
                    where rdate between '2015-01-01' and '2024-12-31'
                  ) t2
                  on t1.wficn =t2.wficn
                  inner join 
                  (select crsp_fundno, caldt, mret
                    from crsp_q_mutualfunds.monthly_returns
                    where caldt between '2015-01-01' and '2024-12-31'
                  )t3 
                  on t3.crsp_fundno = t1.crsp_fundno
      ")
data_ret <- dbFetch(res_ret, n = -1)
dbClearResult(res)
head(data_ret)


# merge return and factors
data_ret <- data_ret %>%
  mutate(year = year(caldt),
         month = month(caldt))
data_factor <- data_factor %>%
  mutate(year = year(dateff),
         month = month(dateff))
merged_data <- inner_join(data_ret, data_factor, by = c("year", "month"))

# calculate excess monthly return 
merged_data <- merged_data %>%
  mutate(excess_ret = mret - rf)


# drop na 
summary(merged_data[, c("excess_ret", "mktrf", "smb", "hml", "umd")])
cleaned_data <- merged_data %>%
  filter(!is.na(excess_ret)) 


# regress for each fund 
regression_results <- cleaned_data %>%
  group_by(fundno) %>%
  do(tidy(lm(excess_ret ~ mktrf + smb + hml + umd, data = .))) %>%
  ungroup()

# summarize regression results
fund_factor_loadings <- regression_results %>%
  filter(term %in% c("(Intercept)", "mktrf", "smb", "hml", "umd")) %>%
  select(fundno, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  rename(
    alpha = `(Intercept)`,
    beta_mkt = mktrf,
    beta_smb = smb,
    beta_hml = hml,
    beta_umd = umd
  )


fund_q_summary <- fund_hold %>%
  group_by(fundno, rdate) %>%
  summarise(
    total_amt = sum(amt, na.rm = TRUE),
    amt_us = sum(amt[is_us == 1], na.rm = TRUE),
    amt_non_us = sum(amt[is_us == 0], na.rm = TRUE),
    intl_share = amt_non_us / total_amt,
    
    # raw eps
    eps_diff_us = ifelse(amt_us > 0, sum(eps_diff[is_us == 1] * amt[is_us == 1], na.rm = TRUE) / amt_us,NA_real_),
    eps_diff_non_us = ifelse(amt_non_us > 0,sum(eps_diff[is_us == 0] * amt[is_us == 0], na.rm = TRUE) / amt_non_us,NA_real_),
    
    # weighted measures 
    eps_diff_us_wt = eps_diff_us * (1 - intl_share),
    eps_diff_non_us_wt = eps_diff_non_us * intl_share,
    
    # sensitivity measures 
    eps_diff_sen_us = mean((chg_amt / eps_diff)[is_us == 1], na.rm = TRUE),
    eps_diff_sen_non_us = mean((chg_amt / eps_diff)[is_us == 0], na.rm = TRUE)
  ) %>%
  ungroup()%>%
  filter(abs(amt_non_us) > 1e-10)

fund_q_summary<- fund_q_summary %>%
  filter(!is.na(eps_diff_sen_us), is.finite(eps_diff_sen_us), eps_diff_sen_us != 0)%>%
  filter(!is.na(eps_diff_sen_non_us), is.finite(eps_diff_sen_non_us), eps_diff_sen_non_us != 0)

fund_q_summary <- fund_q_summary %>%
  mutate(
    eps_diff_sen_us = as.numeric(scale(eps_diff_sen_us)),
    eps_diff_sen_non_us = as.numeric(scale(eps_diff_sen_non_us))
  )


# intl_share trend
#intl_share_summary <- fund_q_summary %>%
#  group_by(rdate) %>%
#  summarise(mean_intl_share = mean(intl_share, na.rm = TRUE)) %>%
#  ungroup()
#
#ggplot(intl_share_summary, aes(x = rdate, y = mean_intl_share, group = 1)) +
#  geom_line() +
#  geom_point() +
#  labs(x = "rdata", y = "intl_share", title = "intl_share trend") +
#  theme_minimal()


fund_avg_summary <- fund_q_summary %>%
  group_by(fundno) %>%
  summarise(
    eps_diff_us = mean(eps_diff_us, na.rm = TRUE),
    eps_diff_non_us = mean(eps_diff_non_us, na.rm = TRUE),
    eps_diff_us_wt = mean(eps_diff_us_wt, na.rm = TRUE),
    eps_diff_non_us_wt = mean(eps_diff_non_us_wt, na.rm = TRUE),
    eps_diff_sen_us = mean(eps_diff_sen_us, na.rm = TRUE),
    eps_diff_sen_non_us = mean(eps_diff_sen_non_us, na.rm = TRUE),
    intl_share = mean(intl_share, na.rm = TRUE)
  )

fund_avg_summary <- fund_avg_summary %>%
  inner_join(fund_factor_loadings %>% select(fundno, alpha), by = "fundno")

# at raw eps_diff
model_a1 <- lm(alpha ~ eps_diff_sen_us + eps_diff_sen_non_us + intl_share, data = fund_avg_summary)
stargazer(model_a1,
          type = "text",
          title = "Regression Results Comparison",
          digits = 4,
          kep.stat = c("n", "rsq", "adj.rsq"))
