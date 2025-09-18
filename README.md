# ***Is there a Cross-Market Heterogeneity in Fund Responses to EPS forecasts?***

Previous studies suggest that EPS forecasts (Pástor et al. 2008, Da and Warachka 2009), as a proxy for market expectations of future cash flows, play an important role in stock prices and investment decisions (Campbell and Vuolteenaho 2004). Fund managers may adjust their portfolios based on these forecasts— increasing their holdings of stocks with strong expected growth while reducing exposure to those with weaker prospects (Lan and Wermers 2024). Meanwhile, Differences between U.S. and non-U.S. stocks—particularly in market structures, regulatory environments, financial reporting transparency (Gelos and Wei 2005), FX volatility (Camanho et al. 2022), and home bias (Coval and Moskowitz 1999, De Marco et al. 2022, Sialm et al. 2020)—may affect investors' trade-offs in investment decisions. We are interested in whether heterogeneity exists in fund managers‘ response to earning estimates between U.S. and non-U.S. stocks. As far as we know, no such research has been conducted so far.

We propose the following two hypotheses to examine how mutual fund holdings respond to EPS forecasts and whether such responses vary between U.S. and non-U.S. stocks.

- **H1:** Fund holdings respond positively to EPS forecasts, with heterogeneity for U.S. versus non-U.S. stocks.
- **H2:** Cross-market heterogeneity in fund responses to EPS forecasts influences fund performance.

In the Data section, we describe the data sources used in our study and how they are related. In the Methodology section, we present the regression model and steps required to test the hypotheses. In the Report section, we present the empirical results in detail. In the Limitations section, we list some key areas for improvement.

### Data

Our sample of U.S. actively managed mutual funds is constructed from the intersection of the Thomson Reuters mutual fund holdings database and the CRSP mutual fund database, linked via MFLINKS from WRDS. Thomson Reuters provides detailed information on fund characteristics and equity holdings of common stocks at a quarterly frequency, including the number of shares held for each security. CRSP supplies monthly net return data for the corresponding funds. For earnings expectations, we obtain analysts’ EPS forecasts from the Thomson Reuters IBES database. The combined dataset allows us to match fund holdings with both realized returns and contemporaneous EPS forecasts, facilitating the examination of how actively managed funds adjust their equity positions in response to analysts’ earnings expectations across U.S. and non-U.S. stocks. Our monthly four-factor data is taken from Fama-French Portfolios & Factors. Our sample period is from the beginning of 2015 to the end of 2024.

### Methodology

🌐 View [here](https://github.com/bing-han-dk/maf900-a2/maf900-a2-methodology.html).

### Report

🌐 View [here](https://github.com/bing-han-dk/maf900-a2/maf900-a2-report.html).

### Limitations

(i) Due to the huge amount of data, we did not regress the changes in fund holdings at the stock level. Instead, we regarded the stocks held in different markets each quarter **as an investment portfolio** and calculated the average level of EPS forecast changes at the portfolio level. (ii) In addition, considering the limited time frame, we did not use rolling regression in the estimation of alpha. (iii) We accumulated all analysts' EPS forecasts in a quarter to generate a quarterly change index to match the fund's quarterly holdings, hence we cannot measure the impact of a single EPS forecast on the fund's holdings.

### Reference

Camanho, N., Hau, H., & Rey, H. (2022). Global portfolio rebalancing and exchange rates. *The Review of Financial Studies*, *35*(11), 5228-5274.

Campbell, J. Y., & Vuolteenaho, T. (2004). Bad beta, good beta. *American Economic Review*, *94*(5), 1249-1275.

Carhart, M. M. (1997). On persistence in mutual fund performance. *The Journal of finance*, *52*(1), 57-82.

Coval, J. D., & Moskowitz, T. J. (1999). Home bias at home: Local equity preference in domestic portfolios. *The Journal of Finance*, *54*(6), 2045-2073.

Da, Z., & Warachka, M. C. (2009). Cashflow risk, systematic earnings revisions, and the cross-section of stock returns. *Journal of Financial Economics*, *94*(3), 448-468.

De Marco, F., Macchiavelli, M., & Valchev, R. (2022). Beyond home bias: International portfolio holdings and information heterogeneity. *The Review of Financial Studies*, *35*(9), 4387-4422.

Fama, E. F., & French, K. R. (1993). Common risk factors in the returns on stocks and bonds. *Journal of financial economics*, *33*(1), 3-56.

Gelos, R. G., & Wei, S. J. (2005). Transparency and international portfolio holdings. *The Journal of Finance*, *60*(6), 2987-3020.

Grønborg, N. S., Lunde, A., Timmermann, A., & Wermers, R. (2021). Picking funds with confidence. *Journal of Financial Economics*, *139*(1), 1-28.

Lan, C., & Wermers, R. (2024). Cashflow timing vs. discount-rate timing: An examination of mutual fund market-timing skills. *Management Science*, *70*(2), 694-713.

Pástor, Ľ., Sinha, M., & Swaminathan, B. (2008). Estimating the intertemporal risk–return tradeoff using the implied cost of capital. *The Journal of Finance*, *63*(6), 2859-2897.

Sialm, C., Sun, Z., & Zheng, L. (2020). Home bias and local contagion: Evidence from funds of hedge funds. *The Review of Financial Studies*, *33*(10), 4771-4810.