Homework 5
================
David
2024-11-09

## Problem 1

Suppose you put 𝑛 people in a room, and want to know the probability
that at least two people share a birthday. For simplicity, we’ll assume
there are no leap years (i.e. there are only 365 days) and that
birthdays are uniformly distributed over the year (which is actually not
the case). Write a function that, for a fixed group size, randomly draws
“birthdays” for each person; checks whether there are duplicate
birthdays in the group; and returns TRUE or FALSE based on the result.

``` r
bdays_sim = function(n) {

  bdays = sample(1:365, size = n, replace = TRUE)

  duplicate = length(unique(bdays)) < n
  
  return(duplicate)

}
```

Next, run this function 10000 times for each group size between 2 and
50. For each group size, compute the probability that at least two
people in the group will share a birthday by averaging across the 10000
simulation runs. Make a plot showing the probability as a function of
group size, and comment on your results.

``` r
sim_res =
  expand_grid(
    n = 2:50,
    iter = 1:10000
  ) |>
  mutate(res = map_lgl(n, bdays_sim)) |>
  group_by(n) |>
  summarise(prob = mean(res))

sim_res |>
  ggplot(aes(x = n, y = prob)) +
  geom_line() +
  theme_minimal() +
  labs(
    title =  "Probability that at least two people have same birthday",
    x = "Number of people",
    y = "Probability"
  )
```

![](homework_5_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Probability of two people having same day of birthday gradually
increases as number of people increase. With the speed of increase going
up from 2 to around 25 and going down with number of people greater than
25. Probability eventually reaches close to 1.00 .

## Problem 2

``` r
norm_test = function(mu) {
  
  norm_distribution = rnorm(30, mean = mu, sd = 5)
  
  table =
    t.test(norm_distribution, mu = 0) |>
    broom::tidy()
  
  return(table)
}

norm_sim =
  expand_grid(
    mu = 0:6,
    iter = 1:5000
  ) |>
  mutate(stat_table = map(mu, norm_test)) |>
  unnest(stat_table) |>
  janitor::clean_names() |>
  select(mu:conf_low) 
```

Make a plot showing the proportion of times the null was rejected (the
power of the test) on the y axis and the true value of 𝜇 on the x axis.

``` r
norm_sim |>
  mutate(
    h = case_when(
      p_value < 0.05 ~ 1,
      p_value >= 0.05 ~ 0
    )
  ) |>
  group_by(mu) |>
  summarise(prob = sum(h) / 5000) |>
  ggplot(aes(x = mu, y = prob)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Probability of Rejecting H0 Hypothesis in t Test",
    x = "Mu",
    y = "Probability"
  )
```

![](homework_5_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

As effect size increases, the power of the test also increases. This is
because larger effect sizes create more distinct differences or stronger
associations, making it easier for statistical tests to detect them. For
instance, in a study with a small sample size, a large effect size may
still yield high power, while a small effect size may result in low
power, risking the failure to detect a true effect.

Make a plot showing average estimate of mu on y axis and true mu on x
axis. Add another layer shwoing the average estimate of mu only in
samples for which the null hypothesis was rejected.

``` r
norm_sim |>
  group_by(mu) |>
  summarise(
    mu_mean_all = mean(estimate),
    mu_mean_1 = mean(estimate[p_value < 0.05])
  ) |>
  ggplot(aes(x = mu)) +
  geom_line(aes(y = mu_mean_all, color = "All Samples")) +
  geom_line(aes(y = mu_mean_1, color = "Rejected Samples")) +
  theme_minimal() +
  labs(
    title = "Average Mu in Tests against Real Mu",
    x = "Mu",
    y = "Average Mu in Tests"
  ) +
  scale_color_manual(
    name = "Sample Type",
    values = c("All Samples" = "blue", "Rejected Samples" = "green4"),
    labels = c("All Samples", "Rejected Samples")
  )
```

![](homework_5_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Across tests where the null hypothesis is rejected, the sample average
of mu is often greater than the true value of mu due to selection bias.
Only samples with larger estimates of mu are likely to achieve
statistical significance, especially for smaller values of mu. This
leads to an inflated average estimate in the subset of rejected tests,
which does not accurately reflect the true mu.

## Problem 3

``` r
homicide = read_csv("homicide-data.csv") |>
  janitor::clean_names()
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The dataset has 52179 observation and 12 variables, including report
date, victim information, location of homicide and disposition status.

``` r
homicide =
  homicide |>
  mutate(city_state = paste(city, state, sep = ",")) |>
  group_by(city_state) |>
  summarise(
    total = n(),
    unsolved = sum(disposition %in% c("Closed without arrest", "Open/No arrest"))
  )
```

Function of prop.test used in below chart and plot. (applied
**broom::tidy()** here instead of below code chunks)

``` r
test = function(unsolved, total){
  
  table =
    prop.test(unsolved, total, p = NULL, alternative = "two.sided", conf.level = 0.95, correct = TRUE) |>
    broom::tidy() |>
    select(estimate, conf.low, conf.high)
  
  return(table)
}
```

For the city of Baltimore, MD, estimate the proportion of homicides that
are unsolved; save the output as an R object, pull the estimated
proportion and confidence intervals from the resulting tidy dataframe.

``` r
homicide |>
  filter(city_state == "Baltimore,MD") |>
  mutate(table = map2(unsolved, total, \(x, y) test(unsolved = x, total = y))) |>
  pull(table) |>
  knitr::kable()
```

<table class="kable_wrapper">
<tbody>
<tr>
<td>

|  estimate |  conf.low | conf.high |
|----------:|----------:|----------:|
| 0.6455607 | 0.6275625 | 0.6631599 |

</td>
</tr>
</tbody>
</table>

Now for each of the cities in your dataset, and extract both the
proportion of unsolved homicides and the confidence interval for each.
List columns and unnest as necessary to create a tidy dataframe with
estimated proportions and CIs for each city.

``` r
homicide =
  homicide |>
  group_by(city_state) |>
  summarise(table = map2(unsolved, total, \(x, y) test(unsolved = x, total = y))) |>
  unnest(table)
```

    ## Warning: There was 1 warning in `summarise()`.
    ## ℹ In argument: `table = map2(unsolved, total, function(x, y) test(unsolved = x,
    ##   total = y))`.
    ## ℹ In group 49: `city_state = "Tulsa,AL"`.
    ## Caused by warning in `prop.test()`:
    ## ! Chi-squared approximation may be incorrect

``` r
homicide |>
  knitr::kable()
```

| city_state        |  estimate |  conf.low | conf.high |
|:------------------|----------:|----------:|----------:|
| Albuquerque,NM    | 0.3862434 | 0.3372604 | 0.4375766 |
| Atlanta,GA        | 0.3833505 | 0.3528119 | 0.4148219 |
| Baltimore,MD      | 0.6455607 | 0.6275625 | 0.6631599 |
| Baton Rouge,LA    | 0.4622642 | 0.4141987 | 0.5110240 |
| Birmingham,AL     | 0.4337500 | 0.3991889 | 0.4689557 |
| Boston,MA         | 0.5048860 | 0.4646219 | 0.5450881 |
| Buffalo,NY        | 0.6122841 | 0.5687990 | 0.6540879 |
| Charlotte,NC      | 0.2998544 | 0.2660820 | 0.3358999 |
| Chicago,IL        | 0.7358627 | 0.7239959 | 0.7473998 |
| Cincinnati,OH     | 0.4452450 | 0.4079606 | 0.4831439 |
| Columbus,OH       | 0.5304428 | 0.5002167 | 0.5604506 |
| Dallas,TX         | 0.4811742 | 0.4561942 | 0.5062475 |
| Denver,CO         | 0.5416667 | 0.4846098 | 0.5976807 |
| Detroit,MI        | 0.5883287 | 0.5687903 | 0.6075953 |
| Durham,NC         | 0.3659420 | 0.3095874 | 0.4260936 |
| Fort Worth,TX     | 0.4644809 | 0.4222542 | 0.5072119 |
| Fresno,CA         | 0.3470226 | 0.3051013 | 0.3913963 |
| Houston,TX        | 0.5074779 | 0.4892447 | 0.5256914 |
| Indianapolis,IN   | 0.4493192 | 0.4223156 | 0.4766207 |
| Jacksonville,FL   | 0.5111301 | 0.4820460 | 0.5401402 |
| Kansas City,MO    | 0.4084034 | 0.3803996 | 0.4370054 |
| Las Vegas,NV      | 0.4141926 | 0.3881284 | 0.4407395 |
| Long Beach,CA     | 0.4126984 | 0.3629026 | 0.4642973 |
| Los Angeles,CA    | 0.4900310 | 0.4692208 | 0.5108754 |
| Louisville,KY     | 0.4531250 | 0.4120609 | 0.4948235 |
| Memphis,TN        | 0.3190225 | 0.2957047 | 0.3432691 |
| Miami,FL          | 0.6048387 | 0.5685783 | 0.6400015 |
| Milwaukee,wI      | 0.3614350 | 0.3333172 | 0.3905194 |
| Minneapolis,MN    | 0.5109290 | 0.4585150 | 0.5631099 |
| Nashville,TN      | 0.3624511 | 0.3285592 | 0.3977401 |
| New Orleans,LA    | 0.6485356 | 0.6231048 | 0.6731615 |
| New York,NY       | 0.3875598 | 0.3494421 | 0.4270755 |
| Oakland,CA        | 0.5364308 | 0.5040588 | 0.5685037 |
| Oklahoma City,OK  | 0.4851190 | 0.4467861 | 0.5236245 |
| Omaha,NE          | 0.4132029 | 0.3653146 | 0.4627477 |
| Philadelphia,PA   | 0.4478103 | 0.4300380 | 0.4657157 |
| Phoenix,AZ        | 0.5514223 | 0.5184825 | 0.5839244 |
| Pittsburgh,PA     | 0.5340729 | 0.4942706 | 0.5734545 |
| Richmond,VA       | 0.2634033 | 0.2228571 | 0.3082658 |
| Sacramento,CA     | 0.3696809 | 0.3211559 | 0.4209131 |
| San Antonio,TX    | 0.4285714 | 0.3947772 | 0.4630331 |
| San Bernardino,CA | 0.6181818 | 0.5576628 | 0.6753422 |
| San Diego,CA      | 0.3796095 | 0.3354259 | 0.4258315 |
| San Francisco,CA  | 0.5067873 | 0.4680516 | 0.5454433 |
| Savannah,GA       | 0.4674797 | 0.4041252 | 0.5318665 |
| St. Louis,MO      | 0.5396541 | 0.5154369 | 0.5636879 |
| Stockton,CA       | 0.5990991 | 0.5517145 | 0.6447418 |
| Tampa,FL          | 0.4567308 | 0.3881009 | 0.5269851 |
| Tulsa,AL          | 0.0000000 | 0.0000000 | 0.9453792 |
| Tulsa,OK          | 0.3310463 | 0.2932349 | 0.3711192 |
| Washington,DC     | 0.4379182 | 0.4112495 | 0.4649455 |

**Tulsa,AL only has one case so the estimate might not be correct.**

Create a plot that shows the estimates and CIs for each city – check out
geom_errorbar for a way to add error bars based on the upper and lower
limits. Organize cities according to the proportion of unsolved
homicides.

``` r
homicide |>
  ggplot(aes(x = fct_reorder(city_state, estimate), y = estimate)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "black") +
  coord_flip() +
  labs(
    title = "Proportion of Unsolved Homicides by City",
    x = "City",
    y = "Estimate of Proportion Unsolved"
  ) +
  theme_minimal()
```

![](homework_5_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

**Tulsa,AL only has one case so the estimate might not be correct.**
