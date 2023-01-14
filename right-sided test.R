# S 제약 회사에서 새로운 진통제를 개발하였다. 새로운 진통제 지속효과가 5시간 이상이라고 말할 수 있는가? #

pain <- read_csv("C:\\05_3.painkiller.csv",
                 col_names = TRUE,
                 na = ".") %>%
  mutate_if(is.character, as_factor)

pain

str(pain)
glimpse(pain)
skim(pain)

pain %>%
  get_summary_stats(pscore)

pain %>%
  summarize(sample_size = n(),
            mean = mean(pscore),
            sd = sd(pscore),
            minimum = min(pscore),
            lower_quartile = quantile(pscore, 0.25),
            median = median(pscore),
            upper_quartile = quantile(pscore, 0.75),
            max = max(pscore))

pain %>%
  ggplot(aes(y=pscore))+
  geom_boxplot()+
  labs(y = "pscore")

pain %>%
  identify_outliers(pscore)

pain %>%
  shapiro_test(pscore)

pain %>%
  t_test(formula = pscore ~ 1,
         alternative = "greater",
         mu = 5.0,
         conf.level = 0.95,
         detailed = TRUE) #우측검정

# 결론 >> p-value가 0.05이상임