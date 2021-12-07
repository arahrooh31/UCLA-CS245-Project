library('tidyverse')
setwd('~/Google Drive/My Drive/UCLA/CS245/project/COVID_DGNN_KG/Data/Weather/')

parse <- function(state) {
  data <- read_csv(str_glue('{tolower(state)}.csv'), col_types=cols()) %>%
    select(STATION, DATE, AWND, SNOW, TAVG, PRCP) %>%
    rename(Station=STATION, Date=DATE, Temperature=TAVG, Wind=AWND, Precipitation=PRCP, Snow=SNOW)
  
  by_completeness <- data %>%
    group_by(Station) %>%
    summarize(Temperature=sum(!is.na(Temperature)), Wind=sum(!is.na(Wind)), Precipitation=sum(!is.na(Precipitation)), Snow=sum(!is.na(Snow)), Total=sum(Temperature+Wind+Precipitation+Snow)) %>%
    # arrange(desc(Temperature), desc(Wind), desc(Precipitation), desc(Snow))
    arrange(desc(Total))
  
  most_complete_station <- by_completeness[[1,1]]

  return(data %>%
           filter(Station==most_complete_station) %>%
           select(-Station) %>%
           add_column(State=state, .before=1)
         )
}

states <- c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA',
            'HI','ID','IL','IN','IA','KS','KY','LA','ME','MD',
            'MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ',
            'NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
            'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
fips <- c('01','02','04','05','06','08','09','10','12','13',
          '15','16','17','18','19','20','21','22','23','24',
          '25','26','27','28','29','30','31','32','33','34',
          '35','36','37','38','39','40','41','42','44','45',
          '46','47','48','49','50','51','53','54','55','56')
names(fips) <- states
names(states) <- fips
data <- reduce(states[-1],
               function(acc, state) bind_rows(acc, parse(state)),
               .init=parse(states[1]))
temperature_tertiles <- quantile(data$Temperature, 0:3/3, na.rm=T)
wind_tertiles <- quantile(data$Wind, 0:3/3, na.rm=T)
precipitation_median <- quantile(data$Precipitation, 0:2/2, na.rm=T)
snow_median <- quantile(data$Snow, 0:2/2, na.rm=T)

data.discretized <- data %>%
  mutate(Temperature=ntile(Temperature, 3),
         Wind=ntile(Wind, 3),
         Precipitation=ntile(Precipitation, 2),
         Snow=ntile(Snow, 2)
         )

# map to ids in our heterogeneous graph
temperature_mapping <- 1520:1522
wind_mapping <- 1523:1525
precipitation_mapping <- 1526:1527
snow_mapping <- 1528:1529

dates <- sort(unique(data.discretized$Date))
add_factors <- function(df, state, factors)
  reduce(factors,
         function(d, f) add_row(d, State=fips[state], Factor=f),
         .init=df)

tbl_risk_factors <- function(data) {
  df <- tibble(State=character(), Factor=integer())
  for (i in seq_len(nrow(data))) {
    row <- data[i,]
    factors <- c(ifelse(is.na(row$Temperature), NA, temperature_mapping[row$Temperature]),
                 ifelse(is.na(row$Wind), NA, wind_mapping[row$Wind]),
                 ifelse(is.na(row$Precipitation), NA, precipitation_mapping[row$Precipitation]),
                 ifelse(is.na(row$Snow), NA, snow_mapping[row$Snow]))
    df <- add_factors(df, row$State, na.omit(factors))
  }
  return(df)
}

# write csv risk factor files for each day
for (date in format(dates, '%Y-%m-%d')) {
  path <- str_glue('risk_factors/{date}.csv')
  df <- tbl_risk_factors(filter(data.discretized, format(Date, '%Y-%m-%d')==date))
  write_csv(df, path)
}

# plot weather data along with confirmed cases
library(ggplot2)
data %>%
  filter(State < 'LA') %>%
  ggplot(mapping=aes(Date, Temperature, color=State)) +
    geom_line(alpha=0.8)
data %>%
  filter(State >= 'LA' & State < 'OH') %>%
  ggplot(mapping=aes(Date, Temperature, color=State)) +
  geom_line(alpha=0.5)
data %>%
  filter(State >= 'OH') %>%
  ggplot(mapping=aes(Date, Temperature, color=State)) +
  geom_line(alpha=0.5)
data %>%
  filter(State < 'LA') %>%
  ggplot(mapping=aes(Date, Wind, color=State)) +
  geom_line(alpha=0.5)
data %>%
  filter(State >= 'LA' & State < 'OH') %>%
  ggplot(mapping=aes(Date, Wind, color=State)) +
  geom_line(alpha=0.7)
data %>%
  filter(State >= 'OH') %>%
  ggplot(mapping=aes(Date, Wind, color=State)) +
  geom_line(alpha=0.5)


# write correlations for risk factors vs confirmed cases
cases <- read_csv('covid_cases_location_by_dates.csv', col_types=cols()) %>%
  select(c(-1,-2, -130, -129, -128, -127, -126, -125, -124)) %>%
  filter(FIPS %% 1000 == 0) %>%
  mutate(FIPS = str_pad(FIPS %/% 1000, 2, pad='0'))

cor.factor <- function(fact, val)
  ifelse(!is.na(fact) & fact == val, 1, 0)
correlation <- function(state.cases, state.factor, val) {
  state.factors.clean <- cor.factor(state.factor, val)
  if (length(unique(state.factors.clean)) == 1)
    return(0)
  c <- cor(state.cases, state.factors.clean)
  if (is.na(c)) print(val)
  return(ifelse(is.na(c), 0, c))
}

# plot some cases vs factors for specific states
plot_state_cases <- function(state) {
  state.cases <- rev(unlist(cases[state,2:121]))
  data %>%
    filter(State == states[unlist(cases[state,1])]) %>%
    select(-State) %>%
    mutate(Cases = state.cases / max(state.cases),
           Temperature = Temperature / max(Temperature),
           Wind = Wind / max(Wind),
           Precipitation = Precipitation / max(Precipitation),
           Snow = Snow / max(Snow)) %>%
           # Temperature = ifelse(max(Temperature)==0, 0, Temperature / max(Temperature)),
           # Wind = ifelse(max(Wind)==0, 0, Wind / max(Wind)),
           # Precipitation = ifelse(max(Precipitation)==0, 0, Precipitation / max(Precipitation)),
           # Snow = ifelse(max(Snow)==0, 0, Snow / max(Snow))) %>%
    pivot_longer(!Date, names_to='Factor', values_to='Value', values_drop_na = T) %>%
    ggplot(mapping=aes(Date, Value, color=Factor)) +
      geom_line() +
      ylab('Normalized Values') +
      ggtitle(str_glue('{states[unlist(cases[state,1])]} State')) +
      theme(plot.title = element_text(hjust = 0.5))
}

for (i in (1:50)[c(-9, -31)]) { # exclude DC and NE
  plot_state_cases(i)
}
plot_state_cases(1)
plot_state_cases(2)
plot_state_cases(3)
plot_state_cases(4)
plot_state_cases(5)
plot_state_cases(6)
plot_state_cases(7)
plot_state_cases(8)
plot_state_cases(10)
plot_state_cases(11)
plot_state_cases(12)
plot_state_cases(13)
plot_state_cases(14)
plot_state_cases(15)
plot_state_cases(16)
plot_state_cases(17)
plot_state_cases(18)
plot_state_cases(19)
plot_state_cases(20)
plot_state_cases(21)
plot_state_cases(22)
plot_state_cases(23)
plot_state_cases(24)
plot_state_cases(25)
plot_state_cases(26)
plot_state_cases(27)
plot_state_cases(28)
plot_state_cases(29)
plot_state_cases(30)
plot_state_cases(31)
plot_state_cases(32)
plot_state_cases(33)
plot_state_cases(34)
plot_state_cases(35)
plot_state_cases(36)
plot_state_cases(37)
plot_state_cases(38)
plot_state_cases(39)
plot_state_cases(40)
plot_state_cases(41)
plot_state_cases(42)
plot_state_cases(43)
plot_state_cases(44)
plot_state_cases(45)
plot_state_cases(46)
plot_state_cases(47)
plot_state_cases(48)
plot_state_cases(49)
plot_state_cases(50)

df <- tibble(State=character(), Factor=integer(), Correlation=double())
for (i in (1:50)[-9]) { # exclude DC
  fip <- unlist(cases[i,1])
  state.cases <- rev(unlist(cases[i,2:121]))
  state.factors <- data.discretized %>%
    filter(State == states[fip])
  df <- df %>%
    add_row(State=fip,
            Factor=1520,
            Correlation=correlation(state.cases, state.factors$Temperature, 1)) %>%
    add_row(State=fip,
            Factor=1521,
            Correlation=correlation(state.cases, state.factors$Temperature, 2)) %>%
    add_row(State=fip,
            Factor=1522,
            Correlation=correlation(state.cases, state.factors$Temperature, 3)) %>%
    add_row(State=fip,
            Factor=1523,
            Correlation=correlation(state.cases, state.factors$Wind, 1)) %>%
    add_row(State=fip,
            Factor=1524,
            Correlation=correlation(state.cases, state.factors$Wind, 2)) %>%
    add_row(State=fip,
            Factor=1525,
            Correlation=correlation(state.cases, state.factors$Wind, 3)) %>%
    add_row(State=fip,
            Factor=1526,
            Correlation=correlation(state.cases, state.factors$Precipitation, 1)) %>%
    add_row(State=fip,
            Factor=1527,
            Correlation=correlation(state.cases, state.factors$Precipitation, 2)) %>%
    add_row(State=fip,
            Factor=1528,
            Correlation=correlation(state.cases, state.factors$Snow, 1)) %>%
    add_row(State=fip,
            Factor=1529,
            Correlation=correlation(state.cases, state.factors$Snow, 2))
}
write_csv(df, 'correlation_matrix.csv', na='0')

factor_name <- c('Low Temp', 'Med Temp', 'High Temp', 'Low Wind', 'Med Wind', 'High Wind', 'No Rain', 'Rain', 'No Snow', 'Snow')
names(factor_name) <- 1520:1529
# plot box plots
df %>%
  ggplot(aes(as.factor(factor_name[paste(Factor)]), Correlation)) +
    geom_boxplot() +
    xlab('Risk Factor')

df %>%
  ggplot(aes(as.factor(states[State]), Correlation, color=as.factor(factor_name[paste(Factor)]))) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x='State', color='Risk Factor')

