library(ggplot2)
library(RColorBrewer)
library(stringr)
library(xlsx)


pal <- brewer.pal(n = 5, name = 'Dark2')
pal2 <- brewer.pal(n = 3, name = 'Set2')
  
setwd('C:/Users/Администратор/Desktop/covid_data')
covid_official <- read.csv("owid-covid-data.csv")
keywords <- c('covid','smell', 'throat', 'fewer', 'dyspnea', 'cough', 'rheum', 'headache')
strains_date <- c('2020-09-20', '2020-05-15', '2020-11-15', '2020-10-15', '2021-11-09')
strains_name <- c('alpha', 'beta', 'gamma', 'delta', 'omicron')
countries <- list('russia', 'france', 'united States', 'india', 'united Kingdom', 'south Africa', 'brazil')

     
a <- (covid_official[covid_official$location == 'Russia' & covid_official$date<='31-10-2020'& covid_official$date>='01-10-2020',])
sum(a$new_cases)
setwd('C:/Users/Администратор/Desktop/covid_data')
write.xlsx(x = corrs[[7]], file = 'cor.xlsx',sheetName = 'Лист 1', )
corrs <- list()
corrs[[1]] <-calculate('russia')
corrs[[2]] <-calculate('france')
corrs[[3]] <-calculate('united States')
corrs[[4]] <-calculate('india')
corrs[[5]] <-calculate('united Kingdom')
corrs[[6]] <-calculate('south Africa')
corrs[[7]] <-calculate('brazil')


corrs_by_keyword <- list()

for (i in 1:length(keywords)) {
  setwd('C:/Users/Администратор/Desktop/covid_data/by_keyword')
  corrs_by_keyword[[i]] <- data.frame()
  for (j in 1:7) {
    corrs_by_keyword[[i]][j,1] <- countries[j]
    corrs_by_keyword[[i]][j,2] <- corrs[[j]][i,2]
    corrs_by_keyword[[i]][j,3] <- corrs[[j]][i,3]
  }
  
  write.xlsx(x = corrs_by_keyword[[i]], file = paste0(keywords[i], '.xlsx'))
}




calculate <- function(country_name) {
  country <- country_name
  setwd(paste0('C:/Users/Администратор/Desktop/covid_data/', country))
  return_data <- data.frame()
  for (i in 1:length(keywords)) {
    print(keywords[i])
    gt <-
      read.csv(
        file = paste0(keywords[i], '_', country, '.csv'),
        skip = 1,
        col.names = list("date", "popularity")
      )
    returned <- plots_and_corr(
      google_trends = gt,
      official_data = covid_official[covid_official$location == str_to_title(country), ],
      shift = 0,
      title = paste0(str_to_title(keywords[i]), ', ', str_to_title(country))
    )
    
    return_data[i,1] <- keywords[i]
    return_data[i,2] <- returned[1]
    return_data[i,3] <- returned[2]   
  }
  colnames(return_data) <- c('wordkey', 'pearson', 'spearman')
  return(return_data)
}

plots_and_corr <- function(google_trends, official_data, shift, title){
  google_trends$date <- as.Date(google_trends$date)
  official_data$date <- as.Date(official_data$date)
  
  official_data$new_cases[is.na(official_data$new_cases)] <- 0
  
  a <- 1
  for (j in 1:(length(google_trends$date)-1)) {
    google_trends$new_cases[j] <- 0
    for (i in a:length(official_data$date)) {
      if(official_data$date[i] > google_trends$date[j] & official_data$date[i] < google_trends$date[j+1]){
        google_trends$new_cases[j] <- google_trends$new_cases[j] + official_data$new_cases[i] 
      } else if(official_data$date[i] > google_trends$date[j+1]){
        a<-i-1
        break
      }
    }
  }
  scale <- max(official_data$new_cases[!is.na(official_data$new_cases_smoothed)])/150
  
  google_trends$popularity <- google_trends$popularity * scale
  
  for (i in 1:length(google_trends$new_cases)) {
    if (is.na(google_trends$new_cases[i])) {
      if (i > 1) {
        google_trends$new_cases[i] <-
          (google_trends$new_cases[[i - 1]] + google_trends$new_cases[[i + 1]]) /
          2
      } else{
        google_trends$new_cases[i] <- google_trends$new_cases[[i + 1]] / 2
      }
    }    
  }
  
  trends_shifted <- list()
  plot(new_cases_smoothed ~ date, official_data, lwd = 1, main = title)
  min <- min(google_trends$popularity)
  for (i in 1:length(google_trends$popularity)) {
    google_trends$popularity[i] <- google_trends$popularity[i]-0.9*min
    if(i<shift+1){
      trends_shifted[i] <- NA
      google_trends$new_cases[i] <- NA
      
    } else{
      trends_shifted[i] <- google_trends$popularity[i-shift]
    }
  }
  
  
  lines(google_trends$date, google_trends$popularity, col = pal2[1], lwd = 2)
  lines(google_trends$date, trends_shifted, col = pal2[2], lwd = 2)
  abline(v =  as.Date(strains_date), col= pal, lty = 2, lwd = 3)
  
  legend("top", strains_name,
         lty = 2,
         lwd = 2,
         col = pal)
  print(length(google_trends$new_cases[!is.na(google_trends$new_cases)]))
  print((google_trends$new_cases))
  
  print(length(trends_shifted[!is.na(trends_shifted)]))
  
  ccf(google_trends$new_cases[!is.na(google_trends$new_cases)], as.numeric(trends_shifted[!is.na(trends_shifted)]), main = paste('Crosscorrelation:',title))
  data_return <- c(cor(google_trends$new_cases[!is.na(google_trends$new_cases)], as.numeric(trends_shifted[!is.na(trends_shifted)]), method = 'pearson'),
                   cor(google_trends$new_cases[!is.na(google_trends$new_cases)], as.numeric(trends_shifted[!is.na(trends_shifted)]), method = 'spearman'))
  print(data_return[1])
  print(data_return[2])
  #print(cor(google_trends$new_cases[!is.na(google_trends$new_cases)], as.numeric(trends_shifted[!is.na(trends_shifted)]), method = 'pearson'))
  #print(cor(google_trends$new_cases[!is.na(google_trends$new_cases)], as.numeric(trends_shifted[!is.na(trends_shifted)]), method = 'spearman'))
  return(data_return)
}



gt_throat <- read.csv("throat_russia.csv", skip = 1, col.names = list("date", "popularity"))
gt_fewer <- read.csv("fewer_russia.csv", skip = 1, col.names = list("date", "popularity"))
gt_dyspnea <- read.csv("dyspnea_russia.csv", skip = 1, col.names = list("date", "popularity"))
gt_cough <- read.csv("cough_russia.csv", skip = 1, col.names = list("date", "popularity"))
 

plots_and_corr(gt_russia_smell, covid_russia, 4, 'Обоняние')
plots_and_corr(gt_russia_throat, covid_russia, 0, 'Боль в горле')
plots_and_corr(gt_russia_fewer, covid_russia, 4, 'Температура при')
plots_and_corr(gt_dyspnea_russia, covid_russia, 4, 'Одышка')
plots_and_corr(gt_cough_russia, covid_russia, 0, 'Кашель')
