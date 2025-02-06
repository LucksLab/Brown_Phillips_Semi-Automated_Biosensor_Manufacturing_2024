library(tidyverse)
library(readxl)

#####
# readin data

file1 <- '/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/Figure_1_Raw_Data_sfGFP.xlsx'

sheet_names <- excel_sheets(file1)

for (each_sheet_name in sheet_names) {
  assign(paste0('sfGFP_', each_sheet_name), read_xlsx(file1, sheet = each_sheet_name))
}

file2 <- '/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/Figure_1_Raw_Data_LacZ.xlsx'

sheet_names <- excel_sheets(file2)

for (each_sheet_name in sheet_names) {
  assign(paste0('lacZ_', each_sheet_name), read_xlsx(file2, sheet = each_sheet_name))
}

#####
# function for pivoting data (except for t) longer, calculating max value, calculating t for half max value

pivoting <- function(data0, datLabel) {
  data0 %>%
    pivot_longer(cols = -time, names_to = 'reaction', values_to = 'expression') %>%
    group_by(reaction) %>%
    summarise(max_expression = max(expression),
              time_half_max = time[which.min(abs(expression - max_expression / 2))]) %>%
    mutate(whichData = datLabel) %>%
    return()
}

#####
# for each sfGFP dataframe, apply pivoting function and append to a combined dataframe, then annotate with automated/manual and replicate
sfGFP <- data.frame()
sfGFP_dataframes <- data.frame()

sfGFP_dataframes <- ls()[grepl('sfGFP', ls())]
sfGFP_dataframes <- sfGFP_dataframes[-1] 
sfGFP_dataframes <- sfGFP_dataframes[-1] 

for (each_dataframe in sfGFP_dataframes) {
  if (each_dataframe == 'sfGFP_Raw Data Experimenter 1 (DMB)') {
    sfGFP <- pivoting(get(each_dataframe), datLabel=each_dataframe)
  } else {
    sfGFP <- rbind(sfGFP, pivoting(get(each_dataframe), datLabel=each_dataframe))
  }
}

sf_prep <- sfGFP %>% mutate(automated = ifelse(!grepl('Manual', whichData),'Automated', 'Manual'),
                            replicate = case_when(
                              grepl('AAM', whichData) ~ 1,
                              grepl('TJL', whichData) ~ 2,
                              grepl('DMB', whichData) ~ 3,
                              TRUE ~ 0),
                            dataset = 'sfGFP') %>%
  mutate(replicate = as.factor(replicate))


#####
# for each lacZ dataframe, apply pivoting function and append to a combined dataframe, then annotate with automated/manual and replicate
lacZ <- data.frame()
lacZ_dataframes <- data.frame()

lacZ_dataframes <- ls()[grepl('lacZ', ls())]
lacZ_dataframes <- lacZ_dataframes[-1] 
lacZ_dataframes <- lacZ_dataframes[-1] 

for (each_dataframe in lacZ_dataframes) {
  if (each_dataframe == "lacZ_Raw Data Auto (AAM)") {
    lacZ <- pivoting(get(each_dataframe), datLabel=each_dataframe)
  } else {
    lacZ <- rbind(lacZ, pivoting(get(each_dataframe), datLabel=each_dataframe))
  }
}


LZ_prep <- lacZ %>% mutate(automated = ifelse(!grepl('Manual', whichData),'Automated', 'Manual'),
                           replicate = case_when(
                             grepl('AAM', whichData) ~ 1,
                             grepl('TJL', whichData) ~ 2,
                             grepl('DMB', whichData) ~ 3,
                             TRUE ~ 0),
                           dataset = 'lacZ') %>%
  mutate(replicate = as.factor(replicate))

#####
# plot sfGFP data

sfGFP_base <- sf_prep %>%
  ggplot(aes(x = max_expression, y = time_half_max, color = replicate)) +
  geom_point(aes(pch = automated)) +
  theme_bw() +
  labs(title = 'sfGFP',
       x = 'Max expression',
       y = 'Time to half max expression') +
  theme(legend.position = 'bottom') +
  scale_x_log10()

sfGFP_base

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/sfGFP_base.svg', width = 6, height = 5)

sfGFP_base + facet_wrap(~automated)

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/sfGFP_base_split.svg', width = 10, height = 5)

# add linear fit with equation displayed to LacZ_base plot

sfGFP_linear  <- sfGFP_base + geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  geom_label(aes(x = 1000, y = 75, 
                 label = paste('y =', 
                               round(coef(lm(time_half_max ~ log10(max_expression), data = sf_prep))[2], 2), 
                               'x +', 
                               round(coef(lm(time_half_max ~ log10(max_expression), data = sf_prep))[1], 2),
                               '\nR^2 = ',
                               round(summary(lm(time_half_max ~ log10(max_expression), data = sf_prep))$r.squared, 5)
                 )), color = 'black')

sfGFP_linear

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/sfGFP_linear.svg', width = 6, height = 5)

# add sigmoidal fit with equation and R^2 displayed to sfGFP_base plot

sigmoid <- function(x, a, b, c, d) {
  a / (1 + exp(-b * (x - c))) + d
}

a_value <- quantile(sf_prep$max_expression, 0.99)
b_value <- 0.15
c_value <- mean(sf_prep$time_half_max)
d_value <- quantile(sf_prep$max_expression, 0.01)

sigmoid_fit_data <- tibble(
  time_half_max = seq(0, 300, length.out = 1000)
) %>%
  mutate(max_expression = sigmoid(time_half_max, a_value, b_value, c_value, d_value))

sfGFP_sigmoid <- sfGFP_base + geom_line(data = sigmoid_fit_data, aes(x = max_expression, y = time_half_max), color = 'blue') +
  geom_label(aes(x = 300, y = 210, label = 'x = 99% quantile of x / \n(1 + exp(-0.15 * (y - mean(y))) \n+ 1% quantile of x'), color = 'blue')

sfGFP_sigmoid

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/sfGFP_sigmoid.svg', width = 6, height = 5)

sfGFP_both <- sfGFP_linear + geom_line(data = sigmoid_fit_data, aes(x = max_expression, y = time_half_max), color = 'blue') +
  geom_label(aes(x = 300, y = 210, label = 'x = 99% quantile of x / \n(1 + exp(-0.15 * (y - mean(y))) \n+ 1% quantile of x'), color = 'blue')


sfGFP_both

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/sfGFP_both.svg', width = 6, height = 5)



#####
# plot LacZ data

LZ_prep_for_fitting <- LZ_prep %>%
  filter(max_expression > .3)

LacZ_base <- LZ_prep %>%
  ggplot(aes(x = max_expression, y = time_half_max, color = replicate)) +
  geom_point(aes(pch = automated)) +
  theme_bw() +
  labs(title = 'LacZ',
       x = 'Max expression',
       y = 'Time to half max expression') +
  theme(legend.position = 'bottom') +
  scale_x_log10()

LacZ_base

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/LacZ_base.svg', width = 6, height = 5)

LacZ_base + facet_wrap(~automated)

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/LacZ_base_split.svg', width = 10, height = 5)

# add linear fit with equation displayed to LacZ_base plot

LacZ_linear  <- LacZ_base + geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  geom_label(aes(x = 0.3, y = 220, 
                 label = paste('y =', 
                               round(coef(lm(time_half_max ~ log10(max_expression), data = LZ_prep_for_fitting))[2], 2), 
                               'x +', 
                               round(coef(lm(time_half_max ~ log10(max_expression), data = LZ_prep_for_fitting))[1], 2),
                               '\nR^2 = ',
                               round(summary(lm(time_half_max ~ log10(max_expression), data = LZ_prep_for_fitting))$r.squared, 5)
                               )), color = 'black')

LacZ_linear

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/LacZ_linear.svg', width = 6, height = 5)

# add quadratic fit with equation and R^2 displayed to LacZ_base plot

LacZ_quadratic <- LacZ_base + geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = FALSE, color = 'blue') +
  geom_label(aes(x = 0.5, y = 55, 
                 label = paste('y =', 
                               round(coef(lm(time_half_max ~ poly(log10(max_expression), 2), data = LZ_prep_for_fitting))[3], 1), 
                               'x^2 +',
                               round(coef(lm(time_half_max ~ poly(log10(max_expression), 2), data = LZ_prep_for_fitting))[2], 1), 
                               'x +', 
                               round(coef(lm(time_half_max ~ poly(log10(max_expression), 2), data = LZ_prep_for_fitting))[1], 1),
                               '\nR^2 = ',
                               round(summary(lm(time_half_max ~ poly(log10(max_expression), 2), data = LZ_prep_for_fitting))$r.squared, 3)
                 )), color = 'blue')
             
LacZ_quadratic

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/LacZ_quadratic.svg', width = 6, height = 5)

# both

LacZ_both <- LacZ_linear + geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = FALSE, color = 'blue') +
  geom_label(aes(x = 0.5, y = 55, 
                 label = paste('y =', 
                               round(coef(lm(time_half_max ~ poly(log10(max_expression), 2), data = LZ_prep_for_fitting))[3], 1), 
                               'x^2 +',
                               round(coef(lm(time_half_max ~ poly(log10(max_expression), 2), data = LZ_prep_for_fitting))[2], 1), 
                               'x +', 
                               round(coef(lm(time_half_max ~ poly(log10(max_expression), 2), data = LZ_prep_for_fitting))[1], 1),
                               '\nR^2 = ',
                               round(summary(lm(time_half_max ~ poly(log10(max_expression), 2), data = LZ_prep_for_fitting))$r.squared, 3)
                 )), color = 'blue')

LacZ_both

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/LacZ_both.svg', width = 6, height = 5)
