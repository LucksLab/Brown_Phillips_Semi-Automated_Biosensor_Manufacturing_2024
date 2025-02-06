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
# plot sfGFP data, split

model_manual_sfGFP <- lm(time_half_max ~ log10(max_expression), data = sf_prep %>% filter(automated == 'Manual'))

manual_sfGFP_m <- round(coef(model_manual_sfGFP)[2],2)
manual_sfGFP_b <- round(coef(model_manual_sfGFP)[1],8)
manual_sfGFP_R2 <- round(summary(model_manual_sfGFP)$r.squared,2)

model_automated_sfGFP <- lm(time_half_max ~ log10(max_expression), data = sf_prep %>% filter(automated == 'Automated'))

automated_sfGFP_m <- round(coef(model_automated_sfGFP)[2],2)
automated_sfGFP_b <- round(coef(model_automated_sfGFP)[1],8)
automated_sfGFP_R2 <- round(summary(model_automated_sfGFP)$r.squared,2)



sf_prep_with_split_models <- sf_prep %>%
  mutate(
    m = case_when(
    automated == 'Manual' ~ manual_sfGFP_m,
    automated == 'Automated' ~ automated_sfGFP_m
    ),
    b = case_when(
      automated == 'Manual' ~ manual_sfGFP_b,
      automated == 'Automated' ~ automated_sfGFP_b
      ),
    R2 = case_when(
      automated == 'Manual' ~ manual_sfGFP_R2,
      automated == 'Automated' ~ automated_sfGFP_R2),
  )
    

sfGFP_base_split <- sf_prep_with_split_models %>%
  ggplot(aes(x = max_expression, y = time_half_max, color = replicate, pch = automated)) +
  geom_point() +
  theme_bw() +
  labs(title = 'sfGFP',
       x = 'Max expression',
       y = 'Time to half max expression') +
  theme(legend.position = 'bottom') +
  scale_x_log10() + facet_wrap(~automated)

# add linear fit with equation displayed to LacZ_base plot, split by automated/manual



sfGFP_linear_split  <- sfGFP_base_split + geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  geom_label(aes(x = 1000, y = 75, 
                 label = paste('y =', 
                               m, 
                               'x +', 
                               b,
                               '\nR^2 = ',
                               R2
                 )), color = 'black')

sfGFP_linear_split

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/sfGFP_linear_split.svg', width = 10, height = 5)

# add sigmoidal fit with equation and R^2 displayed to sfGFP_base plot

sigmoid <- function(x, a, b, c, d) {
  a / (1 + exp(-b * (x - c))) + d
}

a_value <- quantile(sf_prep$max_expression, 0.99)
b_value <- 0.15
c_value <- mean(sf_prep$time_half_max)
d_value <- quantile(sf_prep$max_expression, 0.01)

sigmoid_fit_data_pre <- tibble(
  time_half_max = seq(0, 300, length.out = 1000)
) %>%
  mutate(max_expression = sigmoid(time_half_max, a_value, b_value, c_value, d_value))
  
sigmoid_fit_data  <- rbind(sigmoid_fit_data_pre, sigmoid_fit_data_pre) %>%
  mutate(automated = c(rep('Automated', 1000), rep('Manual', 1000)))

sfGFP_sigmoid_split <- sfGFP_base_split + geom_line(data = sigmoid_fit_data, aes(x = max_expression, y = time_half_max), color = 'blue') +
  geom_label(aes(x = 300, y = 210, label = 'x = 99% quantile of x / \n(1 + exp(-0.15 * (y - mean(y))) \n+ 1% quantile of x'), color = 'blue')

sfGFP_sigmoid_split

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/sfGFP_sigmoid_split.svg', width = 10, height = 5)

sfGFP_both_split <- sfGFP_linear_split + geom_line(data = sigmoid_fit_data, aes(x = max_expression, y = time_half_max), color = 'blue') +
  geom_label(aes(x = 300, y = 210, label = 'x = 99% quantile of x / \n(1 + exp(-0.15 * (y - mean(y))) \n+ 1% quantile of x'), color = 'blue')


sfGFP_both_split

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/sfGFP_both_split.svg', width = 10, height = 5)



#####
# plot LacZ data


LZ_prep_for_fitting <- LZ_prep %>%
  filter(max_expression > .3) 

model_manual_LacZ <- lm(time_half_max ~ log10(max_expression), data = LZ_prep_for_fitting %>% filter(automated == 'Manual'))

manual_LacZ_m <- round(coef(model_manual_LacZ)[2],2)
manual_LacZ_b <- round(coef(model_manual_LacZ)[1],1)
manual_LacZ_R2 <- round(summary(model_manual_LacZ)$r.squared,2)

model_automated_LacZ <- lm(time_half_max ~ log10(max_expression), data = LZ_prep_for_fitting %>% filter(automated == 'Automated'))

automated_LacZ_m <- round(coef(model_automated_LacZ)[2],2)
automated_LacZ_b <- round(coef(model_automated_LacZ)[1],1)
automated_LacZ_R2 <- round(summary(model_automated_LacZ)$r.squared,2)



p_model_manual_LacZ <- lm(time_half_max ~ poly(log10(max_expression), 2), data = LZ_prep_for_fitting %>% filter(automated == 'Manual'))
p_model_automated_LacZ <- lm(time_half_max ~ poly(log10(max_expression), 2), data = LZ_prep_for_fitting %>% filter(automated == 'Automated'))

p_manual_LacZ_A <- round(coef(p_model_manual_LacZ)[3], 1)
p_automated_LacZ_A <- round(coef(p_model_automated_LacZ)[3], 1)
p_manual_LacZ_B <- round(coef(p_model_manual_LacZ)[2], 1)
p_automated_LacZ_B <- round(coef(p_model_automated_LacZ)[2], 1)
p_manual_LacZ_C <- round(coef(p_model_manual_LacZ)[1], 1)
p_automated_LacZ_C <- round(coef(p_model_automated_LacZ)[1], 1)
p_manual_LacZ_R2 <- round(summary(p_model_manual_LacZ)$r.squared,3)
p_automated_LacZ_R2 <- round(summary(p_model_automated_LacZ)$r.squared,3)



LZ_prep_w_models <- LZ_prep %>%
  mutate(
    m = case_when(
      automated == 'Manual' ~ manual_LacZ_m,
      automated == 'Automated' ~ automated_LacZ_m
    ),
    b = case_when(
      automated == 'Manual' ~ manual_LacZ_b,
      automated == 'Automated' ~ automated_LacZ_b
    ),
    R2 = case_when(
      automated == 'Manual' ~ manual_LacZ_R2,
      automated == 'Automated' ~ automated_LacZ_R2),
    pA = case_when(
      automated == 'Manual' ~ p_manual_LacZ_A,
      automated == 'Automated' ~ p_automated_LacZ_A),
    pB = case_when(
      automated == 'Manual' ~ p_manual_LacZ_B,
      automated == 'Automated' ~ p_automated_LacZ_B),
    pC = case_when(
      automated == 'Manual' ~ p_manual_LacZ_C,
      automated == 'Automated' ~ p_automated_LacZ_C),
    pR2 = case_when(
      automated == 'Manual' ~ p_manual_LacZ_R2,
      automated == 'Automated' ~ p_automated_LacZ_R2),
  )


LacZ_base_split <- LZ_prep_w_models %>%
  ggplot(aes(x = max_expression, y = time_half_max, color = replicate, pch = automated)) +
  geom_point() +
  theme_bw() +
  labs(title = 'LacZ',
       x = 'Max expression',
       y = 'Time to half max expression') +
  theme(legend.position = 'bottom') +
  scale_x_log10() + facet_wrap(~automated)

# add linear fit with equation displayed to LacZ_base split plot

LacZ_linear_split  <- LacZ_base_split + geom_smooth(method = 'lm', se = FALSE, color = 'black') +
  geom_label(aes(x = 0.3, y = 220, 
                 label = paste('y =', 
                               m,
                               'x +', 
                               b,
                               '\nR^2 = ',
                               R2
                               )), color = 'black')

LacZ_linear_split

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/LacZ_linear_split.svg', width = 10, height = 5)

# add quadratic fit with equation and R^2 displayed to LacZ_base plot

LacZ_quadratic_split <- LacZ_base_split + geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = FALSE, color = 'blue') +
  geom_label(aes(x = 0.5, y = 55, 
                 label = paste('y =', 
                               pA, 
                               'x^2 +',
                               pB, 
                               'x +', 
                               pC,
                               '\nR^2 = ',
                               pR2
                 )), color = 'blue')
             
LacZ_quadratic_split

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/LacZ_quadratic_split.svg', width = 10, height = 5)

# both

LacZ_both_split <- LacZ_linear_split + geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = FALSE, color = 'blue') +
  geom_label(aes(x = 0.5, y = 55, 
                 label = paste('y =', 
                               pA, 
                               'x^2 +',
                               pB, 
                               'x +', 
                               pC,
                               '\nR^2 = ',
                               pR2
                 )), color = 'blue')

LacZ_both_split

ggsave('/Users/xb/LucksLab Dropbox/LucksLab/Group_Members/XSB/Projects/XSB.02.Dylan_LacZ_sfGFP/Data/LacZ_both_split.svg', width = 10, height = 5)

