library(tidyverse)

set.seed(7)
number_of_paradigms <- 8 
number_of_questions <- 17 
number_of_trials <- 10000 
threshold <- 11 # because 5 out of 10 answers are neoclassical according to the GPT evaluation
paradigm_freqs <- vector()
p_value_threshold <- 0.05

for(i in 1:number_of_trials){
  answers_belong_to_paradigms <- sample(x = 1:number_of_paradigms, size = number_of_questions, replace = T)
  # the highest frequency from one paradigm
  freq_of_paradigm_w_the_most_answers <- max(table(answers_belong_to_paradigms))
  # vector of all maximum frequencies
  paradigm_freqs <- c(paradigm_freqs, freq_of_paradigm_w_the_most_answers)
}


paradigm_count_df <- tibble(max_freq = paradigm_freqs) %>% mutate(id = row_number()) %>% relocate(id)

p_value <- 
paradigm_count_df %>%
  count(max_freq) %>% 
  mutate(prob = n / sum(n)) %>% 
  filter(max_freq >= threshold) %>% 
  summarise(final_prob = sum(prob)) %>% 
  pull(final_prob)

text_y <- 
  paradigm_count_df %>%
  count(max_freq) %>% 
  slice_max(order_by = n, n = 1) %>%
  pull(n)

text_x <- 
  paradigm_count_df %>%
  count(max_freq) %>% 
  slice_max(order_by = n, n = 1) %>%
  pull(max_freq)

p_value_text <- paste0("P-value = ",p_value)

paradigm_count_df %>%
  mutate(color = ifelse(max_freq >= threshold, "Benchmark or more extreme","Less frequent than the benchmark" )) %>% 
  ggplot() +
  geom_histogram(aes(x = max_freq, fill = color)) + 
  scale_fill_manual(values = "darkblue") +  # Set fill color
  ggtitle("Maximum answer frequency", paste0("Paradigm Count: ", number_of_paradigms,", Question Count: ", number_of_questions, ", Trail Count: ", number_of_trials)) +
  theme_light() + 
  theme(legend.position = "bottom")  + 
  geom_text(aes(x = text_x + 1, y = text_y),
            label = p_value_text) +
  geom_vline(mapping= aes(xintercept = 11),
             linetype = "dashed",
             color = "red") +
  scale_x_continuous(breaks = seq(1, 17, by = 2)) +  # Set x-axis to odd numbers
  xlab("Maximum frequency of a paradigm in a trial") + 
  ylab("Number of trials")

ggsave(filename = "initial_plot.png",
       device = "png",
       width = 25,
       height = 15,
       units = "cm",
       dpi = "retina")




#--------------------------------------------


final_tbl <- tibble()
for(number_of_trails in c(5000,10000,15000,20000)){
  for(number_of_paradigms in 3:8){
    set.seed(7)
    print(number_of_paradigms)
    # number_of_paradigms <- 8
    print(number_of_trails)
    # number_of_questions <- 10 
    # number_of_trials <- 1000 
    # threshold <- 5 # because 5 out of 10 answers are neoclassical according to the GPT evaluation
    paradigm_freqs <- vector()
    
    for(i in 1:number_of_trails){
      answers_belong_to_paradigms <- sample(x = 1:number_of_paradigms, size = number_of_questions, replace = T)
      # the highest frequency from one paradigm
      freq_of_paradigm_w_the_most_answers <- max(table(answers_belong_to_paradigms))
      # vector of all maximum frequencies
      paradigm_freqs <- c(paradigm_freqs, freq_of_paradigm_w_the_most_answers)
    }
    
    
    paradigm_count_df <- tibble(max_freq = paradigm_freqs) %>% mutate(id = row_number()) %>% relocate(id)
    
    p_value <- 
      paradigm_count_df %>%
      count(max_freq) %>% 
      mutate(prob = n / sum(n)) %>% 
      filter(max_freq >= threshold) %>% 
      summarise(final_prob = sum(prob)) %>% 
      pull(final_prob)
    
    # text_y <- 
    #   paradigm_count_df %>%
    #   count(max_freq) %>% 
    #   slice_max(order_by = n, n = 1) %>%
    #   pull(n)
    
    # text_x <- 
    #   paradigm_count_df %>%
    #   count(max_freq) %>% 
    #   slice_max(order_by = n, n = 1) %>%
    #   pull(max_freq)
    
    # p_value_text <- paste0("P-value = ",p_value)
    
    temp = tibble(number_of_trails = number_of_trails,
                  number_of_paradigm = number_of_paradigms,
                  p_value = p_value)
    
    final_tbl <- 
      final_tbl %>% 
      bind_rows(temp)
    # paradigm_count_df %>%
    #   mutate(color = ifelse(max_freq >= threshold, "Benchmark or more extreme","Less frequent than the benchmark" )) %>% 
    #   ggplot() +
    #   geom_histogram(aes(x = max_freq, fill = color)) + 
    #   ggtitle("Maximum answer frequency", paste0("Paradigm Count: ", number_of_paradigms,", Question Count: ", number_of_questions, ", Trail Count: ", number_of_trials)) +
    #   theme_light() + 
    #   theme(legend.position = "bottom")  + 
    #   geom_text(aes(x = text_x + 1, y = text_y),
    #             label = p_value_text) +
    #   xlab("Maximum Frequency of a paradigm in a trial") + 
    #   ylab("Number of trials")
    
    # ggsave(filename = "initial_plot.png",
    #        device = "png",
    #        width = 25,
    #        height = 15,
    #        units = "cm",
    #        dpi = "retina")
  }
}

final_tbl <- 
final_tbl %>% 
  mutate(id = row_number()) %>% 
  relocate(id) 

final_tbl <- 
final_tbl %>% 
  mutate(significant = p_value < p_value_threshold)

final_tbl %>% 
  mutate(p_value = round(p_value, digits = 3)) %>%
  ggplot() + 
  geom_tile(aes(x = number_of_trails,
                y = number_of_paradigm,
                fill = significant ), 
            color = "white") + 
  geom_text(aes(x = number_of_trails,
                y = number_of_paradigm,
                label = p_value), 
            color = "white") + 
    theme_light() +
    theme(legend.position = "bottom") +
  scale_y_continuous(breaks = seq(3, 8, by = 1)) +  # Set x-axis to odd numbers
  scale_fill_manual(values = "red3") +  # Set fill color
  xlab("Number of Trials") +
  ylab("Number of Paradigms") + 
  ggtitle(label = "Statistical Significance of Observing 11 out 17 cases",
          paste0("P-values as arrays, significance level: ", p_value_threshold)) + 
  guides(fill = guide_legend(title = "Significance"))
  
ggsave(filename = "stat_sig_plot.png",
       device = "png",
       width = 25,
       height = 15,
       units = "cm",
       dpi = "retina")