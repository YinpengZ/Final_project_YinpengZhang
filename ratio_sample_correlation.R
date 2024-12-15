#correlation between morphology-based species numbers and the cryptic ratios/sample sizes
#Yinpeng Zhang
#Last edit on Dec 8th 2024


library(ggplot2)
#all morphology-based species studies (change the file names for each vertebrate class)
ratio <- read.csv("datasets_all/amphibian_ratios.csv", header = TRUE)
sample <- read.csv("datasets_all/amphibian_sample.csv", header = TRUE)
#multiple morphology-based species studies(change the file names for each vertebrate class)
ratio <- read.csv("datasets_mul/amphibian_ratios.csv", header = TRUE)
sample <- read.csv("datasets_mul/amphibian_sample.csv", header = TRUE)
#
#Pearson corr for mor-spp. number vs cryptic ratios
#nuDNA only
ratio_result_nu <- cor.test(x = ratio$morph_number_nu, y = ratio$ratio_nu, method = "pearson")
print(ratio_result_nu)
#combined molecular
ratio_result_comb <- cor.test(x = ratio$morph_number_comb, y = ratio$ratio_comb, method = "pearson")
print(ratio_result_comb)

#Pearson corr for mor-spp. number vs mean sample sizes (nuDNA-only)
sample_result_nu <- cor.test(x = sample$morph_number_nu, y = sample$sample_nu, method = "pearson")
print(sample_result_nu)

#Optional: for ratio analysis, record p and r if needed
#nu only
p_value <- ratio_result_nu$p.value
correlation <- ratio_result_nu$estimate

#comb
p_value <- ratio_result_comb$p.value
correlation <- ratio_result_comb$estimate

#for sample analysis
p_value <- sample_result_nu$p.value
correlation <- sample_result_nu$estimate



#plot for ratio
ggplot(data.frame(ratio), aes(x = morph_number_nu, y = ratio_nu)) +
  geom_point(size = 3, color = "blue") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(
    title = "Amphibians multi-spp. (nuDNA)",#change title name for each class 
    x = "Morphology-based Species Number", 
    y = "Cryptic Species Ratio"
  ) +
  theme(
    axis.text = element_text(color = "black", size = 20),
    axis.title = element_text(color = "black", size = 25),
    plot.title = element_text(color = "black", size = 25,
                              hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 14)                     
  ) +
  scale_x_continuous(breaks = seq(0, max(ratio$morph_number_nu, na.rm = TRUE), by = 4))

ggplot(data.frame(ratio), aes(x = morph_number_comb, y = ratio_comb)) +
  geom_point(size = 3, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(
    title = "Amphibians all-spp. (combined)", #change title names for each class
    x = "Morphology-based Species Number",
    y = "Cryptic Species Ratio"
  ) +
  theme(
    axis.text = element_text(color = "black", size = 20),
    axis.title = element_text(color = "black", size = 25),
    plot.title = element_text(color = "black", size = 25,
                              hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 12),                     
    legend.title = element_text(size = 14)
  ) +
  scale_x_continuous(breaks = seq(0, max(ratio$morph_number_nu, na.rm = TRUE),by=1))

#plot for sample
ggplot(data.frame(sample), aes(x = morph_number_nu, y = sample_nu)) +
  geom_point(size = 3, color = "blue") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Ray-finned Fishes (multi-spp.)", #change title name for each class
    x = "Morphology-based Species Number",  
    y = "Averaged Sample Sizes"  
  ) +
  theme(
    axis.text = element_text(color = "black", size = 20),
    axis.title = element_text(color = "black", size = 25),
    plot.title = element_text(color = "black", size = 25,
                              hjust = 0.5, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  ) +
  scale_x_continuous(breaks = seq(0, max(ratio$morph_number_nu, na.rm = TRUE),by=4))

  

