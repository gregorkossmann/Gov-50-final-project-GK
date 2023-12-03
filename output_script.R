## ---- echo = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(infer)
library(knitr)
library(kableExtra)
ZA8155_16 <- read_excel("~/Desktop/Gov50Data/ZA8155_16.xls")

real_inc <- as.numeric(as.character(ZA8155_16[["...3"]]))

german_inc <- ZA8155_16 |>
  summarize(`Reales Pro-Kopf-Einkommen in Europa und den USA (1870-1992)`, real_inc)|>
  slice(-(1:75)) |>
  slice_head(n = -35) |>
  ggplot(aes(x = `Reales Pro-Kopf-Einkommen in Europa und den USA (1870-1992)`, y = real_inc, fill = real_inc)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(x = "Years", y = "Income Per Capita in Germany (in mil. USD)") +
  scale_y_continuous(limits = c(0, 9000)) +
  geom_vline(xintercept = "1948", color = "indianred", linetype = "dashed") +
  geom_vline(xintercept = "1952", color = "indianred", linetype = "dashed") 
   
german_inc 


## ---- echo = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------
library(readxl)
US_Loans <- read_excel("~/Desktop/Gov50Data/Gov50USLoans1946-1961.xlsx", 
    col_types = c("text", "numeric", "numeric", 
        "skip"))

percentage_change <- ((US_Loans$"Marshall Plan period: 1949-52" - US_Loans$"Post-war relief period: 1946-1948") / US_Loans$"Post-war relief period: 1946-1948") * 100 

real_change <- (US_Loans$"Marshall Plan period: 1949-52" - US_Loans$"Post-war relief period: 1946-1948")

US_Loans <- US_Loans|>
  mutate("Percentage Change" = percentage_change) |>
  mutate("Real Change (in millions of US dollars)" = real_change)


US_Loans_sorted <- US_Loans[order(US_Loans$"Real Change (in millions of US dollars)", decreasing = TRUE), ]

knitr::kable(US_Loans_sorted, digits = 2)



## ---- fig.width = 12, echo = FALSE---------------------------------------------------------------------------------------------------------------------------------------------

real_change_graph <- US_Loans_sorted |>
  ggplot(aes(x = reorder(`U.S. overseas loans and grants to countries in Western Europe during various periods following the Second World War between 1946 and 1961 (in millions of U.S. dollars)`,
                         -`Real Change (in millions of US dollars)`),
             y = `Real Change (in millions of US dollars)`,
             fill = `Real Change (in millions of US dollars)`)) +
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(x ="Countries Receiving Loans from US", y = "Change in Loan Amount in Marshall Period (1949-52), compared to 1946-48 levels")

real_change_graph


## ---- echo = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------

Agdata <- read_excel("~/Desktop/Gov50Data/Gov50AgOutput1947-1951.xlsx", 
col_types = c("text", "numeric", "numeric", 
"numeric"))


Ag_perc_change <- ((Agdata$`1950-1951` - Agdata$`1946-1947`) / Agdata$`1948-1949`)*100  

Agdata <- Agdata |>
  mutate(perc_change = Ag_perc_change)

Agdata_sorted <- Agdata[order(Agdata$"perc_change", decreasing = TRUE), ]

Agdata_graph <- Agdata_sorted|>
  ggplot(aes(x = reorder(`Agricultural output in Western Europe 1947-1951`, - `perc_change`), y = `perc_change`, fill = `perc_change`)) + 
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(y = "Percentage Change in Agricultural Output from 1947-1951", x = "")+
  scale_fill_continuous(name = "Percentage Change")

Agdata_graph



## ---- echo = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------
Industrydata <- read_excel("~/Desktop/Gov50Data/Gov50IndustrialOutput1947-1951.xlsx", 
col_types = c("text", "numeric", "numeric", 
"numeric"))

ind_perc_change <- ((Industrydata$`1951` - Industrydata$`1947`) / Industrydata$`1947`)*100  

Industrydata <- Industrydata |>
  mutate(perc_change = ind_perc_change)

Industrydata_sorted <- Industrydata[order(Industrydata$"perc_change", decreasing = TRUE), ]


Industry_graph <- Industrydata_sorted|>
  ggplot(aes(x = reorder(`Industrial output in Western Europe 1947-1951`, - `perc_change`), y = `perc_change`, fill = `perc_change`)) + 
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(y = "Percentage Change in Industrial Output from 1947-1951", x = "")+
  scale_fill_continuous(name = "Percentage Change")

Industry_graph


## ---- echo = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------

common_countries <- intersect(intersect(US_Loans$`U.S. overseas loans and grants to countries in Western Europe during various periods following the Second World War between 1946 and 1961 (in millions of U.S. dollars)`, Industrydata$`Industrial output in Western Europe 1947-1951`), Agdata$`Agricultural output in Western Europe 1947-1951`)

US_Loans_subset <- US_Loans[US_Loans$`U.S. overseas loans and grants to countries in Western Europe during various periods following the Second World War between 1946 and 1961 (in millions of U.S. dollars)` %in% common_countries, ]
Industrydata_subset <- Industrydata[Industrydata$`Industrial output in Western Europe 1947-1951` %in% common_countries, ]
Agdata_subset <- Agdata[Agdata$`Agricultural output in Western Europe 1947-1951` %in% common_countries, ]

merged_data <- merge(US_Loans_subset, Industrydata_subset, by.x = "U.S. overseas loans and grants to countries in Western Europe during various periods following the Second World War between 1946 and 1961 (in millions of U.S. dollars)", by.y = "Industrial output in Western Europe 1947-1951")
merged_data <- merge(merged_data, Agdata_subset, by.x = "U.S. overseas loans and grants to countries in Western Europe during various periods following the Second World War between 1946 and 1961 (in millions of U.S. dollars)", by.y = "Agricultural output in Western Europe 1947-1951")

model_industrial <- lm(perc_change.x ~ `Real Change (in millions of US dollars)`, data = merged_data)
model_agricultural <- lm(perc_change.y ~ `Real Change (in millions of US dollars)`, data = merged_data)


summary_industrial <- summary(model_industrial)
summary_agricultural <- summary(model_agricultural)


table_industrial <- data.frame(
  Coefficients = rownames(summary_industrial$coefficients),
  Estimate = summary_industrial$coefficients[, "Estimate"],
  `Std. Error` = summary_industrial$coefficients[, "Std. Error"],
  `t value` = summary_industrial$coefficients[, "t value"],
  `Pr(>|t|)` = summary_industrial$coefficients[, "Pr(>|t|)"]
)

table_agricultural <- data.frame(
  Coefficients = rownames(summary_agricultural$coefficients),
  Estimate = summary_agricultural$coefficients[, "Estimate"],
  `Std. Error` = summary_agricultural$coefficients[, "Std. Error"],
  `t value` = summary_agricultural$coefficients[, "t value"],
  `Pr(>|t|)` = summary_agricultural$coefficients[, "Pr(>|t|)"]
)


kable(table_industrial, "html", align = "c") |>
  kable_styling(full_width = FALSE)

kable(table_agricultural, "html", align = "c") |>
  kable_styling(full_width = FALSE)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
avg_perc_finite <- mean(percentage_change[is.finite(percentage_change)])
avg_perc_finite

avg_real_finite <- mean(real_change[is.finite(real_change)])
avg_real_finite


## ---- eval=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------
## knitr::purl("index.Rmd", "output_script.R")
## cat(readLines("output_script.R"), sep = "\n")

