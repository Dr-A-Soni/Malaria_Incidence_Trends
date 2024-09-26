
```{r global_options, include=FALSE}
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)
```

# Loading all required packages:

```{r, echo = FALSE}

 pacman::p_load(ggplot2,           # For Visualization of Graph
              rio,        # For import and export
              here,        # File locator
              tidyverse,     # For Data management and visualisation
              plotly,       # For interactive graphs
              forecast,      # Fit sin and cosin terms to data
              tseries,      # For Time Series analysis
              ggpubr,       # Simplyfies plots and customisation.
              gt)         # To generate tables.


# Loading the Dataset for Time Series Analysis:


Malaria <- import("D:/Downloads/Data/442CEA8_ALL_LATEST.csv")

Malaria <- Malaria %>% select(5,11:14)

colnames(Malaria) <- c("Time", "Country", "Rate_per_1000_N", "Lower_bound", "Upper_bound")

Malaria <- Malaria %>% arrange(Country,Time) %>% slice(-c(1:23))

Malaria_World <- Malaria %>% filter(Country == "World")
Malaria_South_East_Asia <- Malaria %>% filter(Country == "South-East Asia")
Malaria_India <- Malaria %>% filter(Country == "India")

```
Time Series Analysis:


```{r,Malaria, echo = FALSE, fig.cap = "Figure 1: Malaria incidence in World, South-East Asia and India (2000-2022)", out.width = "60%", fig.align = "center"}
# Convert to time series object
# 1) World:
ts_world <- ts(Malaria_World$Rate_per_1000_N, start = 2000, end = 2022, frequency = 1)


# 2) South-East Asia:

ts_South_East_Asia <- ts(Malaria_South_East_Asia$Rate_per_1000_N, start = 2000, end = 2022, frequency = 1)


# 3) India:
ts_India <- ts(Malaria_India$Rate_per_1000_N, start = 2000, end = 2022, frequency = 1)

Malaria_incidence <- Malaria %>% filter(Country %in% c("World","South-East Asia", "India"))

ggplot(Malaria_incidence, aes(x = Time, y = Rate_per_1000_N, color = Country)) +
    geom_line() +
    labs(title = "Malaria incidence 2000-2022", y = "Rate per 1000 population at Risk", x = "Year") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.text.x =  element_text(face ="bold" ), axis.text.y = element_text(face = "bold"), legend.position = c(0.85,0.85), legend.background = element_rect(fill = "transparent", color = "black"), legend.text = element_text(face = "bold"), legend.title = element_blank()) + scale_color_manual(values =c("purple","orange","blue"), name = "Region") +
    geom_point()

```


```{r, plots, echo = FALSE, fig.cap = "Figure 2: Malaria incidence of India with confidence internval.", out.width = "60%" , fig.align = "center"}
# Plot the malaria incidence over time in India with confidence Interval:

ggplot(Malaria_India, aes(x = Time, y = Rate_per_1000_N)) +
      geom_line(aes(color = "Rate per 1000 at risk"), linewidth = 1) +
      geom_ribbon(aes(ymin = Lower_bound, ymax = Upper_bound, fill = "Confidence Interval"), alpha = 0.2) +
      labs(title = "Malaria Incidence in India (2000-2022)",
           x = "Year",
           y = "Rate per 1000 Population at Risk") +
      theme_bw(base_size = 13) +
      scale_color_manual(values = c("Rate per 1000 at risk" = "black")) +
      scale_fill_manual(values = c("Confidence Interval"= "purple")) +
      theme(legend.title = element_blank(),
            legend.position = c(0.8, 0.8),
            legend.background = element_rect(fill = "transparent", color = "black"),
            legend.text = element_text(face = "bold"),
            plot.title = element_text(face = "bold", hjust = 0.5)) +
            geom_point(color = "black", size = 3) +
            scale_x_continuous(limits = c(2000,NA), breaks = seq(2000,2022, 2)) +
            scale_y_continuous(limits = c(0,35)) +
            theme(axis.text.x =  element_text(face = "bold"), axis.text.y =  element_text(face = "bold"))



```

**Linear Regression Model**


```{r, lm, echo = FALSE, fig.cap = "Figure 3: Linear regression model to the time series analysis", out.width = "60%", fig.align = "center", message = FALSE}

# Fit a linear model

trend_model <- lm(Rate_per_1000_N ~ Time, data = Malaria_India)

# Plot the time series with trend line

ggplot(Malaria_India, aes(x = Time)) +
  geom_line(aes(y = Rate_per_1000_N, color = "Actual Malaria Incidence")) +
  geom_smooth(aes(y = Rate_per_1000_N, color = "Fitted Regression Line"), method = "lm", se = FALSE) +
  labs(title = "Malaria Incidence with Trend Line",
  x = "Year",
  y = "Rate per 1000 Population at Risk") +
 theme_bw(base_size = 16) +
 theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.title = element_blank(), legend.position = c(0.8, 0.8), legend.background = element_rect(fill = "transparent", color = "black"), axis.text.x = element_text(face = "bold"), axis.text.y = element_text(face = "bold")) +
  scale_color_manual(values = c("blue", "red"),labels = c("Actual Malaria Incidence", "Fitted Regression Line"))

```

**Summary on trend model:**

```{r, echo = FALSE, message = FALSE, warning = FALSE}

s <- capture.output(summary(trend_model))


# Part1: For residuals

rs <- s[6:7]
rs <-  gsub("\\s+", " ", rs)
names <- unlist(strsplit(trimws(rs[[1]]), " "))
values <- unlist(strsplit(trimws(rs[[2]]), " "))

# Create a data frame for residuals

residuals_df <- data.frame(
  Statistic = names,
  Value = as.numeric(values)
)



# Part2: For coefficients:

# Extracting coefficients

coefficients_raw <- s[10:12]
coefficients_cleaned <- gsub("\\s+", " ", trimws(unlist(coefficients_raw)))

# Create a data frame for coefficients

coefficients_list <- do.call(rbind, strsplit(coefficients_cleaned, " "))

coefficients_df <- data.frame(
  Term = coefficients_list[, 1],
  Estimate = as.numeric(coefficients_list[, 2]),
  Std_Error = as.numeric(coefficients_list[, 3]),
  t_value = as.numeric(coefficients_list[, 4]),
  p_value = coefficients_list[, 5]
)

coefficients_df <- coefficients_df[-1,]

# create a table using kable

coefficients_df %>% kable(row.names = FALSE, caption = "<strong style ='color:black;text-align: center;'> Table 1a: Coefficients of the Trend model</strong>") %>%
kable_styling(c("striped", "bordered", "condensed"), full_width = FALSE)


multiple_r_squared <- sub("Multiple R-squared:\\s*([0-9.]+),.*", "\\1",s[18])
adjusted_r_squared <- sub(".*Adjusted R-squared:\\s*([0-9.]+)\\s*.*", "\\1",s[18])


f_statistic <- sub("F-statistic:\\s*([0-9.]+).*", "\\1", s[19])
p_value <- sub(".*p-value:\\s*([0-9.e-]+)", "\\1", s[19])

Residual_Standard_Error <- sub("Residual standard error: \\s*([0-9.]+),.*", "\\1", s[17])

# Creating a data frame
summary_df <- data.frame(
  Statistic = c("Residual_Standard_Error", "Multiple R-squared", "Adjusted R-squared", "F-statistic", "p-value"),
  Value = c(Residual_Standard_Error,multiple_r_squared, adjusted_r_squared, f_statistic, p_value)
)

# Combining residuals and model summary  results:

rest <-  rbind(residuals_df,summary_df)

rest %>% kable(caption = "<strong style ='color:black;text-align: center;'> Table 1b Residuals & Summary Statistics of the Trend Model</strong>") %>%
  kable_styling(c("striped", "bordered", "condensed"), full_width = FALSE) %>%
  pack_rows("Residuals", 1,5, color = "black") %>%
  pack_rows("Summary Statstic", 6, 9, color = "black")



```

**Forecasting with ARIMA**

**ARIMA Model and Forecast:**


```{r, AF, echo = FALSE, fig.cap = "Figure 4: Residuals with Arima Model (0,1,0)", out.width = "60%", fig.align = "center", message = FALSE, warning = FALSE}

library(forecast)
pacman::p_load(kableExtra)
# Fit an ARIMA model

fit <- auto.arima(ts_India)

# step1: capture the output

a <- capture.output(checkresiduals(fit))

# Step 2: Extract relevant values
q_value <- sub("Q\\* = (.*), df.*", "\\1", a[5])  # Extract Q*
df_value <- sub(".*df = (\\d+).*", "\\1", a[5])   # Extract df value
p_value <- sub(".*p-value = (.*)", "\\1", a[5])    # Extract p-value
model_df <- sub("Model df: (.*)\\..*", "\\1", a[7]) # Extract Model df
total_lags <- sub(".*Total lags used: (.*)", "\\1", a[7]) # Extract Total lags

# Create a data frame
results_df <- data.frame(
  Statistic = c("Q*", "Degrees of Freedom", "p-value", "Model df", "Total lags used"),
  Value = c(
    q_value,                # Q* value
    df_value,               # df value
    p_value,                # p-value
    model_df,               # Model df
    total_lags              # Total lags used
  ),
  stringsAsFactors = FALSE
)

# Display the results using kable
results_df %>%
  kable(caption = "<strong style ='color:black;text-align: center;'> Table 2: Residuals from ARIMA (0,1,0) with drift</strong>", align = "c") %>%
  kable_styling(c("striped","condensed","bordered"), full_width = TRUE)

```

**ACF and PACF Analysis:**

```{r, ACF, echo = FALSE, fig.cap = "Figure 5: ACF and PACF Analysis.", out.width = "60%", fig.align = "center"}

par(mfrow = c(1,2))
acf(fit$residuals, main = "ACF analysis")
pacf(fit$residuals, main = "PACF Analysis")

```

#### **Forecast of Malaria Incidence in India for the Next Five Years**

The projections as seen in Figure \ref(@fig:Forecast), indicate a sustained decline in malaria incidence, reinforcing the positive impact of ongoing public health interventions.

```{r,Forecast, echo = FALSE, fig.cap = "Figure 6: Forecast of Malaria Incidence in India for next 5 years", fig.align = "center", out.width = "60%"}

# Forecast the next 5 years
forecast_result <- forecast(fit, h = 5)

# Plot the forecast
plot(forecast_result)

```

```{r, Table, echo = FALSE}
pacman::p_load(kableExtra,knitr)

forecast_data <- as.data.frame(forecast_result)

# Add Year column
forecast_data$Year <- seq(from = 2023, by = 1, length.out = nrow(forecast_data))

row.names(forecast_data) <- c(seq(1:5))

forecast_data <- forecast_data %>% select(Year, everything())


# Create the table using kable

forecast_data %>% kable(caption = "<strong style ='color:black;text-align: center;'> Table 3: Forecast of Malaria Incidence in India for next five years</strong>",escape = FALSE, align = "c", ) %>%
    kable_styling(c("striped","condensed","bordered"), full_width = TRUE) %>%
    row_spec(0,bold = TRUE,color = "black", align = "center")


```

