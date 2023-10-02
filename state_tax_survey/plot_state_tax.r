# Create 3d animated plot of 3 state tax rate dimensions,
# income, property and sales

library(tidyverse)
library(plotly)

state_rates <- read_csv("data/state_rate.csv",col_types = "cnnn")

# judge how to weight realized cost of sales and property relative to income tax.
sales_adj    = 0.3 # assume we spend 30% of our taxable income on items subject to sales tax.
property_adj = 1.0 # assume median income tax liability is about equal to the property tax on the median home. 

# use these adjustments to create ranking that we will use to color the markers in the plot.
# the sum of the adjusted values is a *rough* guide to the total tax burden.

state_rates_adj <- state_rates %>% 
   mutate(tax_pain = income_tax + (sales_tax * sales_adj) + (property_tax * property_adj)) %>% 
   arrange(desc(tax_pain))


plot_ly(state_rates_adj,x = ~income_tax,
        y= ~sales_tax,
        z= ~property_tax,
        type="scatter3d", 
        mode="markers",
        color = ~tax_pain,
        hoverinfo = "text",
        text= ~state) %>% 
   layout(title = "Major Tax Rates by State",
          scene = list(xaxis = list(title = 'Income Tax'),
                       yaxis = list(title = 'Sales Tax'),
                       zaxis = list(title = 'Property Tax')))
