# Seattle Crisis Statistics Analysis Project
## Fantastic Four:
### Arman Azhand, Danfeng Yang, Madisen Arurang, Liam O'Keeffe

## Access to our Project
Our ShinyApp can be found [here](https://armanazhand.shinyapps.io/fantastic-four-final-project/)!

## Seattle Crisis Dataset
Our project uses the [Seattle Crisis Data](https://www.kaggle.com/city-of-seattle/seattle-crisis-data) curated by the City of Seattle Open Data, maintained by [Kaggle](https://www.kaggle.com/). This open dataset is updated regularly and contains **over 43,000** reported crisis in the city of Seattle. The version of the data used for analysis in this project ranges from *May 15th, 2015 to November 28th, 2018*\*.

The dataset is in **csv** format, however, we converted it to a **dataframe** and manipulated as needed for easy and efficient analysis.

\* *Even so, if one was to put any other version of the data, the analysis would still work - as long as header names are consistent within version changes.*

---

## Who and Why?
This project is geared to the law enforcement and crisis responders of the Seattle area. Since our dataset focuses on reported crisis and crimes in the Seattle area, we thought it would be appropriate to gear our analysis towards a group that could use this data to not only aid them in their job, but to also save **more** lives and limit the possibilities of certain crisis from escalating in the future.

In other words, our analysis of the dataset will prove to be most useful to not only our main audience, but the *safety and well-being* of the citizens of Seattle.

With the visualizations of this data, we hope that trends in crisis and crimes can be made clearer for law enforcement to be able to do their jobs more **efficiently, safely, and effectively**. Our *biggest* wish is for there to be *less* risks of harm for any group in any situation that may present itself with a *faster* response time.

---

## Project Logistics
The R language - along with the ShinyApp functionality was used to create an interactive analysis experience. The R libraries used for the interactive page are `shiny` and `shinythemes`.

To analyze the dataset, we performed dataset manipulation with the `dplyr`, `lubridate`, and `stringr` libraries.

To visualize the dataset, we used many different graphing tools. Some were made in `base R` but others required the libraries `ggplot2` and `treemapify`.

All other functions were done with `base R`.

---

## About the Team
The team consists of four University of Washington students in the INFO 201 course. This project is the culmination of what we have learned throughout the quarter and is a chance for us to apply our knowledge.

More information about the team - such as fun-facts - can be found in the ShinyApp!
