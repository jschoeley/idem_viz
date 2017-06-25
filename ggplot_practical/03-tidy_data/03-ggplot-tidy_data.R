#' ---
#' title: "ggplot: Tidy data"
#' author: "Jonas Schöley"
#' date: "June 20th, 2017"
#' output:
#'   github_document:
#'     toc: true
#' ---

#'## Every variable in its own column

library(tidyverse)

#' **Data structure matters a lot when working with ggplot**. However, once we
#' provided ggplot with nice and tidy data it does a lot by itself. In order for
#' this to work the data needs to be in the right format to begin with: **data
#' needs to be a data frame** and **every variable of interest needs to be a
#' separate column**. Let's explore what that means.

head(WorldPhones)

#' Here's the number of telephone connections over time by continent. The first
#' problem with this data is that it's not a *data frame*, it's a matrix with
#' row and column names. If we try to plot it, well...

#+ error=TRUE
ggplot(WorldPhones)

#' That's easily fixed however
phones <- as.data.frame(WorldPhones)

#' Say we we want to plot the number of telephone connections over time by
#' continent. This implies the following *variables of interest*:
#'
#'   * the number of telephone connections `n`
#'   * the continent `cont`
#'   * the year `year`
#'
#' Problem is, *none* of these variables are explicitly given in our data frame.
#' Of course the data is all there, just not in a format we can use with ggplot.
#' Remember: all we handle in ggplot are names of variables which in turn are
#' columns of a data frame. So the question is how to reshape the data into a
#' form where all the variables of interest are separate columns in the data
#' frame.
#'
#' To reshape we are going to use the libraries
#' [dplyr](https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html)
#' and
#' [tidyr](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html).
#' Both are loaded when you load `library(tidyverse)`.

#' The easiest variable to make explicit is the year. It is given as rownames of
#' the data frame. We take the rownames, convert them from character to integer
#' type, and add them as the variable `year` to the data frame.

phones <- mutate(phones, year = as.integer(rownames(phones)))
phones

#' That leaves us with the variables *"number of telephone connections"* and
#' *"continent"* to make explicit. They shall become separate columns in the
#' data frame. With the help of `gather()` we **transform from wide to long
#' format**.

phones <- gather(phones, key = cont, value = n, -year)
phones

#' What kind of black magic did just happen? **A short primer on wide versus long
#' data format**:

#' Each table has a *wide format* and a long format representation. The
#' information content is the same in both formats. It's the layout that
#' differs.
#'
#' Here's a wide format table containing the explicit variables `Female` and
#' `Male`.

wide <- data_frame(Female = 1:2, Male = 3:4)

#' The same table in long format representation containing the explicit variables
#' `Sex` and `N`.

gather(wide, key = Sex, value = N)

#' Back to our telephone example. We told the computer to look at all columns
#' apart from `year` and transform them into the columns `cont` and `n`. `cont`
#' holds the continent names for the variable `n`, the number of telephone
#' connections. The continent names are taken from the original column names we
#' *gathered* over.
#'
#' We now can plot our data easily.

ggplot(phones) +
  geom_line(aes(x = year, y = n, colour = cont))

#'## Data Pipelines

#' We can also write everything we did so far as a single *data analysis
#' pipeline*. We start with the raw data and output a plot. This is a great
#' approach for fast, interactive data analysis.
#'
#' This is what we need to know in order to build pipelines:
#'
#' * The object on the left of the pipe operator (`%>%`) is passed onto the
#'   first argument of the function on the right
#' * If we want to use the object on the left in other places than the first
#'   argument we can explicitly refer to it by using a dot (`.`)
#'
#' Here's our telephone example in pipeline form.

# the raw data...
WorldPhones %>%
  # ...is converted to a data frame...
  as.data.frame() %>%
  # ...the rownames are added as the column `year`...
  # (note that I use the dot here to explicitly refer to the input data)
  mutate(year = as.integer(rownames(.))) %>%
  # ...the data gets transformed from wide to long format...
  gather(key = cont, value = n, -year) %>%
  # ...and finally plotted
  # (note that I can pipe the tidy data frame directly into ggplot)
  ggplot() +
  geom_line(aes(x = year, y = n, colour = cont))

#'## Further examples

#'### Tidying data on police arrests

#' Before we start plotting we need to ask ourselves: *What do we need to do
#' with our data in order to get the plot we want?* Here are some examples.

# we start with raw data...
USArrests %>%
  mutate(
    # ...and add the new variable `state` from the rownames...
    state = rownames(.),
    # ...we then reorder the levels of `state` according to the percentage of
    # people living in urban areas...
    state = reorder(state, UrbanPop)) %>%
  # ...and make a dotplot of the percentage of urban population by state...
  ggplot() +
  geom_point(aes(x = UrbanPop, y = state))

# we start with raw data...
USArrests %>%
  mutate(
    # ...and add the new variable `state` from the rownames...
    state = rownames(.),
    # ...we then reorder the levels of `state` according to the combined
    # murder, assault and crime rates...
    state = reorder(state, Murder+Assault+Rape)) %>%
  # ...we convert to long format, gathering "Assault", "Murder" and "Rape"
  # into "crime"...
  gather(key = crime, value = rate, -state, -UrbanPop) %>%
  # ...and make a dotplot of the crime-rate by crime and state
  ggplot() +
  geom_point(aes(x = rate, y = state, colour = crime))

library(ggrepel) # brilliant package for labelling points in a scatterplot
# we start with raw data...
USArrests %>%
  # ...and add the new variable `state` from the rownames...
  mutate(state = rownames(.)) %>%
  # ...andmake a labelled scatterplot of Murder versus Rape
  ggplot(aes(x = Murder, y = Rape)) +
  geom_text_repel(aes(label = state),
                  colour = "grey",
                  segment.color = "grey",
                  size = 3) +
  geom_point()

#'### Tidying Anscombe's quartet

#' Can you figure out what happens here? Try running the code yourself line by
#' line.

anscombe %>%
  mutate(id = seq_along(x1)) %>%
  gather(... = -id) %>%
  separate(key, sep = 1, into = c("axis", "panel")) %>%
  spread(key = axis, value = value) %>%
  ggplot(aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  facet_wrap(~panel)

#'### Tidying data on test-retest reliability

wide <- read_csv("https://raw.githubusercontent.com/jschoeley/idem_viz/master/ggplot_practical/03-tidy_data/wide_data.csv")
wide

wide %>%
  # add a unique identifier to each row (each patient)
  mutate(id = 1:nrow(.)) %>%
  gather(key = type, value = value, -id, -name_rater1, -name_rater2) %>%
  separate(col = type, into = c("measurement", "method"), sep = "_") %>%
  mutate(rater = ifelse(grepl('1', method), name_rater1, name_rater2)) %>%
  separate(col = method, into = c("method", "test"), sep = "\\d") %>%
  mutate(test = ifelse(test == "", "a", test)) %>%
  # beautification
  select(id, rater, test, measurement, method, value) %>%
  arrange(id, measurement, rater, test) -> long

long %>%
  filter(method == "camera") %>%
  ggplot(aes(x = test, y = value)) +
  geom_line(aes(color= rater, group = id)) +
  facet_grid(rater~measurement)

#' Comparisions along the y-axis is easiest if the scales are aligned
#' therefore it is easier to compare along the horizontal.

long %>%
  filter(method == "camera") %>%
  ggplot(aes(x = test, y = value)) +
  geom_line(aes(color= rater, group = id)) +
  facet_grid(measurement~rater)

#' Differences are seen most clearly when plotted directly.

long %>%
  filter(method == "camera") %>%
  spread(test, value = value) %>%
  mutate(diff = a-b) %>%
  ggplot() +
  geom_dotplot(aes(x = diff)) +
  facet_wrap(~rater)

#'## Plotting model output

#' We "tidy" the output of the `survfit` function via the "broom" package.

library(survival)
surv <- survfit(Surv(time = heart$start, time2 = heart$stop, event = heart$event) ~ heart$transplant)
surv
broom::tidy(surv) %>%
  ggplot(aes(x = time, y = estimate)) +
  geom_step(aes(colour = strata))

#' Dobson (1990) Page 93: Randomized Controlled Trial.

dat <- data_frame(
  counts = c(18,17,15,20,10,20,25,13,12),
  outcome = gl(3,1,9),
  treatment = gl(3,3)
)

dat_fit <- glm(counts ~ outcome + treatment, family = poisson(), data = dat)

broom::tidy(dat_fit)
broom::glance(dat_fit)

#'## Further Reading

#' - [Tidy Data.](http://www.jstatsoft.org/v59/i10/paper) A lot of confusion
#'   about ggplot stems from the data being in an unsuitable format. ggplot
#'   works with what its creator calls *tidy data*.
#' - [An introduction to data transformation with `dplyr`.](https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html)
#'   This covers -- among other things -- data pipelines.
#' - [An introduction to data tidying with `tidyr`.](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html).
#'   This involves -- among other things -- transforming between long and wide format.
#' - [The data wrangling cheat sheet](https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf)
#'   is a great quick reference for `dplyr` and `tidyr`.

sessionInfo()

#' cc-by Jonas Schöley 2017
