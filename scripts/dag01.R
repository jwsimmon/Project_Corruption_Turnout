# Load some packages
library(dagitty)
library(ggdag)
library(ggthemr)

# Set graph themes
ggthemr("fresh")
theme_set(theme_minimal())

# Make the DAG
dag <- dagitty(
  "dag {   
  Z_S -> Corrupt_S -> D_1 -> T_2
  Corrupt_S -> SES_1 -> SES_2 -> T_2
  Z_S -> SES_1
  
}")

# Set the exposure and outcome variables
exposures(dag) <- c("Corrupt_S")
outcomes(dag) <- c("T_2")

# Make the coordinates for the plot
coordinates(dag) <-  list(
  x = c(
    Z_S = 4,
    Corrupt_S = 4,
    SES_1 = 3,
    D_1 = 3,
    SES_2 = 2,
    T_2 = 2
  ),
  y = c(
    Z_S = 3,
    Corrupt_S = 1,
    SES_1 = 3,
    D_1 = 1,
    SES_2 = 3,
    T_2 = 1
  )
)

# Plot the DAG
plot(dag)

# For what do we need to adjust?
adjustmentSets(dag)
parents(dag, "Corrupt_S")
# Visualize the backdoor path
plot(backDoorGraph(dag))

# Make the graph
tidy_dagitty(dag) %>% 
  arrange(name) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_text(color = "black", 
                parse = TRUE, 
                size = 3.5,
                label = c(
                  expression(Corrupt[i]), 
                  expression(Duty^G1), 
                  expression(SES^G1), 
                  expression(SES^G2),
                  expression(Vote[j]^G2),
                  expression(Z[i]) 
                )) +
  theme_dag()

# Adjustment set by hand
ggdag_adjustment_set(tidy_dagitty(dag))[[1]] %>%
  filter(adjusted != "adjusted") %>% 
  arrange(name) %>% 
  ggplot(., aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend, 
    color = adjusted
  )) +
  geom_dag_edges() + 
  geom_dag_text(color = "black",
                parse = TRUE,
                size = 3.5,
                label = c(
                  expression(Corrupt[i]),
                  expression(Duty^G1),
                  expression(SES^G1),
                  expression(SES^G2),
                  expression(Vote[j]^G2)
                )) +
  theme_dag()


# Make plot with ggdag
# ggdag(tidy_dagitty(dag), text_size = 3, text_col = 'white') + theme_dag()
# ggdag_adjustment_set(tidy_dagitty(dag), text_size = 3, text_col = 'white') + theme_dag()


