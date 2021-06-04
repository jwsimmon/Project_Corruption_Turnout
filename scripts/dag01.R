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
  S_Z -> S_Corrupt -> D1 -> T2
  S_Corrupt -> SES1 -> SES2 -> T2
  S_Z -> SES1
  
}")

# Set the exposure and outcome variables
exposures(dag) <- c("S_Corrupt")
outcomes(dag) <- c("T2")
adjustedNodes(dag) <- c("S_Z")

# Make the coordinates for the plot
coordinates(dag) <-  list(
  x = c(S_Z = 4, 
        S_Corrupt = 4, 
        SES1 = 3, 
        D1 = 3, 
        SES2 = 2, 
        T2 = 2
  ),
  y = c(
    S_Z = 3, 
    S_Corrupt = 1, 
    SES1 = 3, 
    D1 = 1, 
    SES2 = 3, 
    T2 = 1
  )
)

# Plot the DAG
plot(dag)

# what should we adjust for?
plot(backDoorGraph(dag)) # plot the backdoor path
parents(dag, "S_Corrupt") # parents of S_Corrupt
adjustmentSets(dag)
impliedConditionalIndependencies(dag)

# Find children/parents/ancestors/descendants
# children(dag, "X1")
# children(dag, "X2")
# children(dag, "X3")
# descendants(dag, "X3")


# Make plot with ggdag
ggdag(tidy_dagitty(dag), text_size = 3, text_col = 'white') + theme_dag()
ggdag_adjustment_set(tidy_dagitty(dag), text_size = 3, text_col = 'white') + theme_dag()


# Make the graph
tidy_dagitty(dag) %>% 
  arrange(name) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_text(color = "black", 
                parse = TRUE, 
                size = 3.5,
                label = c(
                  expression(Duty^G1), 
                  expression(Corrupt[i]), 
                  expression(Z[i]), 
                  expression(SES^G1), 
                  expression(SES^G2), 
                  expression(Vote[j]^G2)
                )) +
  theme_dag()

# ggdag_adjustment_set(tidy_dagitty(dag))[[1]] %>% 
#   arrange(name) %>% 
#   ggplot(aes(x = x, y = y, xend = xend, yend = yend, group= adjusted)) +
#   geom_dag_edges(aes(edge_alpha = adjusted)) +
#   geom_dag_text(aes(color = adjusted), 
#                 parse = TRUE, 
#                 size = 3.5,
#                 label = c(
#                   expression(Duty^G1), 
#                   expression(Corrupt[i]), 
#                   expression(Z[i]), 
#                   expression(SES^G1), 
#                   expression(SES^G2), 
#                   expression(Vote[j]^G2)
#                 )) +
#   theme_dag()
