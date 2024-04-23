library(ggplot2)

# Paramètres pour les distributions
moyenne1 <- 10
moyenne2 <- 12
sd <- 3
n <- 30
df <- n + n - 2  # Degrés de liberté pour les deux échantillons

# Calculs pour les t-scores critiques et effectifs
t_critique <- qt(0.975, df)  # Valeur t critique pour alpha = 0.05 bilatéral
t_effet <- (moyenne2 - moyenne1) / (sd * sqrt(1/n + 1/n))  # t-score de l'effet attendu

# Fonction de densité de t pour H0 et H1
x <- seq(-4, 4, length.out = 300)
y_h0 <- dt(x, df)
y_h1 <- dt(x - t_effet, df)  # Décaler pour l'effet

# Créer un dataframe pour ggplot
data <- data.frame(x, y_h0, y_h1)

# Création du graphique
ggplot(data, aes(x)) +
  geom_line(aes(y = y_h0), color = "blue", size = 1.2, linetype = "solid") +
  geom_line(aes(y = y_h1), color = "red", size = 1.2, linetype = "solid") +
  geom_ribbon(data = data[x >= t_critique, ], aes(ymin = 0, ymax = y_h1), fill = "red", alpha = 0.2) +
  geom_vline(xintercept = c(-t_critique, t_critique, t_effet), color = "black", linetype = "dashed") +
  labs(title = "Distributions de t sous H0 et H1 avec Zones de Puissance et β",
       x = "Valeurs t", y = "Densité") +
  annotate("text", x = t_effet, y = dt(t_effet - t_effet, df), label = "", hjust = 0, color = "black") +
  theme_minimal()


library(ggplot2)

# Paramètres pour les distributions
moyenne1 <- 10
moyenne2 <- 12
sd <- 3
n <- 30
df <- n + n - 2  # Degrés de liberté pour les deux échantillons

# Calculs pour les t-scores critiques et effectifs
t_critique <- qt(0.975, df)  # Valeur t critique pour alpha = 0.05 bilatéral
t_effet <- (moyenne2 - moyenne1) / (sd * sqrt(1/n + 1/n))  # t-score de l'effet attendu

# Fonction de densité de t pour H0 et H1
x <- seq(-4, 4, length.out = 300)
y_h0 <- dt(x, df)
y_h1 <- dt(x - t_effet, df)  # Décaler pour l'effet

# Créer un dataframe pour ggplot
data <- data.frame(x, y_h0, y_h1)

# Création du graphique
ggplot(data, aes(x)) +
  geom_line(aes(y = y_h0), color = "blue", size = 1.2, linetype = "solid") +
  geom_line(aes(y = y_h1), color = "red", size = 1.2, linetype = "solid") +
  geom_ribbon(data = data[x >= t_critique, ], aes(ymin = 0, ymax = y_h1), fill = "red", alpha = 0.2) +
  geom_vline(xintercept = c(-t_critique, t_critique, t_effet), color = "black", linetype = "dashed") +
  labs(title = "Distributions de t sous H0 et H1 avec Zones de Puissance et β",
       x = "Valeurs t", y = "Densité") +
  annotate("text", x = t_effet, y = dt(t_effet - t_effet, df), label = "t_effet", hjust = 0, color = "black") +
  annotate("text", x = (t_critique + t_effet)/2, y = dt(t_effet - t_effet, df) * 1.5, label = "β (Erreur de Type II)", color = "blue") +
  annotate("text", x = t_effet + 0.3, y = max(y_h1) * 0.85, label = "Puissance (1 - β)", color = "red") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill = guide_legend(override.aes = list(colour = NULL, linetype = NULL)))
