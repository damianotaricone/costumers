
library(dplyr)
library(data.table)
library(ggplot2)
library(rlang)
library(GGally)
library(knitr)
library(randomForest)
library(GGally)
library(factoextra)
library(cluster)
library(lattice)
library(caret)

#-----------------introduzione  ----------------------------

cat(
"il dataset è formato da informazioni sui clienti raccolti 
tramite le tessere fedeltà. Queste informazioni includono dati come 
l'età, il genere, il reddito annuo e un punteggio di spesa basato sul loro comportamento
di acquisto. l'obiettivo è introdurre dei nuovi prodotti nel assortimento,
ma indirizzandoli a gruppi specifici di clienti per massimizzare le vendite.

L'obiettivo del clustering è quello di suddividere i clienti in gruppi
in modo che all'interno di ciascun gruppo ci siano clienti simili,
mentre tra i gruppi ci siano differenze significative.
voglio creare categorie di clienti che abbiano comportamenti 
di acquisto simili in modo da poter indirizzare i nuovi prodotti in modo più mirato
a ciascun gruppo di clienti, aumentando così le possibilità di successo delle vendite.")


# Reading the CSV file
customers <- customersx

# Displaying the first five rows
head(customers)

# Calculating the sum of missing values for each column
missing_values <- customers %>% summarise_all(~sum(is.na(.)))

# Printing the result
print(missing_values)

# Counting the duplicated rows
duplicated_rows <- sum(duplicated(customers))

# Printing the result
print(paste("Duplicated rows:", duplicated_rows))

# Printing the structure of the data frame which includes data types
str(customers)

#-----------------FUNCTION statistics ----------------------------

cat("Questa funzione calcola statistiche descrittive su una variabile.
     Se la variabile è numerica, restituisce media, deviazione standard,
     mediana e varianza. Se la variabile non è numerica, restituisce il conteggio delle categorie.")


statistics <- function(variable) {
  if (is.numeric(variable)) {
    data_frame <- data.frame(
      Variable = deparse(substitute(variable)),
      Mean = mean(variable, na.rm = TRUE),
      Standard_Deviation = sd(variable, na.rm = TRUE),
      Median = median(variable, na.rm = TRUE),
      Variance = var(variable, na.rm = TRUE)
    )
    rownames(data_frame) <- data_frame$Variable
    data_frame$Variable <- NULL
    return(data_frame)
  } else {
    # Handle non-numeric data
    return(as.data.frame(table(variable)))
  }
}

#-----------------FUNCTION graph_histo ----------------------------

cat("Questa funzione in R crea un grafico a dispersione (istogramma) 
    se la variabile di input è numerica, altrimenti crea un grafico a barre
    che rappresenta il conteggio delle categorie. Utilizza il pacchetto ggplot2
    per la creazione dei grafici.")


graph_histo <- function(x) {
  # Convert string to symbol for ggplot
  x_sym <- sym(names(data.frame(x))[1])
  
  if (is.numeric(x)) {
    p <- ggplot(data.frame(x), aes(!!x_sym)) +
      geom_histogram(aes(y = after_stat(density)), 
                     binwidth = (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / 10,
                     colour = "grey", fill = rainbow(3)[1], alpha = 0.4) +
      geom_density(colour = rainbow(3)[2], linewidth = 1.5, alpha = 0.8) +
      labs(title = names(data.frame(x))[1], y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            axis.title.y = element_text(size = 16)) +
      theme(panel.grid = element_blank(), 
            panel.border = element_blank(),
            axis.line = element_line())
  } else {
    p <- ggplot(data.frame(x), aes(!!x_sym)) +
      geom_bar(fill = rainbow(3)[1]) +
      labs(title = names(data.frame(x))[1], y = "Counts") +
      theme_minimal() +
      theme(plot.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            axis.title.y = element_text(size = 15)) +
      theme(panel.grid = element_blank(), 
            panel.border = element_blank(),
            axis.line = element_line())
  }
  
  print(p)
}


#----------------- SPENDING ----------------------------

# Extracting the "Spending.Score..1.100." column
spending <- customers[["Spending.Score..1.100."]]

# Apply the 'statistics' function
stats_result_spending <- statistics(spending)

# Viewing the result
print(stats_result_spending)

graph_histo(spending)

#----------------- INCOME ----------------------------

# Extracting the "Spending.Score..1.100." column
income <- customers[["Annual.Income..k.."]]

# Apply the 'statistics' function
stats_result_income <- statistics(income)

# Viewing the result
print(stats_result_income)

graph_histo(income)

#----------------- AGE ----------------------------

# Extracting the "Spending.Score..1.100." column
age <- customers[["Age"]]

# Apply the 'statistics' function
stats_result_age <- statistics(age)

# Viewing the result
print(stats_result_age)

graph_histo(age)

#----------------- GENDER ----------------------------


gender <- customers[["Gender"]]

# Apply the 'statistics' function
stats_result <- statistics(gender)

# Viewing the result
print(stats_result)

graph_histo(spending)

#----------------- CORRELAZIONE ----------------------------

cat("analizzeremo la correlazione tra i parametri numerici..
Vogliamo vedere se esiste una differenza tra i generi. Quindi, 
imposteremo")

ggpairs_plot <- ggpairs(
  data = customers,
  columns = c("Age", "Annual.Income..k..", "Spending.Score..1.100."),
  mapping = ggplot2::aes(color = Gender, fill = Gender),
  upper = list(continuous = wrap("points", size = 2, alpha = 0.5)),
  lower = list(continuous = wrap("points", size = 2, alpha = 0.5)),
  diag = list(continuous = wrap("densityDiag", size = 2, alpha = 0.5)),
  title = "Pairs Plot of Customers Data"
) + 
  scale_color_brewer(type = 'qual', palette = 'Pastel1') +
  theme(legend.position = "bottom")

print(ggpairs_plot)


#----------------- Principal Component Analysis (PCA) ----------------------------


# Assuming your data frame is named customers and it has a column named Gender
customers$Male <- as.integer(customers$Gender == "Male")
customers$Female <- as.integer(customers$Gender == "Female")

# 1. Converting 'Gender' to binary variables
customers$Male <- ifelse(customers$Gender == "Male", 0, 1)
customers$Female <- ifelse(customers$Gender == "Female", 0, 1)


# Select all columns from the third column to the end
X <- customers[,3 :ncol(customers)]

# View the first few rows of X
head(X)

# Esegui l'analisi delle componenti principali (PCA) su un dataset 'X' con 2 componenti principali
pca_result <- prcomp(X, rank = 2)

# Le componenti principali possono essere ottenute da pca_result$x
pca_components <- pca_result$x

print(pca_components)

# Ottieni le varianze spiegate dalle componenti principali
varianze_spiegate <- pca_result$sdev^2

print(varianze_spiegate)


# Trasforma i campioni utilizzando le componenti principali calcolate
pca_2d <- predict(pca_result, newdata = X)

# Create a data frame for pca_2d
pca_df <- data.frame(PC1 = pca_2d[, 1], PC2 = pca_2d[, 2])

# Create a biplot
biplot <- ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point() +  # Scatter plot of scores on PC1 and PC2
  xlab("Principal Component 1") +
  ylab("Principal Component 2")

# Add arrows for the principal components
biplot <- biplot + geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_segment(aes(xend = 0, yend = 0, x = 0, y = 0), data = data.frame(), color = "red") +
  geom_segment(aes(xend = pca_components[1, 1], yend = pca_components[2, 1], x = 0, y = 0), 
               arrow = arrow(type = "closed", length = unit(0.15, "inches")), color = "red") +
  geom_segment(aes(xend = pca_components[1, 2], yend = pca_components[2, 2], x = 0, y = 0), 
               arrow = arrow(type = "closed", length = unit(0.15, "inches")), color = "red")

# Show the biplot
print(biplot)

# Install ggfortify if you haven't already
install.packages("ggfortify")
# Load the library
library(ggfortify)

# Generate the biplot with loading vectors
autoplot(pca_result, data = X, loadings = TRUE, loadings.label = TRUE, loadings.colour = 'blue', label.size = 3)


#----------------- K-means clustering ----------------------------

# Assuming X is your data set
# Run K-means and compute the within-cluster sum of squares (wcss) for each number of clusters
wcss <- numeric(10)
for (i in 1:10) {
  set.seed(0)  # Set a random seed for reproducibility
  km <- kmeans(X, centers = i, nstart = 10)
  wcss[i] <- km$tot.withinss
}

# Create the Elbow plot
elbow_plot <- data.frame(NumClusters = 1:10, WCSS = wcss)
ggplot(elbow_plot, aes(x = NumClusters, y = WCSS)) +
  geom_line(color = "#c51b7d") +
  theme_minimal() +
  theme(axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(title = 'Elbow Method',
       x = 'Number of clusters',
       y = 'WCSS')

# Print the plot
print(elbow_plot)

# Set a seed for reproducibility
set.seed(0)

# Apply the kmeans algorithm
kmeans_result <- kmeans(X, centers = 5, nstart = 10, iter.max = 10)

# kmeans_result$cluster contains the cluster assignment for each observation
y_means <- kmeans_result$cluster

# If you want to append the cluster assignments to your original data
X$Cluster <- y_means

# Now you can inspect the first few rows of your data with the cluster assignments
head(X)

pca_df <- as.data.frame(pca_2d)
colnames(pca_df) <- c("Component1", "Component2")

# Add the cluster assignments to the data frame
pca_df$Cluster <- as.factor(y_means)

# Create the scatter plot
plot <- ggplot(pca_df, aes(x = Component1, y = Component2, color = Cluster)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_d() +
  theme_minimal() +
  theme(axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "right") +
  labs(x = "Component 1", y = "Component 2", title = 'divisione in 5 cluster') +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

# Print the plot
print(plot)

cat("Le caratteristiche più importanti sembrano essere il Reddito Annuale e il Punteggio di Spesa. 
    Abbiamo persone il cui reddito è basso ma spendono nella stessa fascia
    - segmento 0. Persone il cui guadagno è alto e spendono molto
    - segmento 1. Clienti il cui reddito è nella media ma spendono allo stesso livello 
    - segmento 2. Poi abbiamo clienti il cui reddito è molto alto ma hanno le spese più alte
    - segmento 4. E infine, persone il cui guadagno è poco ma spendono molto 
    - segmento 5.")

# Esegui l'analisi di k-means
kmeans_result <- kmeans(X, centers = 5) 

# Estrai i centroidi dei cluster
centroids_df <- data.frame(
  Age = kmeans_result$centers[, "Age"],
  `Annual Income` = kmeans_result$centers[, "Annual.Income..k.."],  # Utilizziamo backticks per il nome con spazio
  Spending = kmeans_result$centers[, "Spending.Score..1.100."],
  Male = kmeans_result$centers[, "Male"],
  Female = kmeans_result$centers[, "Female"]
)

# Assegna i nomi di riga (Cluster1, Cluster2, ecc.)
row.names(centroids_df) <- paste0("Cluster", 1:5)

# Stampa il dataframe dei centroidi
print(centroids_df)


#----------------- K-means prediction ----------------------------


new_sample <- data.frame(
  Age = 19,
  `Annual Income` = 15,
  Spending = 39,
  Male = 0,
  Female = 1
)  


# Estrai i centroidi dei cluster dal modello k-means
centroids <- kmeans_result$centers

# Calcola le distanze euclidee tra il nuovo campione e i centroidi dei cluster
distances <- apply(centroids, 1, function(center) sqrt(sum((new_sample - center)^2)))

# Trova il cluster con la distanza minima
predicted_cluster <- which.min(distances)

# Stampa il cluster previsto per il nuovo campione
cat("Il nuovo campione appartiene al cluster", predicted_cluster, "\n")


#----------------- Random Forest ----------------------------

# Caricamento del pacchetto
library(randomForest)

customers$cluster <- as.factor(kmeans_result$cluster)

# Selezionare solo le colonne specificate più la colonna target
selected_columns <- c("Age", "Annual.Income..k..", "Spending.Score..1.100.", "Male", "Female", "cluster")
customers_selected <- customers[, selected_columns]

# Dividere i dati in set di training e test
set.seed(123) # Per riproducibilità
training_indices <- sample(1:nrow(customers_selected), 0.7 * nrow(customers_selected))
train_data <- customers_selected[training_indices, ]
test_data <- customers_selected[-training_indices, ]

# Addestrare il modello Random Forest
# Assicurati che il nome della colonna target sia corretto (ad esempio, 'cluster')
rf_model <- randomForest(cluster ~ ., data=train_data, ntree=100)


# Previsione sul set di test
predictions <- predict(rf_model, test_data)


# Verifica la lunghezza delle predizioni
print(length(predictions))


# Se il nome della colonna target è 'cluster' e non 'Cluster', correggi la riga seguente
table(predictions, test_data$cluster)

# La tua matrice di confusione
confusionMatrix <- matrix(c(4, 0, 0, 0, 0, 2, 27, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 15),
                          nrow = 5, ncol = 5, byrow = TRUE)

# Calcolare precisione, richiamo e F1 per ogni classe
precision <- diag(confusionMatrix) / rowSums(confusionMatrix)
recall <- diag(confusionMatrix) / colSums(confusionMatrix)
f1_scores <- 2 * (precision * recall) / (precision + recall)

# Stampa i punteggi F1
f1_scores

# Creazione di un nuovo esempio
new_example <- data.frame(
  Age = 19, 
  `Annual.Income..k..` = 15, 
  `Spending.Score..1.100.` =39, 
  Male = 0, 
  Female = 1
)


# Predizione con il modello Random Forest
predicted_cluster_rf <- predict(rf_model, new_example)

# Stampa il cluster previsto
print(predicted_cluster_rf)

