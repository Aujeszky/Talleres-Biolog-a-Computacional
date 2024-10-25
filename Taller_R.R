library(gamlss)
library(tidyverse)
library(ggpubr)
library(skimr)
library(ggplot2)

data("rent")
datos <- rent
head(rent)
summary(rent)
datos <- datos %>% select(R, Fl, A, H, loc)
# Se renombran las variables para que sean más explicativas
colnames(datos) <- c("precio", "metros", "anyo", "calefaccion", "situacion")
skim(datos)

head(datos)
summary(datos)
mean(datos$precio)
mean(datos$metros)
mean(datos$anyo)

sd(datos$precio)
sd(datos$metros)
sd(datos$anyo)


p1 <- ggplot(data = datos, aes(x = metros, y = precio)) + 
  geom_point(alpha = 0.4) +
  labs(ttile = "Precio vs metros") +
  theme_bw()
p1

p2 <- ggplot(data = datos, aes(x = anyo, y = precio)) + 
  geom_point(alpha = 0.4) +
  labs(ttile = "Precio vs año") +
  theme_bw()
p2


modelo1 = lm(precio ~ metros, datos)
summary(modelo1)
datos$prediccion1 = predict(modelo1)
ggplot(datos, aes(x = metros, y = precio)) +
  geom_point(color = "black") +  # Gráfico de dispersión
  geom_smooth(mapping = aes(metros, prediccion1), se=TRUE,
              col="#9ecae1", alpha=0.4)+
  labs(title = "Modelo de Regresión de Renta en función del Área",
       x = "Área (m2)",
       y = "Precio") +
  theme_minimal()


modelo2 = lm(precio ~ anyo, datos)
summary(modelo2)
datos$prediccion2 = predict(modelo2)
ggplot(datos, aes(x = metros, y = precio)) +
  geom_point(color = "black") +  # Gráfico de dispersión
  geom_smooth(mapping = aes(metros, prediccion2), se=TRUE,
              col="#9ecae1", alpha=0.4)+
  labs(title = "Modelo de Regresión de Renta en función del Área",
       x = "Área (m2)",
       y = "Años") +
  theme_minimal()

modelo3 = lm(precio ~ metros+anyo, datos)
summary(modelo3)
datos$prediccion3 = predict(modelo3)
ggplot(datos, aes(x = metros, y = precio)) +
  geom_point(color = "black") +  # Gráfico de dispersión
  geom_smooth(mapping = aes(metros, prediccion3), se=TRUE,
              col="#9ecae1", alpha=0.4)+
  labs(title = "Modelo de Regresión de Renta en función del Área",
       x = "Área (m2)",
       y = "Años") +
  theme_minimal()

p3 <- ggplot(data = datos, aes(x = calefaccion, y = precio)) + 
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(ttile = "Precio vs calefacción") +
  theme_bw()
p3

p4 <- ggplot(data = datos, aes(x = situacion, y = precio)) + 
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(ttile = "Precio vs situación") +
  theme_bw()
p4

ggpubr::ggarrange(
  plotlist = list(p1, p2, p3, p4)) %>%
  ggpubr::annotate_figure(
    top = text_grob("Relación entre el precio y el resto de variables",
                    color = "Black",
                    face  = "bold",
                    size  = 14,
                    x     = 0.3)
  )



ggplot(data = datos, aes(x = precio)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_rug(alpha = 0.2) +
  labs(title = "Distribución del precio de los pisos") +
  theme_bw()



modelo_lm <- gamlss(
  formula = precio ~ metros + anyo + calefaccion + situacion,
  family  = NO,
  data    = datos,
  trace   = FALSE
)

summary(modelo_lm)

paste("El valor estimado de la varianza es:", exp(5.73165))

plot(modelo_lm)

# Worm plot de los residuos
wp(modelo_lm, ylim.all = 1)





GA()

# Mostrar las funciones link disponibles para la distribución gamma.
show.link(family = GA)

modelo_glm <- gamlss(
  formula = precio ~ metros + anyo + calefaccion + situacion,
  family  = GA,
  data    = datos,
  trace   = FALSE
)

summary(modelo_glm)

plot(modelo_glm)

# Worm plot de los residuos
wp(modelo_glm, ylim.all = 0.5)

# Por defecto, GAIC emplea una penalización k = 2.5
GAIC(modelo_lm, modelo_glm)




# Se emplea P-splines para los predictores continuos
# Distribución gamma para la variable respuesta
modelo_gam <- gamlss(
  formula = precio ~ pb(metros) + pb(anyo) + calefaccion + situacion,
  family  = GA,
  data    = datos,
  trace   = FALSE
)

summary(modelo_gam)

# Esta función puede tardar si hay muchos predictores no lineales o muchos datos.
drop1(modelo_gam, parallel = "multicore", ncpus = 4)

term.plot(modelo_gam, pages = 1, ask = FALSE, rug = TRUE)

plot(modelo_gam)

# Worm plot de los residuos
wp(modelo_gam, ylim.all = 0.5)


GAIC(modelo_lm, modelo_glm, modelo_gam)



# Modelo GAMLSS para distribución gamma con parámetros de media y escala
# Se emplea P-splines para los predictores continuos
modelo_gamlss <- gamlss(
  formula = precio ~ pb(metros)+pb(anyo)+calefaccion+situacion,
  sigma.formula = ~ pb(metros)+pb(anyo)+calefaccion+situacion,
  family  = GA,
  data    = datos,
  trace   = FALSE
)

summary(modelo_gamlss)

term.plot(modelo_gamlss, parameter = "sigma", pages = 1, ask = FALSE, rug = TRUE)

drop1(modelo_gamlss, parameter = "sigma", parallel = "multicore", ncpus = 4)

# Worm plot de los residuos
wp(modelo_gamlss, ylim.all = 0.5)

modelo_gamlss1 <- gamlss(
  formula = precio ~ pb(metros)+pb(anyo)+calefaccion+situacion,
  sigma.formula = ~ pb(metros)+pb(anyo)+situacion,
  family  = GA,
  data    = datos,
  trace   = FALSE
)

summary(modelo_gamlss1)

term.plot(modelo_gamlss1, parameter = "sigma", pages = 1, ask = FALSE, rug = TRUE)

drop1(modelo_gamlss, parameter = "sigma", parallel = "multicore", ncpus = 4)

# Worm plot de los residuos
wp(modelo_gamlss1, ylim.all = 0.5)



GAIC(
  modelo_lm,
  modelo_glm,
  modelo_gam,
  modelo_gamlss
)
