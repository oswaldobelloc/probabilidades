## Diagramas de Venn


p1 <- draw.pairwise.venn(
  area1 = 22, area2 = 6, cross.area = 6,
  category = c("A", "B"),
  lty = rep("dashed", 1), fill = c("light blue", "green"),
  alpha = rep(0.5, 2), cat.pos = c(0, 0),
  euler.d = TRUE, sep.dist = 0.03,
  rotation.degree = 45
)
grid.newpage()

p2 <- draw.pairwise.venn(
  area1 = 15, area2 = 20, cross.area = 10,
  category = c("P", "M"),
  lty = rep("dashed", 2), fill = c("light blue", "green"),
  alpha = rep(0.5, 1), cat.pos = c(0, 0),
  euler.d = TRUE, sep.dist = 0.03,
  rotation.degree = 0
)
grid.newpage()

p3 <- draw.pairwise.venn(
  area1 = 22, area2 = 6, cross.area = 3,
  category = c("A", "B"),
  lty = rep("dashed", 1), fill = c("light blue", "green"),
  alpha = rep(0.5, 2), cat.pos = c(0, 0),
  euler.d = TRUE, sep.dist = 0.03,
  rotation.degree = 45
)
grid.newpage()

combine_plots(
  p1, p2, p3,
  labels = c("a)", "b)", "c)")
)



g4 <- euler(c("A" = 10, "B" = 20,
              "A&B" = 10),
            shape = "circle")

plot(g4) 

g5 <- euler(c("A" = 10, "B" = 10),
            input = "disjoint",
            shape = "circle")

plot(g5)


venn.diagram(list(B = letters[1:5], A = letters[1:10]),
             fill = c("red", "green"),
             alpha = c(0.5, 0.5), cex = 2, cat.fontface = 4,lty = 2, fontfamily = 3, 
             filename = "trial2.emf")

v <- venneuler(c(A = 450, B = 1800, "A&B" = 230))
plot(v)

## paquete ggVennDiagram
x1 <- list(A = letters[1:5], B = letters[1:10])
# Diagrama de Venn sin leyenda
g1 <- ggVennDiagram(x1, color = 1, lwd = 0.7, label = "none", show_intersect = FALSE) +
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
  theme(legend.position = "none")
g1

x2 <- list(A = 1:5, B = 6:10)
# Diagrama de Venn sin leyenda
g2 <- ggVennDiagram(x2, color = 1, lwd = 0.7, label = "none", show_intersect = FALSE) +
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
  theme(legend.position = "none")
g2

x3 <- list(A = 1:5, B = 2:7)
# Diagrama de Venn sin leyenda
g3 <- ggVennDiagram(x3, color = 1, lwd = 0.7, label = "none") +
  scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
  theme(legend.position = "none")
g3

## paquete euler
plot(euler(c(P = 15, M = 20, "P&M" = 10)),
     fills = list(fill = c("red", "steelblue4"), 
                  alpha = 0.5),
     edges = TRUE, lty = 1:2, 
     labels = list(font = 4), quantities = TRUE)


## Gráfica función de probabilidad ggplot2
ggplot(
  data = dp_ej1,
  aes(x = xi, y = numero_ocurrencia / 36)
) +
  geom_segment(
    aes(
      x = xi, xend = xi, y = 0,
      yend = numero_ocurrencia / 36
    ),
    color = "blue"
  ) +
  geom_point(
    size = 3, color = "red"
  ) +
  scale_x_continuous(breaks = seq(2, 12, 1)) +
  theme_classic() + 
  xlab("Suma de las caras (X)") +
  ylab("Función de probabilidad")

## Gráfica de función de distribución acumulativa de probabilidad
dp_ej1 <- dp_ej1 %>%
  mutate(
    F_xi = cumsum(numero_ocurrencia) / 36,
    xi_end = c(3:12, NA),
    F_xiend = F_xi
  )
ggplot(
  dp_ej1, aes(
    x = xi, y = F_xi, xend = xi_end, yend = F_xiend
  )
) +
  geom_point() +
  geom_point(aes(x = xi_end, y = F_xi), shape = 1) +
  geom_segment(color = "blue") +
  scale_x_continuous(breaks = seq(2, 12, 1)) +
  theme_classic() +
  xlab("Suma de las caras (X)") +
  ylab("Función de distribución acumulativa")

