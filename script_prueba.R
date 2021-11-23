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
  area1 = 22, area2 = 6, cross.area = 0,
  category = c("A", "B"),
  lty = rep("dashed", 1), fill = c("light blue", "green"),
  alpha = rep(0.5, 1), cat.pos = c(0, 0),
  euler.d = TRUE, sep.dist = 0.03,
  rotation.degree = 45
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







